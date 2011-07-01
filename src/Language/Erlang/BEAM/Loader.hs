module Language.Erlang.BEAM.Loader where

import Language.Erlang.BEAM.Opcodes

import qualified Data.ByteString.Lazy as B
import qualified Codec.Compression.Zlib as Zlib

import Data.Binary ()
import Data.Binary.Get

import Data.Char
import Data.Bits
import Data.Maybe

import qualified Data.Text.Lazy as T

import Data.Text.Lazy.Encoding

import Control.Monad
import Control.Monad.Loops
import Control.Applicative

type ChunkData = B.ByteString
type Chunk     = (String, ChunkData)

type Opcode    = String
type Operation = (Opcode, [Operand])

data External  = ExtInteger Integer
               | ExtTuple   [External]
               | ExtAtom    String
               | ExtString  String
               | ExtList    [External]
               deriving Show

newtype Atom   = Atom String deriving (Show, Ord, Eq)
                      
type Arity     = Integer
type Label     = Integer
                                      
data MFA       = MFA    Atom Atom  Arity deriving Show
data Export    = Export Atom Arity Label deriving Show

data FunDef    = FunDef Atom Arity Label [Operation]
               deriving Show
      
data BEAMFile = BEAMFile { beamFileAtoms   :: [Atom] 
                         , beamFileFunDefs :: [FunDef] 
                         , beamFileImports :: [MFA] 
                         , beamFileExports :: [Export] }
              deriving Show

data OperandTag = TagA | TagF | TagH | TagI | TagU
                | TagX | TagY | TagZ
                | TagFR | TagAtom | TagFloat | TagLiteral
                deriving Show
     
data Operand = IOperand Integer
             | UOperand Integer
             | XOperand Integer
             | YOperand Integer
             | FOperand Integer
             | AOperand Atom
             | LOperand External
             deriving Show
     
readBEAMFile :: ChunkData -> [Chunk]
readBEAMFile binary = runGet (getHeader >> getChunks) binary
  where 
    getHeader = skip 12
    getChunks = getChunk `untilM` isEmpty
    getChunk  =
      do name    <- getString (4 :: Int)
         content <- getInt32 >>= getLazyByteString
         align 4
         return (name, content)

parseBEAMFile :: [Chunk] -> Maybe BEAMFile
parseBEAMFile chunks =
  do atoms    <- parseAtomChunk                   <$> lookup "Atom" chunks
     imports  <- parseImportChunk  atoms          <$> lookup "ImpT" chunks
     exports  <- parseExportChunk  atoms          <$> lookup "ExpT" chunks
     let literals = maybe [] parseLiteralChunk (lookup "LitT" chunks)
     fundefs  <- parseCodeChunk    atoms literals <$> lookup "Code" chunks
     return BEAMFile { beamFileAtoms   = atoms 
                     , beamFileFunDefs = fundefs 
                     , beamFileImports = imports 
                     , beamFileExports = exports }
     
parseImportChunk :: [Atom] -> ChunkData -> [MFA]
parseImportChunk atoms =
  readListChunk $ MFA <$> getAtom <*> getAtom <*> getInt32
    where getAtom = readAtom atoms
     
parseExportChunk :: [Atom] -> ChunkData -> [Export]
parseExportChunk atoms =
  readListChunk $ Export <$> readAtom atoms <*> getInt32 <*> getInt32
  
parseAtomChunk :: ChunkData -> [Atom]
parseAtomChunk =
  readListChunk $ Atom <$> (getWord8 >>= getString)
  
readListChunk :: Get a -> ChunkData -> [a]
readListChunk m = runGet (readMany32 m)

parseLiteralChunk :: ChunkData -> [External]
parseLiteralChunk x =
  readListChunk (parseLiteral <$> (getInt32 >>= getLazyByteString)) y
    where y = Zlib.decompress $ B.drop 4 x
          
parseLiteral :: ChunkData -> External
parseLiteral = runGet (verify >> readExternal)
  where verify = getInt8 `expecting` ("external version magic", 131 :: Int)
        
readExternal :: Get External
readExternal =
  do tag <- getLatin1Char
     case tag of
       'a' -> ExtInteger <$> getInt8
       'h' -> ExtTuple   <$> readMany8 readExternal
       'd' -> ExtAtom    <$> (getWord16be >>= getString)
       'k' -> ExtString  <$> (getWord16be >>= getString)
       'l' -> ExtList    <$> readMany32 readExternal
       'j' -> ExtList    <$> return []
       _   -> fail $ "readExternal: can't do tag " ++ show tag

parseCodeChunk :: [Atom] -> [External] -> ChunkData -> [FunDef]
parseCodeChunk atoms literals =
  runGet $ do verifyHeader
              operations <- readOperation literals atoms `untilM` isEmpty
              return $ parseOperations atoms operations
    where verifyHeader =
            do getInt32 `expecting` ("code info length", 16 :: Int)
               getInt32 `expecting` ("instruction set", 0 :: Int)
               maxOpcode' <- getInt32
               unless (maxOpcode' <= maxOpcode) $
                 fail ("max opcode too big: " ++ show maxOpcode')
               skip 8 -- label & function counts

parseOperations :: [Atom] -> [Operation] -> [FunDef]
parseOperations atoms (_ : ("func_info", [AOperand _, AOperand f, UOperand a])
                         : xs@(("label", [UOperand entry]) : _)) =
  let (code, rest) = splitToNextFunctionLabel [] xs
  in FunDef f a entry code : parseOperations atoms rest
parseOperations _ [] = []
parseOperations _ xs =
  error $ "parseOperations: misformed function " ++ show xs

splitToNextFunctionLabel :: [Operation] -> [Operation] ->
                            ([Operation], [Operation])
splitToNextFunctionLabel acc ops =
  case ops of
    (_ : ("func_info", _) : _) -> (reverse acc, ops)
    [("int_code_end", [])]     -> (reverse acc, [])
    (x:xs)                     -> splitToNextFunctionLabel (x:acc) xs
    []                         -> error "code chunk ended prematurely"
     
readAtom :: [Atom] -> Get Atom
readAtom atoms = atomIndex atoms <$> (getInt32 :: Get Int)

readOperation :: [External] -> [Atom] -> Get Operation
readOperation literals atoms =
  do (opcode, argCount) <- (opcodeInfo . fromIntegral) <$> getWord8
     args <- replicateM argCount (readOperand literals atoms)
     return (opcode, args)
     
readOperand :: [External] -> [Atom] -> Get Operand
readOperand literals atoms =
  do taggedByte <- getInt8
     case parseTag taggedByte of
       TagZ -> readZOperand literals taggedByte
       TagA -> readAOperand atoms taggedByte
       _    -> readIntegralOperand taggedByte
     
parseTag :: Integer -> OperandTag
parseTag x =
  case x .&. 7 of
    0 -> TagU
    1 -> TagI
    2 -> TagA
    3 -> TagX
    4 -> TagY
    5 -> TagF
    6 -> TagH
    7 -> TagZ
    _ -> error $ "weird tag: " ++ show x
    
readAOperand :: [Atom] -> Integer -> Get Operand
readAOperand atoms tag =
  do i <- readInteger tag
     return $ AOperand (case i of
                           0 -> Atom "nil"
                           _ -> atomIndex atoms i)
  
readZOperand :: [External] -> Integer -> Get Operand
readZOperand literals tag =
  case tag `shiftR` 4 of
    4 -> do UOperand i <- getInt8 >>= readIntegralOperand
            return $ LOperand (literalIndex literals i)
    _ -> fail $ "readZOperand: ? " ++ show (tag `shiftR` 4)
  
readIntegralOperand :: Integer -> Get Operand
readIntegralOperand tag =
  do i <- readInteger tag
     return $ case parseTag tag of
                TagU -> UOperand i
                TagI -> IOperand i
                TagX -> XOperand i
                TagY -> YOperand i
                TagF -> FOperand i
                x    -> error ("readIntegralOperand: ? " ++ show x)
      
readInteger :: Integer -> Get Integer
readInteger tag | tag .&. 0x8 == 0 =
  return (tag `shiftR` 4)
readInteger tag | tag .&. 0x10 == 0 =
  do b <- getInt8
     return (((tag .&. 0xe0) `shiftL` 3) .|. b)
readInteger _ =
  fail "integer too big for me"
  

-- Helper functions for BEAM data.

atomIndex :: Integral a => [Atom] -> a -> Atom
atomIndex xs i = xs !! (fromIntegral i - 1)

literalIndex :: Integral a => [External] -> a -> External
literalIndex xs i = xs !! fromIntegral i


-- Helper functions for reading binary stuff.

getString :: Integral a => a -> Get String
getString n = unpackByteString <$> getLazyByteString (fromIntegral n)

unpackByteString :: B.ByteString -> String
unpackByteString = T.unpack . decodeASCII

getInt32 :: Integral a => Get a
getInt32 = fromIntegral <$> getWord32be

getInt8 :: Integral a => Get a
getInt8 = fromIntegral <$> getWord8

getLatin1Char :: Get Char
getLatin1Char = chr <$> getInt8

readMany32 :: Get a -> Get [a]
readMany32 m = getInt32 >>= flip replicateM m

readMany8 :: Get a -> Get [a]
readMany8 m = getInt8 >>= flip replicateM m

align :: Int -> Get ()
align n =
  do m <- fromIntegral <$> bytesRead
     skip ((((m + n - 1) `div` n) * n) - m)

expecting :: (Eq a, Show a) => Get a -> (String, a) -> Get ()
expecting m (name, x) =
  do y <- m
     unless (y == x) (fail ("Wrong " ++ name ++ ": expected " ++
                            show x ++ ", not " ++ show y))
