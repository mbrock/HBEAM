module Main where

import qualified Data.ByteString.Lazy as B

import Data.Binary
import Data.Binary.Get

import Data.Bits

import qualified Data.Text.Lazy as T

import Data.Text.Lazy.Encoding

import Control.Monad
import Control.Monad.Loops
import Control.Applicative

import Opcodes


type ChunkData = B.ByteString
type Chunk     = (String, ChunkData)

type Opcode    = String
type Operation = (Opcode, [Operand])

newtype Atom   = Atom String deriving Show
                      
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
     
data Operand = IOperand Integer
             | AOperand Atom
  deriving Show


main :: IO ()
main = B.getContents >>= print . parseBEAMFile . readBEAMFile
     
readBEAMFile :: ChunkData -> [Chunk]
readBEAMFile binary = runGet (getHeader >> getChunks) binary
  where 
    getHeader = skip 12
    getChunks = getChunk `untilM` isEmpty
    getChunk  =
      do name    <- getString 4
         content <- getInt32 >>= getLazyByteString
         align 4
         return (name, content)

parseBEAMFile :: [Chunk] -> Maybe BEAMFile
parseBEAMFile chunks =
  do atoms   <- parseAtomChunk         <$> (lookup "Atom" chunks)
     imports <- parseImportChunk atoms <$> (lookup "ImpT" chunks)
     exports <- parseExportChunk atoms <$> (lookup "ExpT" chunks)
     fundefs <- parseCodeChunk   atoms <$> (lookup "Code" chunks)
     return $ BEAMFile { beamFileAtoms   = atoms 
                       , beamFileFunDefs = fundefs 
                       , beamFileImports = imports 
                       , beamFileExports = exports }
     
parseImportChunk :: [Atom] -> ChunkData -> [MFA]
parseImportChunk atoms =
  readListChunk $ MFA <$> getAtom <*> getAtom <*> getInt32
    where getAtom = readAtom atoms
     
parseExportChunk :: [Atom] -> ChunkData -> [Export]
parseExportChunk atoms =
  readListChunk $ Export <$> (readAtom atoms) <*> getInt32 <*> getInt32
  
parseAtomChunk :: ChunkData -> [Atom]
parseAtomChunk =
  readListChunk $ getWord8 >>= getString >>= return . Atom
  
readListChunk :: Get a -> ChunkData -> [a]
readListChunk m = runGet (readMany m)

parseCodeChunk :: [Atom] -> ChunkData -> [FunDef]
parseCodeChunk atoms =
  runGet $ do verifyHeader
              operations <- (readOperation atoms) `untilM` isEmpty
              return $ parseOperations atoms operations
    where verifyHeader =
            do getInt32 `expecting` ("code info length", 16)
               getInt32 `expecting` ("instruction set", 0)
               maxOpcode' <- getInt32
               unless (maxOpcode' <= maxOpcode) $
                 fail ("max opcode too big: " ++ show maxOpcode')
               skip 8 -- label & function counts

parseOperations :: [Atom] -> [Operation] -> [FunDef]
parseOperations atoms (_ : ("func_info", [AOperand m, AOperand f, IOperand a])
                         : ("label", [IOperand entry]) : xs) =
  let (code, rest) = splitToNextFunctionLabel [] xs
  in FunDef f a entry code : parseOperations atoms rest
parseOperations _ [] = []

splitToNextFunctionLabel :: [Operation] -> [Operation] ->
                            ([Operation], [Operation])
splitToNextFunctionLabel acc ops =
  case ops of
    (_ : ("func_info", _) : _) -> (reverse acc, ops)
    [("int_code_end", [])]     -> (reverse acc, [])
    (x:xs)                     -> splitToNextFunctionLabel (x:acc) xs
    []                         -> error "code chunk ended prematurely"
     
readAtom :: [Atom] -> Get Atom
readAtom atoms = atomIndex atoms <$> getInt32

readOperation :: [Atom] -> Get Operation
readOperation atoms =
  do (opcode, argCount) <- (opcodeInfo . fromIntegral) <$> getWord8
     args <- replicateM argCount (readOperand atoms)
     return (opcode, args)
     
readOperand :: [Atom] -> Get Operand
readOperand atoms =
  do taggedByte <- getInt8
     case parseTag taggedByte of
       TagZ -> readZOperand taggedByte
       TagA -> readAOperand atoms taggedByte
       _    -> readIOperand taggedByte
     
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
  do IOperand i <- readIOperand tag
     return $ AOperand (case i of
                           0 -> Atom "nil"
                           _ -> atomIndex atoms i)
  
readZOperand :: Integer -> Get Operand
readZOperand tag =
  fail "can't handle floats or lists yet"
  
readIOperand :: Integer -> Get Operand
readIOperand tag | tag .&. 0x8 == 0 =
  return $ IOperand (tag `shiftR` 4)
readIOperand tag | tag .&. 0x10 == 0 =
  do b <- getInt8
     return $ IOperand (((tag .&. 0xe0) `shiftL` 3) .|. b)
readIOperand tag =
  fail "integer too big for me"
  

-- Helper functions for BEAM data.

atomIndex :: Integral a => [Atom] -> a -> Atom
atomIndex atoms i = atoms !! (fromIntegral i - 1)


-- Helper functions for reading binary stuff.

getString :: Integral a => a -> Get String
getString n =
  getLazyByteString (fromIntegral n) >>= return . unpackByteString

unpackByteString :: B.ByteString -> String
unpackByteString = T.unpack . decodeASCII

getInt32 :: Integral a => Get a
getInt32 = fromIntegral <$> getWord32be

getInt8 :: Integral a => Get a
getInt8 = fromIntegral <$> getWord8

readMany :: Get a -> Get [a]
readMany m = getInt32 >>= flip replicateM m

align :: Int -> Get ()
align n =
  do m <- fromIntegral <$> bytesRead
     skip ((((m + n - 1) `div` n) * n) - m)

expecting :: (Eq a, Show a) => Get a -> (String, a) -> Get ()
expecting m (name, x) =
  do y <- m
     unless (y == x) (fail ("Wrong " ++ name ++ ": expected " ++
                            show x ++ ", not " ++ show y))
