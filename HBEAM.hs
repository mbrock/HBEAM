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
type Operation = (Opcode, [Argument])

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

data ArgumentTag = TagA | TagF | TagH | TagI | TagU
                 | TagX | TagY | TagZ
                 | TagFR | TagAtom | TagFloat | TagLiteral
     
data Argument = IArg Integer
  deriving Show


main :: IO ()
main =
  do binary <- B.getContents
     print (parseBEAMFile (readBEAMFile binary))
     
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
  do atoms   <- parseAtomChunk <$> (lookup "Atom" chunks)
     imports <- parseImportChunk atoms <$> (lookup "ImpT" chunks)
     exports <- parseExportChunk atoms <$> (lookup "ExpT" chunks)
     fundefs <- parseCodeChunk atoms <$> (lookup "Code" chunks)
     return $ BEAMFile { beamFileAtoms   = atoms 
                       , beamFileFunDefs = fundefs 
                       , beamFileImports = imports 
                       , beamFileExports = exports }
     
parseImportChunk :: [Atom] -> ChunkData -> [MFA]
parseImportChunk atoms =
  runGet (readMany (liftM3 MFA getAtom getAtom getInt32))
    where getAtom = readAtom atoms
     
parseExportChunk :: [Atom] -> ChunkData -> [Export]
parseExportChunk atoms =
  runGet (readMany (liftM3 Export (readAtom atoms) getInt32 getInt32))
  
parseAtomChunk :: ChunkData -> [Atom]
parseAtomChunk =
  runGet (readMany $ getWord8 >>= getString >>= return . Atom)

parseCodeChunk :: [Atom] -> ChunkData -> [FunDef]
parseCodeChunk atoms =
  runGet $ do verifyHeader
              parseOperations atoms <$> (readOperation `untilM` isEmpty)
    where verifyHeader =
            do getInt32 `expecting` ("code info length", 16)
               getInt32 `expecting` ("instruction set", 0)
               maxOpcode' <- getInt32
               unless (maxOpcode' <= maxOpcode) $
                 fail ("max opcode too big: " ++ show maxOpcode')
               skip 8 -- label & function counts

parseOperations :: [Atom] -> [Operation] -> [FunDef]
parseOperations atoms (_ : ("func_info", [IArg m, IArg f, IArg a])
                       : ("label", [IArg entry]) : xs) =
  let (code, rest) = splitToNextFunctionLabel [] xs
  in FunDef (atomIndex atoms f) a entry code : parseOperations atoms rest
parseOperations _ [] = []

splitToNextFunctionLabel :: [Operation] -> [Operation] ->
                            ([Operation], [Operation])
splitToNextFunctionLabel acc xs@(_ : ("func_info", _) : _) =
  (reverse acc, xs)
splitToNextFunctionLabel acc [("int_code_end", [])] =
  (reverse acc, [])
splitToNextFunctionLabel acc (x:xs) =
  splitToNextFunctionLabel (x:acc) xs
     
readAtom :: [Atom] -> Get Atom
readAtom atoms = atomIndex atoms <$> getInt32

readOperation :: Get Operation
readOperation =
  do (opcode, argCount) <- (opcodeInfo . fromIntegral) <$> getWord8
     args <- replicateM argCount readArgument
     return (opcode, args)
     
readArgument :: Get Argument
readArgument =
  do taggedByte <- getInt8
     case parseTag taggedByte of
       TagZ -> readZArgument taggedByte
       TagA -> readAArgument taggedByte
       _    -> readIArgument taggedByte
     
parseTag :: Integer -> ArgumentTag
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
    
readAArgument :: Integer -> Get Argument
readAArgument tag = 
  -- FIXME: wrong
  readIArgument tag
  
readZArgument :: Integer -> Get Argument
readZArgument tag =
  fail "can't handle floats or lists yet"
  
readIArgument :: Integer -> Get Argument
readIArgument tag | tag .&. 0x8 == 0 =
  return $ IArg (tag `shiftR` 4)
readIArgument tag | tag .&. 0x10 == 0 =
  do b <- getInt8
     return $ IArg (((tag .&. 0xe0) `shiftL` 3) .|. b)
readIArgument tag =
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
