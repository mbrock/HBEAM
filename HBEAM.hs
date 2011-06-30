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
      
data BEAMFile = BEAMFile { beamFileAtoms   :: [Atom] 
                         , beamFileCode    :: [Operation] 
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
  do atoms   <- fmap parseAtomChunk (lookup "Atom" chunks)
     imports <- fmap (parseImportChunk atoms) (lookup "ImpT" chunks)
     exports <- fmap (parseExportChunk atoms) (lookup "ExpT" chunks)
     codes   <- fmap parseCodeChunk (lookup "Code" chunks)
     return $ BEAMFile { beamFileAtoms   = atoms 
                       , beamFileCode    = codes 
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

parseCodeChunk :: ChunkData -> [Operation]
parseCodeChunk =
  runGet $ do getInt32 `expecting` ("code info length", 16)
              getInt32 `expecting` ("instruction set", 0)
              maxOpcode' <- getInt32
              unless (maxOpcode' <= maxOpcode) $
                fail ("max opcode too big: " ++ show maxOpcode')
              skip 8 -- label & function counts
              readOperation `untilM` isEmpty
     
readAtom :: [Atom] -> Get Atom
readAtom atoms = fmap ((atoms !!) . (subtract 1)) getInt32

readOperation :: Get Operation
readOperation =
  do (opcode, argCount) <- fmap (opcodeInfo . fromIntegral) getWord8
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
  

-- Helper functions for reading binary stuff.

getString :: Integral a => a -> Get String
getString n =
  getLazyByteString (fromIntegral n) >>= return . unpackByteString

unpackByteString :: B.ByteString -> String
unpackByteString = T.unpack . decodeASCII

getInt32 :: Integral a => Get a
getInt32 = fmap fromIntegral getWord32be

getInt8 :: Integral a => Get a
getInt8 = fmap fromIntegral getWord8

readMany :: Get a -> Get [a]
readMany m = getInt32 >>= flip replicateM m

align :: Int -> Get ()
align n =
  do m <- fmap fromIntegral bytesRead
     skip ((((m + n - 1) `div` n) * n) - m)

expecting :: (Eq a, Show a) => Get a -> (String, a) -> Get ()
expecting m (name, x) =
  do y <- m
     unless (y == x) (fail ("Wrong " ++ name ++ ": expected " ++
                            show x ++ ", not " ++ show y))
