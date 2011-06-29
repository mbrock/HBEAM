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

main :: IO ()
main =
  do binary <- B.getContents
     print (parseBEAMFile (readBEAMFile binary))
     
readBEAMFile binary = runGet (getHeader >> getChunks) binary
  where 
    getHeader =
      do skip 12
    getChunks =
      do done <- isEmpty
         if done
           then return []
           else do chunk <- getChunk
                   chunks <- getChunks
                   return (chunk:chunks)
    getChunk =
      do name <- getLazyByteString 4
         stuff <- getInt32 >>= getLazyByteString
         align 4
         return (unpackByteString name, stuff)
         
data BEAMFile = BEAMFile { beamFileAtoms :: [Atom] 
                         , beamFileCode :: [(String, [BEAMArgument])] }
              deriving Show

parseBEAMFile chunks =
  do atomChunk <- lookup "Atom" chunks
     codeChunk <- lookup "Code" chunks
     return $ BEAMFile { beamFileAtoms = parseAtomChunk atomChunk 
                       , beamFileCode = parseCodeChunk codeChunk }
     
getString :: Integral a => a -> Get String
getString n =
  getLazyByteString (fromIntegral n) >>= return . unpackByteString
  
unpackByteString = T.unpack . decodeASCII
     
newtype Atom = Atom String
             deriving Show
  
parseAtomChunk chunk = flip runGet chunk $
  do count <- getInt32
     replicateM count (getWord8 >>= getString >>= return . Atom)

getInt32 :: Integral a => Get a
getInt32 = fmap fromIntegral getWord32be

getInt8 :: Integral a => Get a
getInt8 = fmap fromIntegral getWord8

align :: Int -> Get ()
align n =
  do m <- fmap fromIntegral bytesRead
     skip ((((m + n - 1) `div` n) * n) - m)
     
parseCodeChunk chunk = flip runGet chunk $
  do infoLength <- getInt32
     unless (infoLength == 16) $
       fail ("weird code info length: " ++ show infoLength)
     instrSet <- getInt32
     unless (instrSet == 0) $
       fail ("weird instruction set id: " ++ show instrSet)
     maxOpcode' <- getInt32
     unless (maxOpcode' <= maxOpcode) $
       fail ("max opcode too big: " ++ show maxOpcode')
     skip 8 -- label & function counts
     readOpcode `untilM` isEmpty
     
data BEAMArgumentTag = TagA | TagF | TagH | TagI | TagU
                     | TagX | TagY | TagZ
                     | TagFR | TagAtom | TagFloat | TagLiteral
     
readOpcode =
  do (opName, argCount) <- fmap (opcodeInfo . fromIntegral) getWord8
     args <- replicateM argCount readArgument
     return (opName, args)
     
readArgument =
  do taggedByte <- fmap fromIntegral getWord8
     case parseTag taggedByte of
       TagZ -> readZArgument taggedByte
       TagA -> readAArgument taggedByte
       _ -> readIArgument taggedByte
     
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
    
readAArgument tag = 
  -- FIXME: wrong
  readIArgument tag
  
readZArgument tag =
  fail "can't handle floats or lists yet"
  
data BEAMArgument = IArg Integer
  deriving Show
                    
readIArgument tag | tag .&. 0x8 == 0 =
  return $ IArg (tag `shiftR` 4)
readIArgument tag | tag .&. 0x10 == 0 =
  do b <- getInt8
     return $ IArg (((tag .&. 0xe0) `shiftL` 3) .|. b)
readIArgument tag =
  fail "integer too big for me"