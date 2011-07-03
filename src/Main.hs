module Main where 

import System.Environment (getArgs)

import Language.Erlang.BEAM.Loader
import Language.Erlang.BEAM.Emulator

import qualified Data.ByteString.Lazy as B

import Control.Applicative

main :: IO ()
main = do Just x <- (parseBEAMFile . readBEAMFile) <$> B.getContents
          putStrLn "Imports: "
          mapM_ (\(i, MFA (Atom m) (Atom f) a) ->
                  putStrLn ("    " ++ show i ++ ": " ++ m ++ ":" ++ f
                            ++ "/" ++ show a))
            (zip [(0::Int)..] (beamFileImports x))
          putStrLn ""
          putStrLn "Exports: "
          mapM_ (\(Export (Atom f) a e) ->
                  putStrLn ("    " ++ f ++ "/" ++ show a ++ " @ " ++ show e))
            (beamFileExports x)
          putStrLn ""

          mapM_ (\(FunDef (Atom name) arity label ops) ->
                  do putStrLn (name ++ "/" ++ show arity ++ " (@" ++
                               show label ++ "):")
                     mapM_ (\op -> putStrLn ("    " ++ show op)) ops
                     putStrLn "") (beamFileFunDefs x)

          putStrLn ""
          [m', f', a', args'] <- getArgs
          node <- nodeFromBEAMFile x
          let mfa = MFA (Atom m') (Atom f') (read a')
              args = read args'
          putStrLn $ "Spawning " ++ showMFA mfa ++ "."
          spawnProcess node mfa args
          