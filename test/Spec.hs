{-# OPTIONS_GHC -Wno-name-shadowing #-}
import           Prelude hiding (lines, id, lex)
import           Data.Foldable (traverse_)
import           Data.IORef (newIORef, writeIORef, readIORef, IORef)
import           Data.List (isSuffixOf, intercalate)
import           Control.Monad (when)
import           System.Directory (listDirectory, doesFileExist)
import           System.FilePath ((</>))
import           System.Exit (exitFailure)
import           Lexer (lex)
import           Parser (parse)
import           Compiler (compile)
import           Decompiler (decompileAtom)
import           Naive (drive)
import           Data.String (lines)
import           Data.Algorithm.Diff (getGroupedDiff)
import           Data.Algorithm.DiffOutput (ppDiff)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified KnowledgeBase as KB
import Decl (declare)

main :: IO ()
main = do
  failingRef <- newIORef False
  let validateTestOutput' = validateTestOutput failingRef
  sources <- findSources "test/cases"
  (`traverse_` sources)
    $ \source -> do
      contents <- TIO.readFile source
      let validateTestOutput'' = validateTestOutput' source contents

      let ast = parse $ lex contents
      let actualOutput = show ast 
      let expPath = mkExpPath source "ast"
      validateTestOutput'' expPath actualOutput
      
      case declare ast of
        Left errMsg -> do
          let actualOutput = errMsg
          let expPath = mkExpPath source "decl"
          validateTestOutput'' expPath actualOutput
        Right decls -> do
          let actualOutput = "There are no decl errors"
          let expPath = mkExpPath source "decl"
          validateTestOutput'' expPath actualOutput
          
          let (ir, adtMap) = compile decls ast
          let actualOutput = show ir
          let expPath = mkExpPath source "ir"
          validateTestOutput'' expPath actualOutput

          let solution = Naive.drive ir
          let actualOutput = show $ KB.map (decompileAtom decls adtMap) solution
          let expPath = mkExpPath source "naive"
          validateTestOutput'' expPath actualOutput
  failed <- readIORef failingRef
  when failed exitFailure

validateTestOutput :: IORef Bool -> FilePath -> T.Text -> FilePath -> String -> IO ()
validateTestOutput failingRef source contents expPath actualOutput = do
  mExpOutput <- readExpFile expPath
  case mExpOutput of
    Just expOutput -> when (expOutput /= actualOutput)
      $ do
        writeIORef failingRef True
        putStrLn "Mismatch between expected and actual output."
        putStrLn $ showTestCase source contents
        putStrLn ""
        putStrLn $ "Diff (Expected at " <> expPath <> "):"
        let diff =
              getGroupedDiff (lines expOutput) (lines actualOutput)
        putStrLn $ ppDiff diff
        replaceExp expPath actualOutput
        putStrLn ""
    Nothing        -> do
      writeIORef failingRef True
      putStrLn "There is no expected file."
      putStrLn $ showTestCase source contents
      putStrLn ""
      putStrLn "Actual:"
      putStrLn actualOutput
      replaceExp expPath actualOutput
      putStrLn ""

mkExpPath :: FilePath -> String -> FilePath
mkExpPath source suffix = source <> "." <> suffix <> ".exp"

readExpFile :: FilePath -> IO (Maybe String)
readExpFile expPath = do
  exists <- doesFileExist expPath
  if exists
    then Just <$> readFile expPath
    else pure Nothing

findSources :: FilePath -> IO [FilePath]
findSources dir = do
  files <- listDirectory dir
  let infFiles = filter (".inf" `isSuffixOf`) files
  pure (map (dir </>) infFiles)

showTestCase :: FilePath -> T.Text -> String
showTestCase path contents = intercalate
  "\n"
  [ "================================================================================"
  , "Contents of " <> path
  , "================================================================================"
  , T.unpack contents]

replaceExp :: FilePath -> String -> IO ()
replaceExp expPath expContents = do
  putStrLn "Do you want to replace the .exp file? [Y/n]"
  contents <- getLine
  when (contents == "Y") $ writeFile expPath expContents