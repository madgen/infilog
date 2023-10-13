import           Prelude hiding (lines, id, lex)
import           Data.Foldable (traverse_)
import           Data.IORef (newIORef, writeIORef, readIORef)
import           Data.List (isSuffixOf, intercalate)
import           Control.Monad (when)
import           System.Directory (listDirectory, doesFileExist)
import           System.FilePath ((</>))
import           System.Exit (exitFailure)
import           AST (Program)
import           Lexer (lex)
import           Parser (parse)
import           Compiler (compile)
import           Naive (drive)
import           Data.String (lines)
import           Data.Algorithm.Diff (getGroupedDiff)
import           Data.Algorithm.DiffOutput (ppDiff)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  failingRef <- newIORef False
  sources <- findSources "test/cases"
  (`traverse_` sources)
    $ \source -> do
      contents <- TIO.readFile source
      let ast = parse $ lex contents
      let solution = Naive.drive . compile $ ast
      let actualOutput = show solution
      let expPath = mkExpPath source
      mExpOutput <- readExpFile expPath
      case mExpOutput of
        Just expOutput -> when (expOutput /= actualOutput)
          $ do
            writeIORef failingRef True
            putStrLn "Mismatch between expected and actual output."
            putStrLn $ showTestCase source ast
            putStrLn $ "Diff (Expected at " <> expPath <> "):"
            let diff =
                  getGroupedDiff (lines expOutput) (lines actualOutput)
            putStrLn $ ppDiff diff
            replaceExp expPath actualOutput
            putStrLn ""
        Nothing        -> do
          writeIORef failingRef True
          putStrLn "There is no expected file."
          putStrLn $ showTestCase source ast
          putStrLn "Actual:"
          putStrLn actualOutput
          replaceExp expPath actualOutput
          putStrLn ""
  failed <- readIORef failingRef
  when failed exitFailure

mkExpPath :: FilePath -> FilePath
mkExpPath source = source <> ".exp"

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

showTestCase :: FilePath -> Program -> String
showTestCase path ast = intercalate
  "\n"
  [ "================================================================================"
  , "AST for " <> path
  , "================================================================================"
  , show ast]

replaceExp :: FilePath -> String -> IO ()
replaceExp expPath expContents = do
  putStrLn "Do you want to replace the .exp file? [Y/n]"
  contents <- getLine
  when (contents == "Y") $ writeFile expPath expContents