module Cache(verifyAndStore, loadOrCompute) where

import Control.Monad
import System.Directory
import Test.Hspec
import Text.Printf

filename :: Int -> Char -> FilePath
filename = printf ".cache/%02d%c"

-- tries to fetch the result from the cache and if absent computes it
loadOrCompute :: Int -> Char -> String -> IO (Bool, String)
loadOrCompute day part computed = do
  createDirectoryIfMissing True ".cache"
  is_cached <- doesFileExist (filename day part)
  result <- if is_cached then readFile (filename day part) else return computed
  return (is_cached, result)

-- assert the result, and if correct saves it
verifyAndStore :: Int -> Char -> String -> String -> IO ()
verifyAndStore day part expected computed = do
  (is_cached, result) <- loadOrCompute day part computed
  when is_cached $ unless (result == expected) $ removeFile (filename day part)
  result `shouldBe` expected
  unless is_cached $ writeFile (filename day part) result
