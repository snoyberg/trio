import Test.Hspec
import Trio
import qualified System.IO as IO
import System.IO.Error
import qualified Data.ByteString as B

main :: IO ()
main = hspec $ do
  it "read non-existent file" $ do
    res <- runTrio () $ readFileBinary "does not exist"
    case res of
      Left e -> e `shouldSatisfy` isDoesNotExistError
      Right bs -> error $ "somehow read a non-existent file: " ++ show bs
  it "read existent file" $ do
    let fp = "trio.cabal"
    actual <- runTrio_ () (readFileBinary fp :: Trio () IOError B.ByteString)
    expected <- B.readFile fp
    actual `shouldBe` expected
  it "withBinaryFile works" $ do
    let fp = "trio.cabal"
        inner = withBinaryFile fp IO.ReadMode
              $ liftIOChecked . B.hGetContents
    actual <- runTrio_ () (inner :: Trio () IOError B.ByteString)
    expected <- B.readFile fp
    actual `shouldBe` expected
