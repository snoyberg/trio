import Test.Hspec
import Trio
import qualified System.IO as IO
import System.IO.Error
import qualified Data.ByteString as B
import Data.IORef -- FIXME add a Trio-lifted version of this

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
  it "scoped" $ do
    ref <- newIORef 0
    runTrio_ () $ do
      scoped $ do
        x <- liftIO $ readIORef ref
        liftIO $ x `shouldBe` 0
        _ <- allocate
          (liftIO $ writeIORef ref 1)
          (const $ liftIO $ writeIORef ref 2)
        y <- liftIO $ readIORef ref
        liftIO $ y `shouldBe` 1
      x <- liftIO $ readIORef ref
      liftIO $ x `shouldBe` 2
  it "binaryFile works" $ do
    let fp = "trio.cabal"
        inner = do
          h <- binaryFile fp IO.ReadMode
          liftIOChecked $ B.hGetContents h
    actual <- runTrio_ () (inner :: Trio () IOError B.ByteString)
    expected <- B.readFile fp
    actual `shouldBe` expected
