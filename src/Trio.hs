{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Trio
  ( Trio
  , runTrio
  , runTrio_
  , runTrioVoid
  , mapException
  , throwIO
  , throwUnchecked
  , catch
  , try
  , bracket
  , MonadIO (..)
  , liftIOChecked
  , readFileBinary
  , withBinaryFile
  , openBinaryFile
  , hClose
  , FromIOException (..)
  ) where

import Data.Void
import Data.Typeable
import qualified Control.Exception as E
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as B
import qualified System.IO as IO

newtype Trio r e a = Trio (ReaderT r IO a)
  deriving (Functor, Applicative, Monad)

unTrio :: Trio r e a -> r -> IO a
unTrio (Trio (ReaderT f)) = f

mkTrio :: (r -> IO a) -> Trio r e a
mkTrio = Trio . ReaderT

data Checked = Checked Any
  deriving Typeable
instance Show Checked where
  show _ = "Some checked exception from Trio, you shouldn't see this"
instance E.Exception Checked

-- | Return exceptions in an 'Either'
runTrio :: MonadIO m => r -> Trio r e a -> m (Either e a)
runTrio r t = runTrioVoid r $ try t

-- | Rethrows checked exceptions as runtime exceptions
runTrio_ :: (MonadIO m, E.Exception e) => r -> Trio r e a -> m a
runTrio_ r t = liftIO $ runTrio r t >>= either E.throwIO return

-- | Guaranteed lack of checked exceptions via Void
runTrioVoid :: MonadIO m => r -> Trio r Void a -> m a
runTrioVoid r t = liftIO $ unTrio t r

-- | Throw an exception as a checked exception. This must not be an
-- async exception type.
throwIO :: e -> Trio r e void
-- FIXME do something about trying to use an async exception type
-- here? Looks like we can't do that without adding an E.Exception
-- constraint
throwIO e = mkTrio (\_ -> E.throwIO $ Checked $ unsafeCoerce e)

-- | Throw an exception as an unchecked exception
throwUnchecked :: E.Exception e => e -> Trio r e' void
throwUnchecked e = mkTrio $ \_ -> E.throwIO e

-- | Catch a checked exception
catch :: Trio r e1 a -> (e1 -> Trio r e2 a) -> Trio r e2 a
catch t onErr = mkTrio $ \r ->
  unTrio t r `E.catch` \(Checked e) ->
    let Trio (ReaderT g) = onErr (unsafeCoerce e)
     in g r

-- | Return a checked exception as an 'Either' value
try :: Trio r e a -> Trio r void (Either e a)
try t = fmap Right t `catch` (return . Left)

-- | Allocate a resource in an exception safe manner
bracket
  :: Trio r e a -- ^ allocate
  -> (a -> Trio r e ()) -- ^ cleanup
  -> (a -> Trio r e b) -- ^ inner action
  -> Trio r e b
bracket alloc cleanup inner = mkTrio $ \r -> E.bracket
  (unTrio alloc r)
  (\a -> unTrio (cleanup a) r)
  (\a -> unTrio (inner a) r)

instance e ~ E.SomeException => MonadIO (Trio r e) where
  liftIO = liftIOChecked

-- | Lift an 'IO' action to a 'Trio' action, wrapping up checked
-- exceptions. Any other exception types will be rethrown as unchecked
-- exceptions.
liftIOChecked :: forall r e a. E.Exception e => IO a -> Trio r e a
liftIOChecked io =
  mkTrio $ \_ -> io `E.catch` \(e :: e) ->
    if isAsyncException e
      then E.throwIO e
      else E.throwIO $ Checked $ unsafeCoerce e

isAsyncException :: E.Exception e => e -> Bool
isAsyncException e =
  case E.fromException (E.toException e) of
    Just (E.SomeAsyncException _) -> True
    Nothing -> False

-- | Things which can be converted from an 'E.IOException'
class FromIOException e where
  fromIOException :: E.IOException -> e

instance FromIOException E.IOException where
  fromIOException = id
instance FromIOException E.SomeException where
  fromIOException = E.toException

-- | Convert from one checked exception type to another.
mapException :: (e1 -> e2) -> Trio r e1 a -> Trio r e2 a
mapException f t = t `catch` (throwIO . f)

-- | Read a binary file
readFileBinary :: FromIOException e => FilePath -> Trio r e B.ByteString
readFileBinary = mapException fromIOException . liftIOChecked . B.readFile

-- | Open a file in binary mode. Better to use 'withBinaryFile'
openBinaryFile
  :: FromIOException e
  => FilePath
  -> IO.IOMode
  -> Trio r e IO.Handle
openBinaryFile fp mode
  = mapException fromIOException
  $ liftIOChecked
  $ IO.openBinaryFile fp mode

-- | Close a file handle
hClose
  :: FromIOException e
  => IO.Handle
  -> Trio r e ()
hClose = mapException fromIOException . liftIOChecked . IO.hClose

-- | Perform an action with an open binary file
withBinaryFile
  :: FromIOException e
  => FilePath
  -> IO.IOMode
  -> (IO.Handle -> Trio r e a)
  -> Trio r e a
withBinaryFile fp mode inner = bracket
  (openBinaryFile fp mode)
  hClose
  inner