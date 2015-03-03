module Control.Monad.Parallel.ProgressBar
  ( sequence
  , sequence_
  , mapM
  , mapM_
  , forM
  , forM_
  ) where

import           Control.Concurrent.MonadIO            ( MonadIO, liftIO )
import           Control.Concurrent.STM.MonadIO        ( newTMVar, putTMVar, takeTMVar )
import           Control.Monad                         ( liftM )
import           Control.Monad.Parallel                ( MonadParallel )
import qualified Control.Monad.Parallel         as P   ( sequence, sequence_ )
import           Data.List                             ( genericLength )
import           Prelude                        hiding ( mapM, mapM_, sequence, sequence_ )
import           System.IO                             ( hFlush, stdout )
import           System.ProgressBar                    ( Label, progressBar )

withProgressBar :: MonadIO m => Label -> Label -> Integer -> [m a] -> m [m a]
withProgressBar prefix postfix width ms = do
  let n = genericLength ms

  prog <- newTMVar 0
  liftIO $ do progressBar prefix postfix width 0 n
              hFlush stdout
  return $ flip map ms $ \m -> do
    a <- m
    i <- liftM (+1) $ takeTMVar prog
    liftIO $ do progressBar prefix postfix width i n
                hFlush stdout
    putTMVar prog i
    return a

-- | Execute the actions in parallel with a progress bar. For detail of the
--   progress bar, see "System.ProgressBar".
sequence :: (MonadIO m, MonadParallel m)
         => Label   -- ^ Prefixed label
         -> Label   -- ^ Postfixed label
         -> Integer -- ^ Total progress bar width in characters
         -> [m a] -> m [a]
sequence prefix postfix width ms = P.sequence =<< withProgressBar prefix postfix width ms

sequence_ :: (MonadIO m, MonadParallel m) => Label -> Label -> Integer -> [m a] -> m ()
sequence_ prefix postfix width ms = P.sequence_ =<< withProgressBar prefix postfix width ms

mapM :: (MonadIO m, MonadParallel m) => Label -> Label -> Integer -> (a -> m b) -> [a] -> m [b]
mapM prefix postfix width f = sequence prefix postfix width . map f

mapM_ :: (MonadIO m, MonadParallel m) => Label -> Label -> Integer -> (a -> m ()) -> [a] -> m ()
mapM_ prefix postfix width f = sequence_ prefix postfix width . map f

forM :: (MonadIO m, MonadParallel m) => Label -> Label -> Integer -> [a] -> (a -> m b) -> m [b]
forM prefix postfix width = flip (mapM prefix postfix width)

forM_ :: (MonadIO m, MonadParallel m) => Label -> Label -> Integer -> [a] -> (a -> m ()) -> m ()
forM_ prefix postfix width = flip (mapM_ prefix postfix width)
