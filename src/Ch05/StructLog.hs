module Ch05.StructLog where

import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)
import Control.Monad ((>=>))
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Writer (WriterT (..))
import Control.Monad.Writer (MonadWriter (pass, tell), Writer, runWriter)
import Data.Foldable (traverse_)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Prelude hiding (log)

data Item = Msg String | Section String [Item]
  deriving (Show, Eq)

type Log = [Item]

type Logging a = Writer Log a

main :: Bool -> IO ()
main withTS
  | withTS = runTS programTS
  | otherwise = run program
  where
    program :: Logging Integer
    program = do
      let init = 5
      log $ "start with " ++ show init
      res' <- withSection "expression" $ do
        log "plus 1"
        let added = init + 1
        log "time 4"
        pure $ added * 4
      log "result"
      pure res'

    programTS :: LoggingTS ()
    programTS = do
      logWithTS "start"
      withSectionTS "delay" $ do
        logWithTS "waiting 3 seconds"
        _ <- liftIO $ threadDelay 3000000
        logWithTS "done"
      logWithTS "result"

run :: Show a => Logging a -> IO ()
run = runLogging >>> (\(res, logs) -> traverse_ print logs *> print res)

runTS :: Show a => LoggingTS a -> IO ()
runTS = runLoggingTS >=> (\(res, logs) -> traverse_ print logs)

log :: Show t => t -> Logging ()
log t = tell [Msg $ show t]

withSection :: String -> Logging a -> Logging a
withSection title l = pass $ do
  a <- l
  pure (a, \items -> [Section title items])

runLogging :: Logging a -> (a, Log)
runLogging = runWriter

data ItemTS
  = MsgTS {message :: String, ts :: POSIXTime}
  | SectionTS {title :: String, start :: POSIXTime, end :: POSIXTime, items :: [ItemTS]}
  deriving (Show, Eq)

type LogTS = [ItemTS]

type LoggingTS a = WriterT LogTS IO a

runLoggingTS :: LoggingTS a -> IO (a, LogTS)
runLoggingTS = runWriterT

logWithTS :: Show t => t -> LoggingTS ()
logWithTS t = liftIO getPOSIXTime >>= (\ts -> tell [MsgTS {message = show t, ts = ts}])

withSectionTS :: String -> LoggingTS a -> LoggingTS a
withSectionTS title ml = pass $ do
  start <- liftIO getPOSIXTime
  l <- ml
  end <- liftIO getPOSIXTime
  pure (l, \items -> [SectionTS {title = title, start = start, end = end, items = items}])
