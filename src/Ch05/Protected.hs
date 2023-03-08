module Ch05.Protected where

import Control.Monad.Reader (Reader, ask, asks, runReader)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..))

main :: Maybe String -> IO ()
main pass = withSecret (ProtectedData "password" 42) (\n -> "secret number is " ++ show n) >>= print
  where
    withSecret :: ProtectedData s -> (s -> a) -> IO (Maybe a)
    withSecret s fn = case pass of
      Just pwd -> pure $ run s (fn <$> access pwd)
      Nothing -> runIO s (fn <$> accessIO)

data ProtectedData a = ProtectedData String a

accessData :: String -> ProtectedData a -> Maybe a
accessData s (ProtectedData pass v) =
  if s == pass then Just v else Nothing

type Protected s a = MaybeT (Reader (ProtectedData s)) a

run :: ProtectedData s -> Protected s a -> Maybe a
run protected computation = runReader (runMaybeT computation) protected

access :: String -> Protected a a
access pass = lift ask >>= (MaybeT . pure . accessData pass)
-- access = MaybeT . asks . accessData

type ProtectedIO s a = MaybeT (ReaderT (ProtectedData s) IO) a

runIO :: ProtectedData s -> ProtectedIO s a -> IO (Maybe a)
runIO protected computation = runReaderT (runMaybeT computation) protected

accessIO :: ProtectedIO a a
accessIO = do
  protected <- lift ask
  liftIO $ putStrLn "enter password: "
  password <- liftIO getLine
  let accessed = accessData password protected
  MaybeT (pure accessed)