module Ch05.RPN where

import Control.Arrow ((>>>))
import Control.Monad (guard)
import Control.Monad.State (State (..), get, modify, put)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Foldable (traverse_)

type Stack = [Integer]

type StackT m = StateT Stack m

eval :: String -> Maybe Integer
eval = words >>> (\ws -> evalStateT (evalListOfTokens ws) [])

evalListOfTokens :: (MonadFail m) => [String] -> StackT m Integer
evalListOfTokens ts = traverse_ evalToken ts *> pop <* guardStackIsEmpty

guardStackIsEmpty :: (MonadFail m) => StackT m ()
guardStackIsEmpty = get >>= (null >>> \s -> if s then pure () else fail "stack is not empty")

evalToken :: (MonadFail m) => String -> StackT m ()
evalToken "+" = evalOp (+)
evalToken "-" = evalOp (-)
evalToken "*" = evalOp (*)
evalToken n = lift (readM n) >>= push

evalOp :: (MonadFail m) => (Integer -> Integer -> Integer) -> StackT m ()
evalOp op = flip op <$> pop <*> pop >>= push

push :: (MonadFail m) => Integer -> StackT m ()
push n = modify (n :)

pop :: (MonadFail m) => StackT m Integer
pop = do
  (n : ns) <- get -- we are using the MonadFail instance of Maybe here
  put ns
  pure n

readM :: (MonadFail m, Read a) => String -> m a
readM s
  | [x] <- [x | (x, "") <- reads s] = pure x
  | otherwise = fail $ "Failed to parse: \"" ++ s ++ "\""