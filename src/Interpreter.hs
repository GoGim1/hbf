module Interpreter ( exec ) where

import Control.Monad.State
    ( when,
      MonadState(get),
      StateT,
      MonadTrans(lift),
      gets,
      modify,
      evalStateT )
import Data.Char ( ord, chr )
import Instruction ( Instruction(..) )
import qualified Machine
import Parser ( parseP )

type Interpreter = StateT Machine.Machine IO

exec :: String -> IO ()
exec input = evalStateT (run $ parseP input) Machine.init

run :: [Instruction] -> Interpreter ()
run = mapM_ step

step :: Instruction -> Interpreter ()
step Inc = modify Machine.inc
step Dec = modify Machine.dec
step Prev = modify Machine.prev
step Next = modify Machine.next
step Write = gets Machine.get >>= lift. print . chr
step Read = do
    c <- lift getChar
    modify $ Machine.set (ord c)
step loop@(Loop ops) = do 
    m <- get
    when (Machine.get m /= 0) (run ops >> step loop)

