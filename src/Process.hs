{-# LANGUAGE DeriveFunctor #-}

module Process where

import           Control.Monad
import           Control.Monad.Free
import           Data.Maybe

newtype Channel = Channel { ch_name :: String }
  deriving (Eq, Ord)

type Message = String

-- | Instruction defines the set of external operations that a process
-- execute. Instruction is an analogue of operating system API.
data Instr cont
  = Send Channel Message cont
  | Receive Channel (Message -> cont)
  | Say String cont
  | Exit
  deriving (Functor)

type Process = Free Instr

send :: (Show a) => Channel -> a -> Process ()
send ch msg = liftF $ Send ch (show msg) ()

receive :: (Read a) => Channel -> Process a
receive ch = liftM read $ liftF $ Receive ch id

say :: String -> Process ()
say str = liftF $ Say str ()

exit :: Process ()
exit = liftF Exit
