{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Process where

import           Control.Monad
import           Control.Monad.Free
import           Data.Functor.Classes
import           Data.Maybe

newtype Channel = Channel { ch_name :: String }
  deriving (Eq, Ord)

-- | @Message@ denotes the type for messages communicating between
-- processes by 'send' and 'receive' functions.
type Message = String

-- | @RetValue@ denotes the type for data returned by a subprocess
-- to a parent process.
type RetValue = String

-- | Instruction defines the set of external operations that a process
-- execute. Instruction is an analogue of operating system API.
data Instr cont
  = Send Channel Message cont
  | Receive Channel (Message -> cont)
  | Fork [Process ()] ([RetValue] -> cont)
  | Say String cont
  | Exit RetValue
  deriving (Functor)

type Process = Free Instr

send :: (Show a) => Channel -> a -> Process ()
send ch msg = liftF $ Send ch (show msg) ()

receive :: (Read a) => Channel -> Process a
receive ch = liftM read $ liftF $ Receive ch id

fork :: [Process ()] -> Process [RetValue]
fork plist = liftF $ Fork plist id

say :: String -> Process ()
say str = liftF $ Say str ()

exit :: (Show a) => a -> Process ()
exit msg = liftF $ Exit (show msg)
