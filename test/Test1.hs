{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Text.Printf

import           Process
import           System

data ProcState = ProcState
  { my      :: Int      -- ^ The number assigned to the process at start-up.
  , m       :: Int      -- ^ The maximum number received from the incoming channel so far.
  , y       :: Int      -- ^ The number to be send to the outgoing channel.
  , counter :: Int      -- ^ How many times the process received a message.
  }

proc :: Channel -> Channel -> StateT ProcState Process ()
proc in_ch out_ch = do
  ProcState my _ _ _ <- get
  lift $ say $ printf "my number = %d." my
  modify $ \s -> s { m = my
                   , y = my
                   , counter = 0 }

  forever $ do
    ProcState my m y counter <- get

    -- Send and receive a message in parallel threads.
    [_, x'] <- lift $ fork [ do send out_ch y
                           
                           , do (x::Int) <- receive in_ch
                                exit x
                           ]
    let x = read x'
      
    lift $ say $ printf "my = %d; x = %d; m = %d; y = %d; counter = %d" my x m y counter
    
    modify $ \s -> s { y = x
                     , m = max m x
                     , counter = counter + 1 }

    when (x == my) $ do
      ProcState my m y counter <- get
      lift $ say $ printf "my = %d; x = %d; m = %d; y = %d; counter = %d" my x m y counter
      if (my == m)
        then lift $ say "I'm the leader."
        else lift $ say $ printf "The leader has number %d." m
      lift $ exit ()


-- | 'genCircle' creates @num@ processes and communication channels
-- between them. The processes are connected in a circular manner:
--      P_{0} -> P_{1} -> ... -> P_{num-1} -> P_{0}.
--
-- The function initializes every process with a unique number.
genCircle :: Int -> [(String, Process ())]
genCircle num =
  map (\n -> (show n, evalStateT (proc (channel ((n-1) `mod` num) n)
                                       (channel n ((n+1) `mod` num)))
                                       (ProcState n undefined undefined undefined)))
  [0 .. num-1]
  where
    channel p1 p2 = Channel $ printf "%d-%d" p1 p2

main = runSystem $ genCircle 4

