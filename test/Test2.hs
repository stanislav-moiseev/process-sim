module Main where

import           System.Environment
import           Control.Monad
import           Data.List
import           Text.Printf

import Structural.ProcessGraph

data ProcState = ProcState
  { state_id :: !Int
  , x        :: !Int      -- ^ The number received from the incoming channel.
  , y        :: !Int      -- ^ The number to be send to the outgoing channel.
  , m        :: !Int      -- ^ The maximum number received from the incoming channel so far.
  , counter  :: !Int      -- ^ How many times the process has received a message.
  , leader   :: !(Maybe Bool)
  } deriving (Eq, Ord, Show)

-- | Specification of the Leader Election Protocol.
proc :: Int -> Channel -> Channel -> ProcessGraph ProcState Int
proc my in_ch out_ch s@(ProcState state_id x y m counter leader) =
  case state_id of
    1 -> [ Send out_ch y (s { state_id = 2})
         , Receive in_ch (\x' -> s { state_id = 3, x = x' }) ]
    2 -> [ Receive in_ch (\x' -> s { state_id = 4, x = x' }) ]
    3 -> [ Send out_ch y (s { state_id = 4}) ]
    4 -> [ Tau $ s { state_id = 5
                   , y = x
                   , m = max m x
                   , counter = counter + 1 } ]
    5 -> [ Tau $ if x /= my then s { state_id = 1 }
                            else s { state_id = 6, leader = Just (my == m) } ]
    6 -> [ Stop ]

procs :: Int -> [ProcessGraph ProcState Int]
procs num =
  [ proc n (channel ((n-1) `mod` num) n) (channel n ((n+1) `mod` num)) | n <- [0 .. num-1]]
  where
    channel p1 p2 = Channel $ printf "%d-%d" p1 p2

states :: Int -> [ProcState]
states num =
  [ProcState 1 my my my 0 Nothing | my <- [0 .. num-1]]

test_full num = do
  let trace = eval (procs num) [Active $ states num]
  forM_ (zip [(0::Int)..] trace) $ \(step, composed_states) -> do
    let str = printf "[step %d]" step :: String
    printf "\n%s\n" (replicate 80 '-')
    forM_ (sort composed_states) $ \cs -> do
      print cs

test_short :: Int -> IO ()
test_short num = do
  let trace = eval (procs num) [Active $ states num]
  forM_ (zip [(0::Int)..] trace) $ \(step, composed_states) -> do
    let str = printf "[step %d]" step :: String
    printf "\n%s %s %s\n" (replicate 35 '-') str (replicate (80-35-2-(length str)) '-')
    forM_ (sort composed_states) $ \cs -> do
      case cs of
        Active lst   -> printf "Active   %s  %s\n" (show $ map state_id lst) (show $ map counter lst)
        Stopped lst  -> printf "Stopped  %s  %s\n" (show $ map state_id lst) (show $ map counter lst)
        Deadlock lst -> printf "Deadlock %s  %s\n" (show $ map state_id lst) (show $ map counter lst)

main = do
  args <- getArgs
  case args of
    []    -> do prog_name <- getProgName
                printf "Usage: %s [number_of_processes]\n" prog_name
    (n:_) -> test_short $ read n
