module Main where

import           System.Environment
import           Control.Monad
import           Data.List
import           Text.Printf

import Structural.ProcessGraph

data ProcState = ProcState
  { s       :: !Int     -- ^ vertex id
  , x       :: !Int     -- ^ the number received from the incoming channel
  , y       :: !Int     -- ^ the number to be send to the outgoing channel
  , m       :: !Int     -- ^ the maximum number received from the incoming channel so far
  , counter :: !Int     -- ^ how many times the process has received a message
  , leader  :: !(Maybe Bool)
  } deriving (Eq, Ord, Show)

-- | Specification of the Leader Election Protocol.
proc
  :: Int                -- ^ an integer mark assigned to the process
  -> Channel            -- ^ incoming channel from P_{k-1}
  -> Channel            -- ^ outgoing channel to P_{k+1}
  -> ProcessGraph ProcState Int
proc my in_ch out_ch p@(ProcState s x y m counter leader) =
  case s of
    1 -> [ Send out_ch y (p { s = 2 })
         , Receive in_ch (\x' -> p { s = 3, x = x' }) ]
    2 -> [ Receive in_ch (\x' -> p { s = 4, x = x' }) ]
    3 -> [ Send out_ch y (p { s = 4}) ]
    4 -> [ Tau $ p { s = 5
                   , y = x
                   , m = max m x
                   , counter = counter + 1 } ]
    5 -> (guard (x /= my) >> [ Tau $ p { s = 1 } ]) ++
         (guard (x == my) >> [ Tau $ p { s = 6, leader = Just (my == m) } ])
    6 -> [ Stop ]

procInv
  :: Int                -- ^ total number of processes
  -> [Int]              -- ^ integer marks assigned to processes
  -> [ProcState]        -- ^ a list of process states
  -> Bool
procInv num marks states =
  all (\(k, p) -> inv_x k p && inv_y k p && inv_m k p && inv4 k p &&
                  inv_react k p (states !! ((k+1) `mod` num))) $
  zip [0..] states
    
  where
    inv_x k (ProcState s x y m counter leader)
      | s `elem` [3,4] = x == marks !! ((k - counter - 1) `mod` num)
      | otherwise      = x == marks !! ((k - counter) `mod` num)

    inv_y k (ProcState s x y m counter leader) =
      y == marks !! ((k - counter) `mod` num)

    inv_m k (ProcState s x y m counter leader) =
      m == (maximum $ map (\c -> marks !! (c `mod` num)) [k, k-1 .. k - counter])
      
    inv4 k (ProcState s x y m counter leader)
      | s == 5    = counter > 0
      | s == 6    = leader == Just (marks !! k == maximum marks)
      | otherwise = True

    inv_react k (ProcState s1 x1 y1 m1 counter1 leader1) (ProcState s2 x2 y2 m2 counter2 leader2)
      | s1 `elem` [5,1,3] && s2 `elem` [5,1,2] = counter1 == counter2
      | s1 `elem` [2,4]   && s2 `elem` [3,4]   = counter1 == counter2
      | s1 `elem` [5,1,3] && s2 `elem` [3,4]   = counter1 == counter2 + 1
      | s1 `elem` [2,4]   && s2 `elem` [5,1,2] = counter1 + 1 == counter2
      | otherwise                              = True
      

-- | @procs@ takes the number of processes @num@ and returns the list of process
-- graphs, where the processes a connected in a circular way.
procs :: Int -> [ProcessGraph ProcState Int]
procs num =
  [ proc n (channel ((n-1) `mod` num) n) (channel n ((n+1) `mod` num)) | n <- [0 .. num-1]]
  where
    channel p1 p2 = Channel $ printf "%d-%d" p1 p2

-- | @states@ takes the number of processes @num@ and returns the list of
-- initial states, i.e. it sets up the starting vertices of the process graphs
-- and internal process variables.
states :: Int -> [ProcState]
states num =
  [ProcState 1 my my my 0 Nothing | my <- [0 .. num-1]]


test_short :: Int -> IO ()
test_short num = do
  let trace = eval (procs num) $ states num
  let inv   = procInv num [0 .. num-1]
  
  inv_checks <- forM (zip [(0::Int)..] trace) $ \(step, composed_states) -> do
    let str = printf "[step %d]" step :: String
    printf "\n%s%s%s\n" (replicate 35 '-') str (replicate (80-35-(length str)) '-')
    inv_checks <- forM (sort composed_states) $ \(status, states) -> do
      let inv_check = inv states
      printf "%-8s %s  %s  %s\n" (show status) (show $ map s states) (show $ map counter states) (show $ inv_check)
      return inv_check
    return $ and inv_checks

  if (and inv_checks)
  then putStrLn "\nSumulation finished. All invariants are True.\n"
  else putStrLn "\nSumulation finished. Some invariant is False.\n"


main = do
  args <- getArgs
  case args of
    (n:_) -> test_short $ read n
    _     -> do prog_name <- getProgName
                printf "Usage: %s [number-of-processes]\n" prog_name

