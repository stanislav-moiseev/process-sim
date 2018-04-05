module Structural.ProcessGraph where

import           Control.Monad
import           Data.List

newtype Channel = Channel { ch_name :: String }
  deriving (Eq, Ord)

-- | @Trans@ defines possible edge labels of a process graph.
--
-- @s@ defines a data type for process states. A process state can include,
-- e.g., a vertex of a process graph and internal process variables.
data Trans s msg
  = Stop                        -- ^ No edge. This corresponds to final states.
  
  | Tau s                       -- ^ An internal action. Move to state @s@.
  
  | Send Channel msg s          -- ^ Send a message and then move to state @s@.
  
  | Receive Channel (msg -> s)  -- ^ Receive a message and then move to the
                                -- state defined by the continuation function.
                                -- The continuation function can be used to save
                                -- the received value in the internal process
                                -- state.

isStop Stop = True
isStop _    = False

-- | @ProcessGraph@ is a function from process states (e.g. graph vertices) to a
-- set of possible transitions which are selected nondeterministically.
type ProcessGraph s msg = s -> [Trans s msg]

-- | @ComposedState@ characterizes possible states of a "composed" process.
--
-- Here @s@ denotes the data type of states of individual processes in the
-- composition. And @[s]@ describes the state of the composed process.
data ComposedStateStatus = Active | Stopped | Deadlock
  deriving (Eq, Ord, Show)

-- | @step@ makes one computation step of the composed process.
-- It accepts a list of processes @procs@ and their initial states @states@
-- and returns all possible next states.
--
-- [Definition]
--
-- Let @P: (s, t)@ denote a possible transition of a process @P@, where
--   * @s@ is a state of process @P@;
--   * @t@ is a transition of process @P@ starting at state @s@, i.e. @t `elem` P s@.
--
-- All possible transitions of the composed process are defined by the
-- following rules:
--
-- > [STOP]       P1: (s1, Stop)
-- >              ...
-- >              Pn: (sn, Stop)
-- >          ==> (s1,...,sn) ~> (s1,...,sn)
--
-- > [TAU]        Pk: (sk, Tau sk')
-- >          ==> (...,sk,...) ~> (...,sk',...)
--
-- > [REACT]      Pi: (si, Send ch1 msg si')
-- >              Pj: (sj, Receive ch2 cont2)
-- >              i ≠ j
-- >              ch1 == ch2
-- >          ==> (...,si,...,sj,...) ~> (...,si',...,sj',...)
-- >              where sj' = cont2 msg
--
-- * If the STOP rule is applicable to some composed state (s1,...,sn),
--   then we mark the next state as 'Stopped'.
--
-- * If TAU or REACT rules is applicable to some composed state (s1,...,sn),
--   then we mark the next state as 'Active'.
--
-- * If /no/ rule is applicable to some composed state (s1,...,sn),
--   then the process does not change state, and we mark the state as 'Deadlock'.
--
step :: (Eq s) => [ProcessGraph s msg] -> (ComposedStateStatus, [s]) -> [(ComposedStateStatus, [s])]
step procs (Stopped, states)  = [(Stopped, states)]
step procs (Deadlock, states) = [(Deadlock, states)]
step procs (Active, states) =
  -- Get a list of possible transitions for every process and compute all the
  -- combinations of transitions.
  --
  -- The @sequence@ function (when used in the List monad) computes the
  -- cartesian product of lists, e.g.
  --
  -- > sequence [[1,2],[3,4,5]] == [[1,3],[1,4],[1,5],[2,3],[2,4],[2,5]]
  let combined_trans = sequence $ zipWith ($) procs states in

  -- Find all applicable rules.
  let stop_rules  = [states' | tr <- combined_trans
                             , all isStop tr
                             , let states' = states]

      tau_rules   = [states' | tr <- combined_trans
                             , (k, Tau s') <- zip [0..] tr
                             , let states' = updateList k s' states]

      react_rules = [states' | tr <- combined_trans
                             , (k1, Send ch1 msg s1') <- zip [0..] tr
                             , (k2, Receive ch2 cont2) <- zip [0..] tr
                             , ch1 == ch2
                             , let states' = updateList k1 s1' $ updateList k2 (cont2 msg) states] in

  if null $ stop_rules ++ tau_rules ++ react_rules
     then [ (Deadlock, states) ]
     else nub $ (map (\s -> (Stopped,s)) stop_rules) ++
                (map (\s -> (Active,s)) $ tau_rules ++ react_rules)

  where
    updateList :: Int -> a -> [a] -> [a]
    updateList pos new lst =
      take pos lst ++ [new] ++ drop (pos+1) lst


-- | @eval@ simulates the concurrent execution of processes @procs@.
--
-- > eval procs states = [ cs0, cs1, cs2, ... ],
--
-- where @cs_k@ is the list of all possible states of the composed process at
-- step @k@.
--
-- The simulation terminates when all processes have stopped or come to a
-- deadlock.
--
-- If there is a loop is the process graph, @eval@ will return an infinite list.
eval :: Eq s => [ProcessGraph s msg] -> [s] -> [[(ComposedStateStatus, [s])]]
eval procs states =
  let trace = iterate (nub . concatMap (step procs)) [(Active, states)]
      (prefix, suffix) = span (any (\(status, states) -> status == Active)) trace in
  prefix ++ [head suffix]
