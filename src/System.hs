module System (runSystem) where

import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict                  as M

import           Data.Time.Clock.System
import           System.Random

import           SystemTypes
import           SystemLog
import           Process

processStep :: PID -> Process () -> SystemM ()

processStep pid (Pure a) = lift $ logProcTerm pid

processStep pid (Free Exit) = lift $ logProcTerm pid

processStep sender_id (Free (Send ch msg sender)) = do
  receive_queue <- gets receive_queue
  case ch `M.lookup` receive_queue of
    Nothing -> do
      queue <- gets queue
      send_queue <- gets send_queue
      modify $ \s -> s { send_queue = M.insertWith (flip (++)) ch [(sender_id, msg)] send_queue
                       , queue = queue ++ [(sender_id, sender)] }

    Just ((receiver_id, receiver) : others) -> do
      lift $ logProcMsg sender_id receiver_id ch msg
      queue <- gets queue
      modify $ \s -> s { queue         = queue ++ [(receiver_id, receiver msg), (sender_id, sender)]
                       , receive_queue = if null others
                                         then ch `M.delete` receive_queue
                                         else M.insert ch others receive_queue }

processStep receiver_id (Free (Receive ch receiver)) = do
  send_queue <- gets send_queue
  case ch `M.lookup` send_queue of
    Nothing -> do
      receive_queue <- gets receive_queue
      modify $ \s -> s { receive_queue = M.insertWith (++) ch [(receiver_id, receiver)] receive_queue }

    Just ((sender_id, msg) : others) -> do
      lift $ logProcMsg sender_id receiver_id ch msg
      queue <- gets queue
      modify $ \s -> s { queue      = queue ++ [(receiver_id, receiver msg)]
                       , send_queue = if null others
                                      then ch `M.delete` send_queue
                                      else M.insert ch others send_queue }

processStep pid (Free (Say str cont)) = do
  lift $ logProcSays pid str
  queue <- gets queue
  modify $ \s -> s { queue = queue ++ [(pid, cont)] }


-- | @system@ defines how the execution environment operates.
system :: SystemM ()
system = do
  queue <- gets queue
  case queue of
    [] -> do
      -- Check if there is a deadlock.
      receive_queue <- gets receive_queue
      send_queue <- gets send_queue
      if M.null receive_queue && M.null send_queue
        then lift $ logSymSuccess
        else lift $ logSymDeadlocks send_queue receive_queue

    _  -> do
      -- Randomly select the next process to run.
      pos <- lift $ randomRIO (0, length queue - 1)
      let (pid, p) = queue !! pos
      modify $ \s -> s { queue = (take pos queue) ++ (drop (pos+1) queue) }
      processStep pid p
      system


runSystem :: [(String, Process ())] -> IO ()
runSystem procs = do
  t <- getSystemTime
  setStdGen $ mkStdGen $ fromIntegral $ systemNanoseconds t
  let initial_state = SystemState { queue         = map (\(name, p) -> (PID name, p)) procs
                                  , receive_queue = M.empty
                                  , send_queue    = M.empty }
  runStateT system initial_state
  return ()
