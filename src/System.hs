module System (runSystem, ProcessName) where

import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.List
import qualified Data.Map.Strict                  as M
import           Data.Maybe

import           Data.Time.Clock.System
import           System.Random

import           Process
import           SystemLog
import           SystemTypes

processStep :: PID -> Process () -> SystemM ()

processStep pid (Pure msg) = processStep pid (Free (Exit (show msg)))

processStep pid (Free (Exit msg)) = do
  -- Check if this process is a child process of another process.
  info <- gets info
  let p_info = fromJust $ pid `M.lookup` info
  case parent p_info of
    Nothing         -> do -- We log only primary processes when they
                          -- terminate, not subprocesses.
                          logProcTerm pid

    Just parent_pid -> do
      -- Decrease parent's counter.
      parents <- gets parents
      let ParentProcessStatus counter msgs parent = fromJust $ parent_pid `M.lookup` parents

      if counter >= 2
        then do
          -- Decrease the counter.
          modify $ \s -> s { parents = M.insert parent_pid (ParentProcessStatus (counter-1) ((pid, msg) : msgs) parent) parents }

        else do
          modify $ \s -> s { parents = M.delete parent_pid parents }
          -- Wake the parent up.
          queue <- gets queue
          -- We sort messages to get them in the right order. We
          -- assume here that PIDs of the child processes were
          -- assigned in ascending order.
          let msgs' = map snd $ sort $ (pid, msg) : msgs
          modify $ \s -> s { queue = queue ++ [(parent_pid, parent msgs')] }


processStep sender_pid (Free (Send ch msg sender)) = do
  receive_queue <- gets receive_queue
  case ch `M.lookup` receive_queue of
    Nothing -> do
      queue <- gets queue
      send_queue <- gets send_queue
      modify $ \s -> s { send_queue = M.insertWith (flip (++)) ch [(sender_pid, msg)] send_queue
                       , queue = queue ++ [(sender_pid, sender)] }

    Just ((receiver_pid, receiver) : others) -> do
      logProcMsg sender_pid receiver_pid ch msg
      queue <- gets queue
      modify $ \s -> s { queue         = queue ++ [(receiver_pid, receiver msg), (sender_pid, sender)]
                       , receive_queue = if null others
                                         then ch `M.delete` receive_queue
                                         else M.insert ch others receive_queue }

processStep receiver_pid (Free (Receive ch receiver)) = do
  send_queue <- gets send_queue
  case ch `M.lookup` send_queue of
    Nothing -> do
      receive_queue <- gets receive_queue
      modify $ \s -> s { receive_queue = M.insertWith (++) ch [(receiver_pid, receiver)] receive_queue }

    Just ((sender_pid, msg) : others) -> do
      logProcMsg sender_pid receiver_pid ch msg
      queue <- gets queue
      modify $ \s -> s { queue      = queue ++ [(receiver_pid, receiver msg)]
                       , send_queue = if null others
                                      then ch `M.delete` send_queue
                                      else M.insert ch others send_queue }

processStep parent_pid (Free (Fork plist parent_cont))
  | null plist = do
      queue <- gets queue
      modify $ \s -> s { queue = queue ++ [(parent_pid, parent_cont [])] }

  | otherwise = do
      -- Create child processes.
      forM_ (zip [0..] plist) $ \(sub_idx, child_proc) -> do
        SystemState next_pid info queue _ _ _ <- get
        -- Allocate new PID.
        let child_pid@(PID child_pid') = next_pid
        modify $ \s -> s { next_pid = PID (child_pid' + 1) }
        -- Create a child process.
        parent_name <- getProcName parent_pid
        let child_name = parent_name ++ "/" ++ (show sub_idx)
        modify $ \s -> s { info = M.insert child_pid (ProcessInfo child_name (Just parent_pid)) info }
        -- Schedule the child process.
        modify $ \s -> s { queue = queue ++ [(child_pid, child_proc)] }

      -- Freeze the parent process and put it to the list of processes
      -- waiting for their children to terminate.
      parents <- gets parents
      modify $ \s -> s { parents = M.insert parent_pid (ParentProcessStatus (length plist) [] parent_cont) parents }


processStep pid (Free (Say str cont)) = do
  logProcSays pid str
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
      parents <- gets parents
      if M.null receive_queue && M.null send_queue && M.null parents
        then logSymSuccess
        else logSymDeadlocks

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
  let initial_state = SystemState
        { next_pid      = PID $ length procs
        , info          = M.fromList [(PID pid, ProcessInfo name Nothing)
                                     | (pid, name) <- zip [0..] (map fst procs)]
        , queue         = [(PID pid, proc)
                          | (pid, proc) <- zip [0..] (map snd procs)]
        , receive_queue = M.empty
        , send_queue    = M.empty
        , parents       = M.empty
        }
  runStateT system initial_state
  return ()
