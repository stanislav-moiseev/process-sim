module SystemTypes where

import           Control.Monad
import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict                  as M
import           Text.Printf

import           Process

-- | Internal process identifier.
newtype PID = PID Int
  deriving (Eq, Ord)

-- | External process name used in logging.
type ProcessName = String

data ProcessInfo = ProcessInfo
  { name   :: ProcessName
    -- If the current process is a fork of some parent process, then
    -- @parent@ denotes the PID of the parent process; otherwise, it
    -- is set to @Nothing@.
  , parent :: Maybe PID
  }


data SystemState = SystemState
  { next_pid      :: PID

  , info          :: M.Map PID ProcessInfo

  , -- ^ The main queue of running processes used for process
    -- scheduling (for all operations except send/receive).
    queue         :: [(PID, Process ())]

  , -- ^ Processes waiting for a message to come over some
    -- communication channel).
    receive_queue :: M.Map Channel [(PID, Message -> Process ())]

  , -- ^ Messages sent previously by not received yet.
    -- @PID@ refers to sender's PID.
    send_queue    :: M.Map Channel [(PID, Message)]

  , -- ^ Continuations for parent processes that have forked into
    -- subprocesses and are waiting for the subprocess to finish.
    --
    -- > parents pid = (counter, cont),
    --
    -- where @counter@ denotes the number of subprocesses being waited
    -- for, and @cont@ is the continuation of the parent process.
    --
    -- When a sbprocess finishes, the parent's counter decreases by 1;
    -- when the counter comes to zero, the parent process wakes up.
    parents       :: M.Map PID (Int, Process ())
  }


-- | The system monad that simulates the execution environment.
type SystemM = StateT SystemState IO

getProcName :: PID -> SystemM ProcessName
getProcName pid@(PID pid') = do
  info <- gets info
  case pid `M.lookup` info of
    Nothing     -> error $ printf "getProcName: unknown PID %d" pid'
    Just p_info -> return $ name $ p_info
