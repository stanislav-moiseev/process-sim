module SystemTypes where

import           Control.Monad
import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict                  as M

import           Process

-- | Process identifier.
newtype PID = PID { pid_name :: String }

data SystemState = SystemState
  { -- ^ The main queue of running processes used for process
    -- scheduling (for all operations except send/receive).
    queue         :: [(PID, Process ())]

  , -- ^ Processes waiting for a message to come over some
    -- communication channel).
    receive_queue :: M.Map Channel [(PID, Message -> Process ())]

  , -- ^ Messages sent previously by not received yet.
    send_queue    :: M.Map Channel [(PID, Message)]
  }


-- | The system monad that simulates the execution environment.
type SystemM = StateT SystemState IO
