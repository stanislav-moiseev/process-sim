module Operational.SystemLog where

import           Control.Monad
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict                  as M
import           System.Console.ANSI              as ANSI
import           Text.Printf

import           Operational.Process
import           Operational.SystemTypes

logProcTerm pid = do
  p_name <- getProcName pid
  lift $ printf "[%s] terminated\n" p_name

logProcSays pid str = do
  p_name <- getProcName pid
  lift $ do
    printf "[%s] says" p_name
    ANSI.setSGR [SetColor Foreground Vivid Blue]
    printf " %s\n" (show str)
    setSGR [Reset]

logProcMsg sender_pid receiver_pid ch msg = do
  sender_name <- getProcName sender_pid
  receiver_name <- getProcName receiver_pid
  lift $ printf "from [%s] to [%s] channel [%s] message %s\n" sender_name receiver_name (ch_name ch) (show msg)

logSymSuccess :: SystemM ()
logSymSuccess = lift $ do
  printf "\nSimulation finished. "
  ANSI.setSGR [SetColor Foreground Vivid Green]
  printf "All processes terminated successfully.\n"
  setSGR [Reset]

logSymDeadlocks = do
  receive_queue <- gets receive_queue
  send_queue <- gets send_queue
  parents <- gets parents
  
  lift $ printf "\nSimulation finished. "

  lift $ ANSI.setSGR [SetColor Foreground Vivid Red]
  lift $ printf "A deadlock detected.\n"

  lift $ ANSI.setSGR [SetColor Foreground Vivid Yellow]
  when (not $ M.null send_queue) $ do
    lift $ printf "\nList of processes waiting to send a message:\n"
    forM_ (M.toList send_queue) $ \(ch, senders) ->
      forM_ senders $ \(sender_pid, msg, _) -> do
        sender_name <- getProcName sender_pid
        lift $ printf "  | from process %-6s channel %-8s message %s\n"
                      ("[" ++ sender_name ++ "]")
                      ("[" ++ ch_name ch ++ "]")
                      (show msg)
               
  when (not $ M.null receive_queue) $ do
    lift $ printf "\nList of processes waiting to receive a message:\n"
    forM_ (M.toList receive_queue) $ \(ch, receivers) ->
      forM_ receivers $ \(receiver_pid, _) -> do
        receiver_name <- getProcName receiver_pid
        lift $ printf "  | process %-6s channel %-8s\n"
                      ("[" ++ receiver_name ++ "]")
                      ("[" ++ ch_name ch ++ "]")

  when (not $ M.null parents) $ do
    lift $ printf "\nList of forked processes waiting for their subprocesses to terminate :\n"
    forM_ (M.toList parents) $ \(parent_pid, ParentProcessStatus counter _ _) -> do
      parent_name <- getProcName parent_pid
      lift $ printf "  | process %-6s waiting for %d subprocess(es)\n"
                      ("[" ++ parent_name ++ "]")
                      counter

  lift $ setSGR [Reset]
