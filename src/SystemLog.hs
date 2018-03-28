module SystemLog where

import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Trans.Class
import qualified Data.Map.Strict           as M
import           System.Console.ANSI       as ANSI
import           Text.Printf

import           Process
import           SystemTypes

logProcTerm pid = do
  printf "[%s] terminated\n" (pid_name pid)

logProcSays pid str = do
  printf "[%s] says" (pid_name pid)
  ANSI.setSGR [SetColor Foreground Vivid Blue]
  printf " %s\n" (show str)
  setSGR [Reset]

logProcMsg sender_id receiver_id ch msg = do
  printf "from [%s] to [%s] channel [%s] message %s\n" (pid_name sender_id) (pid_name receiver_id) (ch_name ch) (show msg)

logSymSuccess = do
  printf "\nSimulation finished. "
  ANSI.setSGR [SetColor Foreground Vivid Green]
  printf "All processes terminated successfully.\n"
  setSGR [Reset]

logSymDeadlocks send_queue receive_queue = do
  printf "\nSimulation finished. "

  ANSI.setSGR [SetColor Foreground Vivid Red]
  printf "A deadlock detected.\n"

  ANSI.setSGR [SetColor Foreground Vivid Yellow]
  when (not $ M.null send_queue) $ do
    printf "\nList of undelivered messages:\n"
    forM_ (M.toList send_queue) $ \(ch, senders) ->
      forM_ senders $ \(sender_id, msg) -> printf "  | from process %-6s channel %-8s message %s\n"
                                                  ("[" ++ pid_name sender_id ++ "]")
                                                  ("[" ++ ch_name ch ++ "]")
                                                  (show msg)
  when (not $ M.null receive_queue) $ do
    printf "\nList of processes waiting to receive a message:\n"
    forM_ (M.toList receive_queue) $ \(ch, receivers) ->
      forM_ receivers $ \(receiver_id, _) -> printf "  | process %-6s channel %-8s\n"
                                                    ("[" ++ pid_name receiver_id ++ "]")
                                                    ("[" ++ ch_name ch ++ "]")
  setSGR [Reset]
