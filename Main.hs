-- Copyright (c) 2012, Mark Wright.  All rights reserved.

{-# LANGUAGE DeriveDataTypeable,TemplateHaskell #-}

module Main where

import Control.Concurrent.STM
import Control.Monad (replicateM_)
import Control.Monad.Trans (lift, liftIO)
import Data.Data
import Data.Typeable
import Data.Binary
import Remote
import Remote.Process

data AddCountRequest = AddCountRequest
  { acrCount :: Int
  } deriving (Eq, Data, Show, Typeable)

data GetAndResetRequest = GetAndResetRequest
  {
  } deriving (Eq, Data, Show, Typeable)
             
data GetAndResetReply = GetAndResetReply
  { gsrCount :: Int
  } deriving (Eq, Data, Show, Typeable)

instance Binary AddCountRequest where
  get = genericGet
  put = genericPut

instance Binary GetAndResetRequest where
  get = genericGet
  put = genericPut

instance Binary GetAndResetReply where
  get = genericGet
  put = genericPut

type Count = TVar Int

handleAddCountRequest :: Count -> AddCountRequest -> ProcessM ()
handleAddCountRequest c msg = do
  say ("Received: " ++ show msg)
  liftIO $ atomically $ do 
    n <- readTVar c
    writeTVar c (acrCount msg + n)
  return ()

handleGetAndResetRequest :: ProcessId -> Count -> GetAndResetRequest -> ProcessM ()
handleGetAndResetRequest mPid c msg = do
  say ("Received: " ++ show msg)
  n <- liftIO $ atomically $ do
    n' <- readTVar c
    writeTVar c 0
    return n'
  send mPid GetAndResetReply { gsrCount = n }
  return ()

handleUnknown :: ProcessM ()
handleUnknown = do
  say ("Received unknown")
  return ()

newCount :: Int -> IO Count
newCount n = atomically $ newTVar n

slaveProcess :: ProcessId -> ProcessM ()
slaveProcess mPid = do
  c <- liftIO $ newCount 0
  slave mPid c

slave :: ProcessId -> Count -> ProcessM ()
slave mPid c =
  receiveWait [ match (\(msg@AddCountRequest{}) -> handleAddCountRequest c msg)
              , match (\(msg@GetAndResetRequest{}) -> handleGetAndResetRequest mPid c msg)
              , matchUnknown handleUnknown
              ]
  >> slave mPid c

$( remotable ['slaveProcess] )

initialProcess :: String -> ProcessM ()
initialProcess "MASTER" = do
  mPid <- getSelfPid
  slavePid <- spawnLocal $ slaveProcess mPid
  replicateM_ 3 $ send slavePid AddCountRequest { acrCount = 10 }
  return ()
initialProcess "SLAVE" = receiveWait []
initialProcess _ = error "Role must be SLAVE or MASTER"

main :: IO ()
main = remoteInit (Just "config") [Main.__remoteCallMetaData] initialProcess  
