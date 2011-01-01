module CheckOpen where

import System.Console.CmdArgs.Verbosity
import Control.Concurrent
import Control.Monad
import Network

import HostPortStatus

checkOpenPorts :: Int -> [HostPort] -> IO [HostPortStatus]
checkOpenPorts timeout xs = do
    mVar <- newEmptyMVar
    forM_ xs $ isOpenMVar timeout mVar
    ys <- forM xs $ \_ -> do
        x <- takeMVar mVar
        whenLoud $ print x
        return x
    return $ filter (\(HostPortStatus (_, _, s)) -> s == StatusOpen) ys

isOpenMVar :: Int -> MVar HostPortStatus -> HostPort -> IO ()
isOpenMVar timeout mVar (HostPort (hostname, port)) = create where
    create = do
        lock <- newMVar True
        threadId <- forkIO $ check lock
        _ <- forkIO $ daemon lock threadId
        return ()
    daemon lock threadId = do
        threadDelay timeout
        killThread threadId
        fill lock StatusTimeout
    check lock = do
        s <- isOpen $ HostPort (hostname, port)
        _ <- forkIO $ fill lock s
        return ()
    fill lock status = do
        isLock <- tryTakeMVar lock
        if isLock /= Nothing
            then putMVar mVar $ HostPortStatus (hostname, port, status)
            else return ()

isOpen :: HostPort -> IO Status
isOpen (HostPort (hostname, port)) = catch
    (connectTo hostname port >> whenLoud (putStrLn msg) >> return StatusOpen)
    (\e -> whenLoud (putStrLn $ errMsg e) >> return StatusClose) where
    msg = hostname ++ ":" ++ showPort port ++ " connection succeeded."
    errMsg e = hostname ++ ":" ++ showPort port ++ " connection failed " ++ " : " ++ show e

