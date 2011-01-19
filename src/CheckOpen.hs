module CheckOpen where

import System.Console.CmdArgs.Verbosity
import Control.Concurrent
import Control.Monad
import Data.List
import Network

import HostPortStatus

chunkSize :: Int
chunkSize = 1000

mkChunk :: Int -> [a] -> [[a]]
mkChunk size xs = unfoldr step xs where
    step [] = Nothing
    step ys = Just $ splitAt size ys

filterOpenPortsMany :: Int -> [HostPort] -> IO [HostPort]
filterOpenPortsMany timeout xs = do
    ys <- checkOpenPortsMany timeout xs
    return $ sort $ map (\(HostPortStatus (h, p, _)) -> HostPort (h, p)) $
             filter (\(HostPortStatus (_, _, s)) -> s == StatusOpen) ys

checkOpenPortsMany :: Int -> [HostPort] -> IO [HostPortStatus]
checkOpenPortsMany timeout xs = do
    let xss = mkChunk chunkSize xs
    yss <- forM xss $ checkOpenPorts timeout
    return $ concat yss

filterOpenPorts :: Int -> [HostPort] -> IO [HostPort]
filterOpenPorts timeout xs = do
    ys <- checkOpenPorts timeout xs
    return $ map (\(HostPortStatus (h, p, _)) -> HostPort (h, p)) $
             filter (\(HostPortStatus (_, _, s)) -> s == StatusOpen) ys

checkOpenPorts :: Int -> [HostPort] -> IO [HostPortStatus]
checkOpenPorts timeout xs = do
    mVar <- newEmptyMVar
    forM_ xs $ isOpenMVar timeout mVar
    ys <- forM xs $ \_ -> do
        x <- takeMVar mVar
        whenLoud $ putStrLn $ show x
        return x
    return $ sort ys

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
    (connectTo hostname port >> whenNormal (putStrLn msg) >> return StatusOpen)
    (\e -> whenNormal (putStrLn $ errMsg e) >> return StatusClose) where
    msg = show (HostPort (hostname, port)) ++ " connection succeeded."
    errMsg e = show (HostPort (hostname, port)) ++ " connection failed " ++ " : " ++ show e

