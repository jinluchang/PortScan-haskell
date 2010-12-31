module Main where

import System.Environment
import System.Console.GetOpt
import System.Console.CmdArgs.Verbosity
import Data.Time.Clock.POSIX
import Control.Concurrent
import Control.Monad
import Network

data Status = StatusOpen | StatusClose | StatusTimeout
    deriving (Show, Eq)

newtype HostPortStatus = HostPortStatus (HostName, PortID, Status)

defaultFlags :: Flags
defaultFlags = Flags
    { flagHelp = False
    , flagVerbose = False
    , flagQuiet = False
    , flagTimeout = 1000000
    }

hps :: [(HostName, PortID)]
hps = zip (map (\x -> "162.105.243." ++ show x) ([1..255] :: [Int])) (repeat (PortNumber 808)) 

main :: IO ()
main = do
    whenLoud $ putStrLn "Hello world."
    (flags, _, _) <- processOptions
    let timeout = flagTimeout flags
    startTime <- getPOSIXTime
    checkOpenPorts timeout hps >>= print
    whenLoud $ getPOSIXTime >>=
        \x -> putStrLn $ "Total time used: " ++ show (x-startTime)
    whenLoud $ putStrLn "Goodbye world."
    return ()

checkOpenPorts :: Int -> [(HostName, PortID)] -> IO [HostPortStatus]
checkOpenPorts timeout xs = do
    mVar <- newEmptyMVar
    forM_ xs $ isOpenMVar timeout mVar
    ys <- forM xs $ \_ -> do
        x <- takeMVar mVar
        whenLoud $ print x
        return x
    return $ filter (\(HostPortStatus (_, _, s)) -> s == StatusOpen) ys

isOpenMVar :: Int -> MVar HostPortStatus -> (HostName, PortID) -> IO ()
isOpenMVar timeout mVar (hostname, port) = create where
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
        s <- isOpen hostname port
        _ <- forkIO $ fill lock s
        return ()
    fill lock status = do
        isLock <- tryTakeMVar lock
        if isLock /= Nothing
            then putMVar mVar $ HostPortStatus (hostname, port, status)
            else return ()

isOpen :: HostName -> PortID -> IO Status
isOpen hostname port = catch
    (connectTo hostname port >> whenLoud (putStrLn msg) >> return StatusOpen)
    (\e -> whenLoud (putStrLn $ errMsg e) >> return StatusClose) where
    msg = hostname ++ ":" ++ showPort port ++ " connection succeeded."
    errMsg e = hostname ++ ":" ++ showPort port ++ " connection failed " ++ " : " ++ show e

processOptions :: IO (Flags, [String], [String])
processOptions = do
    allArgs <- getArgs
    let (actions, args, msgs) = getOpt Permute options allArgs
        flags = foldr ($) defaultFlags actions
    if flagQuiet flags then setVerbosity Quiet else return ()
    if flagVerbose flags then setVerbosity Loud else return ()
    whenLoud $ putStrLn $ "Flags    : " ++ show flags
    whenLoud $ putStrLn $ "Args     : " ++ show args
    whenLoud $ putStrLn $ "Messages : " ++ show msgs
    return (flags, args, msgs)

data Flags = Flags
    { flagHelp       :: Bool
    , flagVerbose    :: Bool
    , flagQuiet      :: Bool
    , flagTimeout    :: Int
    } deriving (Show, Eq, Ord)

options :: [OptDescr (Flags -> Flags)]
options =
    [ Option "h"   ["help"]
        (NoArg $ \f -> f {flagHelp = True})
        "Show help message."
    , Option "v"   ["verbose"]
        (NoArg $ \f -> f {flagVerbose = True})
        "Be verbose."
    , Option "q"   ["quiet"]
        (NoArg $ \f -> f {flagQuiet = True})
        "Be quiet."
    , Option "t"   ["timeout"]
        (flip ReqArg "TIMEOUT" $ \s f -> f {flagTimeout = read s}) 
        "Timeout Value"
    ]

showPort :: PortID -> String
showPort port = case port of
    PortNumber x -> show x
    Service x -> x
    UnixSocket x -> x

instance Show HostPortStatus where
    show (HostPortStatus (hostname, port, status)) =
        "< " ++ hostname ++ ":" ++ showPort port ++ " " ++ show status ++ " >"
