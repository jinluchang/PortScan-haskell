module Main where

import System.Environment
import System.Console.GetOpt
import System.Console.CmdArgs.Verbosity
import Data.Time.Clock.POSIX
import Control.Concurrent
import Control.Monad
import Network

data Status = StatusOpen | StatusClose | StatusTimeout
    deriving (Show)

newtype HostPortStatus = HostPortStatus (HostName, PortID, Status)

defaultFlags :: Flags
defaultFlags = Flags
    { flagHelp = False
    , flagVerbose = False
    , flagQuiet = False
    , flagTimeout = 1000000
    }

hps :: [(HostName, PortID)]
hps =
    [ ("162.105.243.37", PortNumber 809)
    , ("162.105.243.37", PortNumber 809)
    , ("162.105.243.37", PortNumber 808)
    , ("162.105.243.37", PortNumber 808)
    , ("162.105.243.37", PortNumber 809)
    , ("162.105.243.37", PortNumber 809)
    , ("162.105.243.38", PortNumber 809)
    , ("162.105.243.39", PortNumber 809)
    , ("162.105.243.40", PortNumber 809)
    , ("162.105.243.41", PortNumber 809)
    , ("162.105.243.42", PortNumber 809)
    ]

main :: IO ()
main = do
    whenLoud $ putStrLn "Hello world."
    whenLoud $ putStrLn "Goodbye world."
    (flags, _, _) <- processOptions
    let timeout = flagTimeout flags
    startTime <- getPOSIXTime
    mVar <- newEmptyMVar
    isOpenMVar timeout mVar ("162.105.243.37", PortNumber 809)
    isOpenMVar timeout mVar ("162.105.243.37", PortNumber 808)
    isOpenMVar timeout mVar ("162.105.243.32", PortNumber 808)
    whenLoud $ getPOSIXTime >>= \x -> print (x-startTime)
    takeMVar mVar >>= print
    takeMVar mVar >>= print
    takeMVar mVar >>= print
    checkOpenPorts timeout hps >>= print
    whenLoud $ getPOSIXTime >>= \x -> print (x-startTime)
    return ()

checkOpenPorts :: Int -> [(HostName, PortID)] -> IO [HostPortStatus]
checkOpenPorts timeout xs = do
    mVar <- newEmptyMVar
    forM_ xs $ isOpenMVar timeout mVar
    forM xs $ \_ -> do
        x <- takeMVar mVar
        whenLoud $ print x
        return x

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
        "< " ++ show hostname ++ ":" ++ showPort port ++ " " ++ show status ++ " >"
