module Main where

import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.Time.Clock.POSIX
import Data.List
import Network

import Verbosity
import HostPortStatus
import CheckOpen

defaultFlags :: Flags
defaultFlags = Flags
    { flagHelp = False
    , flagVerbose = False
    , flagQuiet = False
    , flagTimeout = 300000
    , flagSubnets = ["127.0.0.1"]
    , flagPorts = [1..1000]
    , flagWithAllPorts = False
    }

data Flags = Flags
    { flagHelp           :: Bool
    , flagVerbose        :: Bool
    , flagQuiet          :: Bool
    , flagTimeout        :: Int
    , flagSubnets        :: [String]
    , flagPorts          :: [Int]
    , flagWithAllPorts   :: Bool
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
        "Timeout value."
    , Option "s"   ["subnets"]
        (flip ReqArg "SUBNETS" $ \s f -> f {flagSubnets = splitWith ',' s})
        "Subnet range, seperated by comma."
    , Option "p"   ["ports"]
        (flip ReqArg "PORTS" $ \s f -> f {flagPorts = readPorts $ splitWith ',' s})
        "Ports to check whether is open, seperated by comma."
    , Option "a"   ["with-all-ports"]
        (NoArg $ \f -> f {flagWithAllPorts = True})
        "Only show results that opens all ports specified."
    ] where
        readPorts ps = concat $ map readP ps
        readP str = if end == []
            then [read start]
            else [read start .. (read . tail) end] where (start, end) = break (=='-') str

main :: IO ()
main = do
    whenLoud $ putStrLn "Hello world."
    (flags, _, _) <- processOptions
    let timeout = flagTimeout flags
        subnets = flagSubnets flags
        ports = map (PortNumber . fromIntegral) $ flagPorts flags
    if not $ flagHelp flags
        then return ()
        else do
            putStrLn $ flip usageInfo options $
                "Usage: port-scan [options]\n" ++
                "Scan open ports on specified subnets and ports.\n"
            exitSuccess
    startTime <- getPOSIXTime
    hostPorts <- filterOpenPortsMany timeout (parseHostsPortsIPv4 subnets ports)
    if flagWithAllPorts flags
        then putStrLn $ showHostNames $ filterAllPorts (length ports) hostPorts
        else putStrLn $ showHostPorts hostPorts
    whenLoud $ getPOSIXTime >>=
        \x -> putStrLn $ "Total time used: " ++ show (x-startTime)
    whenLoud $ putStrLn "Goodbye world."
    return ()

showHostPorts :: [HostPort] -> String
showHostPorts xs =
    "Open HostPorts:\n" ++
    (concat $ intersperse "\n" $ map show xs)

showHostNames:: [HostName] -> String
showHostNames xs =
    "Open HostNames:\n" ++
    (concat $ intersperse "\n" xs)

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


