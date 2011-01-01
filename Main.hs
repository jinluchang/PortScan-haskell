module Main where

import System.Environment
import System.Console.GetOpt
import System.Console.CmdArgs.Verbosity
import Data.Time.Clock.POSIX
import Data.List
import Network

import HostPortStatus
import CheckOpen

defaultFlags :: Flags
defaultFlags = Flags
    { flagHelp = False
    , flagVerbose = False
    , flagQuiet = False
    , flagTimeout = 300000
    , flagSubnets = ["162.105.243.0/24"]
    , flagPorts = [808]
    }

data Flags = Flags
    { flagHelp       :: Bool
    , flagVerbose    :: Bool
    , flagQuiet      :: Bool
    , flagTimeout    :: Int
    , flagSubnets    :: [String]
    , flagPorts      :: [Int]
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
        (flip ReqArg "PORTS" $ \s f -> f {flagPorts = map read $ splitWith ',' s})
        "Ports to check whether is open, seperated by comma."
    ]

main :: IO ()
main = do
    whenLoud $ putStrLn "Hello world."
    (flags, _, _) <- processOptions
    let timeout = flagTimeout flags
        subnets = flagSubnets flags
        ports = map (PortNumber . fromIntegral) $ flagPorts flags
    startTime <- getPOSIXTime
    hostPorts <- filterOpenPortsMany timeout (parseHostsPortsIPv4 subnets ports)
    putStrLn $ showHostPorts hostPorts
    whenLoud $ getPOSIXTime >>=
        \x -> putStrLn $ "Total time used: " ++ show (x-startTime)
    whenLoud $ putStrLn "Goodbye world."
    return ()

showHostPorts :: [HostPort] -> String
showHostPorts xs =
    "Open HostPorts:\n" ++
    (concat $ intersperse "\n" $ map show xs)

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


