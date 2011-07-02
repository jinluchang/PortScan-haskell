module Flags where

import System.Console.GetOpt
import System.Environment

import Verbosity
import HostPortStatus

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
    { flagHelp           :: Bool
    , flagVerbose        :: Bool
    , flagQuiet          :: Bool
    , flagTimeout        :: Int
    , flagSubnets        :: [String]
    , flagPorts          :: [Int]
    , flagWithAllPorts   :: Bool
    } deriving (Show, Eq, Ord)

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
