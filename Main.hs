module Main where

import System.Environment
import System.Console.GetOpt
import System.Console.CmdArgs.Verbosity
import Data.Time.Clock.POSIX
import Network

import HostPortStatus
import CheckOpen

defaultFlags :: Flags
defaultFlags = Flags
    { flagHelp = False
    , flagVerbose = False
    , flagQuiet = False
    , flagTimeout = 300000
    , flagSubnet = "162.105.243.0/24"
    , flagPort = 808
    }

data Flags = Flags
    { flagHelp       :: Bool
    , flagVerbose    :: Bool
    , flagQuiet      :: Bool
    , flagTimeout    :: Int
    , flagSubnet     :: String
    , flagPort       :: Int
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
    , Option "s"   ["subnet"]
        (flip ReqArg "SUBNET" $ \s f -> f {flagSubnet = s}) 
        "Subnet range."
    , Option "p"   ["port"]
        (flip ReqArg "PORT" $ \s f -> f {flagPort = read s}) 
        "Ports to check whether is open."
    ]

main :: IO ()
main = do
    whenLoud $ putStrLn "Hello world."
    (flags, _, _) <- processOptions
    let timeout = flagTimeout flags
        subnet = flagSubnet flags
        port = PortNumber $ fromIntegral $ flagPort flags
    startTime <- getPOSIXTime
    filterOpenPorts timeout (parseHostPortIPv4 subnet port)>>= print
    whenLoud $ getPOSIXTime >>=
        \x -> putStrLn $ "Total time used: " ++ show (x-startTime)
    whenLoud $ putStrLn "Goodbye world."
    return ()

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


