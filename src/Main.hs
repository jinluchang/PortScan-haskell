module Main where

import System.Exit
import System.Console.GetOpt
import Data.Time.Clock.POSIX
import Data.List
import Network

import Verbosity
import HostPortStatus
import CheckOpen
import Flags

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
