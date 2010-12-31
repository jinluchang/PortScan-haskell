module Main where

import System.Environment
import System.Console.GetOpt
import System.Console.CmdArgs.Verbosity

data Flags = Flags
    { flagHelp    :: Bool
    , flagVerbose :: Bool
    , flagQuiet   :: Bool
    } deriving (Show, Eq, Ord)

defaultFlags :: Flags
defaultFlags = Flags
    { flagHelp = False
    , flagVerbose = False
    , flagQuiet = False
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
    ]

main :: IO ()
main = do
    allArgs <- getArgs
    let (actions, args, msgs) = getOpt Permute options allArgs
        flags = foldr ($) defaultFlags actions
    if flagQuiet flags then setVerbosity Quiet else return ()
    if flagVerbose flags then setVerbosity Loud else return ()
    whenLoud $ putStrLn "Hello World!"
    whenLoud $ putStrLn $ "Flags    : " ++ show flags
    whenLoud $ putStrLn $ "Args     : " ++ show args
    whenLoud $ putStrLn $ "Messages : " ++ show msgs
    return ()


