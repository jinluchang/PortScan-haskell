module HostPortStatus where

import Network

data Status = StatusOpen | StatusClose | StatusTimeout
    deriving (Show, Eq)

newtype HostPort = HostPort (HostName, PortID)

newtype HostPortStatus = HostPortStatus (HostName, PortID, Status)


hps :: [HostPort]
hps = zipWith (\h p -> HostPort (h, p)) (map (\x -> "162.105.243." ++ show x) ([1..255] :: [Int])) (repeat (PortNumber 808)) 

showPort :: PortID -> String
showPort port = case port of
    PortNumber x -> show x
    Service x -> x
    UnixSocket x -> x

instance Show HostPort where
    show (HostPort (hostname, port)) =
        "< " ++ hostname ++ ":" ++ showPort port ++ " >"

instance Show HostPortStatus where
    show (HostPortStatus (hostname, port, status)) =
        "< " ++ hostname ++ ":" ++ showPort port ++ " " ++ show status ++ " >"


