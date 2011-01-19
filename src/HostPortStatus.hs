module HostPortStatus where

import Network
import Data.List

newtype HostPort = HostPort (HostName, PortID)

newtype HostPortStatus = HostPortStatus (HostName, PortID, Status)

data Status = StatusOpen | StatusClose | StatusTimeout
    deriving (Show, Eq, Ord)


-- filter [HostPort] to only include HostName which open all ports
filterAllPorts :: Int -> [HostPort] -> [HostName]
filterAllPorts n hps = map head $ filter (\hs -> n == length hs) hss where
    hss = group $ map hostName hps
    hostName (HostPort (h, _)) = h

parseHostsPortsIPv4 :: [String] -> [PortID] -> [HostPort]
parseHostsPortsIPv4 subnets ports = sort $ concat
    [parseHostPortIPv4 subnet port | subnet <- subnets, port <-ports]

parseHostPortIPv4 :: String -> PortID -> [HostPort]
parseHostPortIPv4 subnet port = map (\x -> HostPort (x, port)) $
    parseSubnetIPv4 subnet

parseSubnetIPv4 :: String -> [HostName]
parseSubnetIPv4 subnet = if lenStr == []
    then [hostname]
    else subnetIPv4 hostname $ read . tail $ lenStr where
        (hostname, lenStr) = break (=='/') subnet

subnetIPv4 :: HostName -> Int -> [HostName]
subnetIPv4 start len = map hostNameFromIntegerIPv4 $
    intSubnetIPv4 (integerFromHostNameIPv4 start) len

intSubnetIPv4 :: Integer -> Int -> [Integer]
intSubnetIPv4 start len | len > 15 && 32 > len = [(start+1)..(end-1)] where
    end = start + 2^(32-len)

intSubnetIPv4 _ _ = error $ "Subnet is too large."

hostNameFromIntegerIPv4 :: Integer -> HostName
hostNameFromIntegerIPv4 num = concat $ intersperse "." $
    map show [ip1, ip2, ip3, ip4] where
    ip4 = num `mod` 256
    ip3 = num `div` 256 `mod` 256
    ip2 = num `div` 256 `div` 256 `mod` 256
    ip1 = num `div` 256 `div` 256 `div` 256 `mod` 256

integerFromHostNameIPv4 :: HostName -> Integer
integerFromHostNameIPv4 hostname = ((ip1*256+ip2)*256+ip3)*256+ip4 where
    [ip1s, ip2s, ip3s, ip4s] = splitWith '.' hostname
    ip1 = read ip1s
    ip2 = read ip2s
    ip3 = read ip3s
    ip4 = read ip4s

showPort :: PortID -> String
showPort port = case port of
    PortNumber x -> show x
    Service x -> x
    UnixSocket x -> x

comparePort :: PortID -> PortID -> Ordering
comparePort (PortNumber x1) (PortNumber x2) = compare x1' x2' where
    x1' = fromIntegral x1 :: Int
    x2' = fromIntegral x2 :: Int
comparePort (Service x1) (Service x2) = compare x1 x2
comparePort (UnixSocket x1) (UnixSocket x2) = compare x1 x2
comparePort (PortNumber _) _ = GT
comparePort _ (PortNumber _) = LT
comparePort (Service _) _ = GT
comparePort (UnixSocket _) _ = LT

instance Show HostPort where
    show (HostPort (hostname, port)) =
        hostname ++ ":" ++ showPort port

instance Show HostPortStatus where
    show (HostPortStatus (hostname, port, status)) =
        hostname ++ ":" ++ showPort port ++ " " ++ show status

instance Eq HostPort where
     HostPort (hostname1, port1) == HostPort (hostname2, port2) =
        ordA == EQ && ordB == EQ where
        ordA = compare (integerFromHostNameIPv4 hostname1)
                       (integerFromHostNameIPv4 hostname2)
        ordB = comparePort port1 port2

instance Eq HostPortStatus where
    (==) (HostPortStatus (hostname1, port1, status1))
         (HostPortStatus (hostname2, port2, status2)) =
         (HostPort (hostname1, port1), status1) ==
         (HostPort (hostname2, port2), status2)

instance Ord HostPort where
    compare (HostPort (hostname1, port1)) (HostPort (hostname2, port2)) = ord where
        ord = compare (ordA, ordB) (EQ, EQ)
        ordA = compare (integerFromHostNameIPv4 hostname1)
                       (integerFromHostNameIPv4 hostname2)
        ordB = comparePort port1 port2

instance Ord HostPortStatus where
    compare (HostPortStatus (hostname1, port1, status1))
            (HostPortStatus (hostname2, port2, status2)) =
            compare (HostPort (hostname1, port1), status1) (HostPort (hostname2, port2), status2)

splitWith :: (Eq a) => a -> [a] -> [[a]]
splitWith k xs = foldr step [[]] xs where
    step y (ys:rest) | y == k = []:ys:rest
                     | otherwise = (y:ys):rest
    step _ _ = error $ "splitWith : Impossible happens."


