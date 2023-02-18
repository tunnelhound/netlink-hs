module Main where

import Data.Foldable (forM_)
import System.Network.Netlink

main :: IO ()
main = withNlSock nlFamilyRoute $ \rt -> do { rts <- rtnlRouteGetAll rt; forM_ rts showRoute }
