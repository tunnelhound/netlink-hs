{-# OPTIONS_GHC -fwarn-unused-imports #-}

module System.Network.Netlink.Route where

import           System.Network.Netlink.Types

import           Control.Monad (unless, forM_, filterM)
import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Bits
import           Data.Foldable (toList)
import qualified Data.Text as T
import           Data.Traversable (forM)
import           Data.Word (Word8, Word32)

import           Foreign.C.String (CString, peekCString, withCString)
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr
import           Foreign.Storable

import qualified Network.Socket as Network

#include <net/if.h>

#include <netlink/netlink.h>
#include <netlink/route/link.h>

nlFamilyRoute :: NlFamily
nlFamilyRoute = NlFamily (#const NETLINK_ROUTE)

data LinkAddressDetails
  = LinkAddressDetails
  { ladAddr :: Maybe LinkAddr
  -- ^ Local address
  , ladBroadcast :: Maybe LinkAddr
  -- ^ Broadcast addr, if exists
  , ladMulticast :: Maybe LinkAddr
  -- ^ Multicast addr, if exists
  , ladPeer :: Maybe LinkAddr
    -- ^ Peer addr, if exists
  , ladPrefixLen :: Word8
  } deriving (Show, Eq)

readLinkAddressDetails :: MonadIO m => RtnlAddr -> m LinkAddressDetails
readLinkAddressDetails addr =
  liftIO $
  LinkAddressDetails <$> nlGet addrLocal addr
                     <*> nlGet addrBroadcast addr
                     <*> nlGet addrMulticast addr
                     <*> nlGet addrPeer addr
                     <*> nlGet addrPrefixLen addr

rtnlAddrFromDetails :: MonadIO m => InterfaceIndex -> LinkAddressDetails -> m RtnlAddr
rtnlAddrFromDetails linkIndex lad =
  liftIO $ do
    addr <- rtnlAddrAlloc
    nlSet ifIndex       addr linkIndex
    nlSet addrLocal     addr (ladAddr lad)
    nlSet addrBroadcast addr (ladBroadcast lad)
    nlSet addrMulticast addr (ladMulticast lad)
    nlSet addrPeer      addr (ladPeer lad)
    nlSet addrPrefixLen addr (ladPrefixLen lad)
    pure addr

-- * Rtnl link

-- | Represents a link message for getting, adding, or modifying links
newtype RtnlLink = RtnlLink (ForeignPtr RtnlLink)
  deriving (Show, Eq, Ord)
  deriving HasNlObject via NlObject RtnlLink

foreign import ccall unsafe "rtnl_link_alloc" c_rtnl_link_alloc :: IO (Ptr RtnlLink)

-- | Allocate a new 'RtnlLink' object for modifications
rtnlLinkAlloc :: MonadIO m => m RtnlLink
rtnlLinkAlloc = liftIO $ do
  fromNlPtr =<< c_rtnl_link_alloc

foreign import ccall unsafe "rtnl_link_change" c_rtnl_link_change
    :: NlSock -> Ptr RtnlLink -> Ptr RtnlLink -> CInt -> IO NlError

rtnlLinkChange :: MonadIO m => NlSock -> RtnlLink -> RtnlLink -> m ()
rtnlLinkChange nlSock origLink newLink =
  liftIO $
  withNlObject origLink $ \origLinkPtr ->
  withNlObject newLink $ \newLinkPtr ->
  translateNlError (c_rtnl_link_change nlSock origLinkPtr newLinkPtr (#const NLM_F_REPLACE))

foreign import ccall unsafe "rtnl_link_get_kernel" c_rtnl_link_get_kernel
    :: NlSock -> InterfaceIndex -> Ptr CChar -> Ptr (Ptr RtnlLink) -> IO NlError

data LinkIdentifier
  = LinkByIndex InterfaceIndex
  | LinkByName T.Text
  deriving (Show)

rtnlLinkGet :: MonadIO m => NlSock -> LinkIdentifier -> m RtnlLink
rtnlLinkGet sock linkId =
  liftIO $
  withIfName $ \namePtr ->
  alloca $ \linkPtrPtr -> do
    translateNlError (c_rtnl_link_get_kernel sock realIndex namePtr linkPtrPtr)
    fromNlPtr =<< peek linkPtrPtr

  where
    (realIndex, withIfName) =
      case linkId of
        LinkByIndex idx -> (idx, \withName -> withName nullPtr)
        LinkByName realName ->
          (0, withCString (T.unpack realName))

foreign import ccall unsafe "rtnl_link_set_ifindex" c_rtnl_link_set_ifindex :: Ptr RtnlLink -> InterfaceIndex -> IO ()
foreign import ccall unsafe "rtnl_link_get_ifindex" c_rtnl_link_get_ifindex :: Ptr RtnlLink -> IO InterfaceIndex

class HasIfIndex what where
  ifIndex :: NlAttribute what InterfaceIndex

instance HasIfIndex RtnlLink where
  ifIndex = NlAttribute c_rtnl_link_set_ifindex c_rtnl_link_get_ifindex

foreign import ccall unsafe "rtnl_link_set_name" c_rtnl_link_set_name :: Ptr RtnlLink -> CString -> IO ()
foreign import ccall unsafe "rtnl_link_get_name" c_rtnl_link_get_name :: Ptr RtnlLink -> IO CString

ifName :: NlAttribute RtnlLink T.Text
ifName = NlAttribute (\ptr nm ->
                          withCString (T.unpack nm) $ \nmPtr ->
                            c_rtnl_link_set_name ptr nmPtr)
                     (\ptr -> do
                        nmPtr <- c_rtnl_link_get_name ptr
                        T.pack <$> peekCString nmPtr)

foreign import ccall unsafe "rtnl_link_set_flags" c_rtnl_link_set_flags :: Ptr RtnlLink -> InterfaceFlags -> IO ()
foreign import ccall unsafe "rtnl_link_unset_flags" c_rtnl_link_unset_flags :: Ptr RtnlLink -> InterfaceFlags -> IO ()
foreign import ccall unsafe "rtnl_link_get_flags" c_rtnl_link_get_flags :: Ptr RtnlLink -> IO InterfaceFlags

newtype InterfaceFlags = InterfaceFlags CInt
  deriving (Show, Eq, Ord)
  deriving newtype (Storable, Bits)
  deriving (Semigroup, Monoid) via BitFlags InterfaceFlags

iffUpFlag, iffPointToPointFlag :: InterfaceFlags
iffUpFlag = InterfaceFlags (#const IFF_UP)
iffPointToPointFlag = InterfaceFlags (#const IFF_POINTOPOINT)

ifFlags :: NlAttribute RtnlLink InterfaceFlags
ifFlags = NlAttribute (\link newFlags -> do
                         c_rtnl_link_unset_flags link (complement zeroBits) -- Unset all flags
                         c_rtnl_link_set_flags link newFlags)
                      c_rtnl_link_get_flags

-- * RtnlAddr

newtype RtnlAddr = RtnlAddr (ForeignPtr RtnlAddr)
    deriving (Show, Eq, Ord)
    deriving HasNlObject via NlObject RtnlAddr

rtnlAddrGetAll :: MonadIO m => m (NlCache RtnlAddr)
rtnlAddrGetAll = liftIO $
                 withNlSock nlFamilyRoute $ \sock ->
                 rtnlAddrGetAllFromSock sock

foreign import ccall unsafe "rtnl_addr_alloc_cache" c_rtnl_addr_alloc_cache :: NlSock -> Ptr (Ptr (NlCache RtnlAddr)) -> IO NlError
rtnlAddrGetAllFromSock :: MonadIO m => NlSock -> m (NlCache RtnlAddr)
rtnlAddrGetAllFromSock sock =
  liftIO $ alloca $ \resultPtr -> do
    translateNlError (c_rtnl_addr_alloc_cache sock resultPtr)
    cachePtr <- peek resultPtr
    NlCache <$> newForeignPtr c_nl_cache_free cachePtr <*> pure id

foreign import ccall unsafe "rtnl_addr_alloc" c_rtnl_addr_alloc :: IO (Ptr RtnlAddr)

rtnlAddrAlloc :: MonadIO m => m RtnlAddr
rtnlAddrAlloc = liftIO $
                fromNlPtr =<< c_rtnl_addr_alloc

foreign import ccall unsafe "rtnl_addr_add" c_rtnl_addr_add :: NlSock -> Ptr RtnlAddr -> CInt -> IO NlError
foreign import ccall unsafe "rtnl_addr_delete" c_rtnl_addr_delete :: NlSock -> Ptr RtnlAddr -> CInt -> IO NlError

rtnlAddrAdd :: MonadIO m => NlSock -> RtnlAddr -> m ()
rtnlAddrAdd nl addr =
  liftIO $ withNlObject addr $ \addrPtr ->
  translateNlError (c_rtnl_addr_add nl addrPtr 0)

rtnlAddrDelete :: MonadIO m => NlSock -> RtnlAddr -> m ()
rtnlAddrDelete nl addr =
  liftIO $ withNlObject addr $ \addrPtr ->
  translateNlError (c_rtnl_addr_delete nl addrPtr 0)

-- attributes

foreign import ccall unsafe "rtnl_addr_set_ifindex" c_rtnl_addr_set_ifindex :: Ptr RtnlAddr -> InterfaceIndex -> IO ()
foreign import ccall unsafe "rtnl_addr_get_ifindex" c_rtnl_addr_get_ifindex :: Ptr RtnlAddr -> IO InterfaceIndex

foreign import ccall unsafe "rtnl_addr_set_link" c_rtnl_addr_set_link :: Ptr RtnlAddr -> Ptr RtnlLink -> IO ()
foreign import ccall unsafe "rtnl_addr_get_link" c_rtnl_addr_get_link :: Ptr RtnlAddr -> IO (Ptr RtnlLink)

foreign import ccall unsafe "rtnl_addr_set_family" c_rtnl_addr_set_family :: Ptr RtnlAddr -> CInt -> IO ()
foreign import ccall unsafe "rtnl_addr_get_family" c_rtnl_addr_get_family :: Ptr RtnlAddr -> IO CInt

foreign import ccall unsafe "rtnl_addr_set_local" c_rtnl_addr_set_local :: Ptr RtnlAddr -> Ptr NlAddr -> IO ()
foreign import ccall unsafe "rtnl_addr_get_local" c_rtnl_addr_get_local :: Ptr RtnlAddr -> IO (Ptr NlAddr)

foreign import ccall unsafe "rtnl_addr_set_peer" c_rtnl_addr_set_peer :: Ptr RtnlAddr -> Ptr NlAddr -> IO ()
foreign import ccall unsafe "rtnl_addr_get_peer" c_rtnl_addr_get_peer :: Ptr RtnlAddr -> IO (Ptr NlAddr)

foreign import ccall unsafe "rtnl_addr_set_broadcast" c_rtnl_addr_set_broadcast :: Ptr RtnlAddr -> Ptr NlAddr -> IO ()
foreign import ccall unsafe "rtnl_addr_get_broadcast" c_rtnl_addr_get_broadcast :: Ptr RtnlAddr -> IO (Ptr NlAddr)

foreign import ccall unsafe "rtnl_addr_set_multicast" c_rtnl_addr_set_multicast :: Ptr RtnlAddr -> Ptr NlAddr -> IO ()
foreign import ccall unsafe "rtnl_addr_get_multicast" c_rtnl_addr_get_multicast :: Ptr RtnlAddr -> IO (Ptr NlAddr)

foreign import ccall unsafe "rtnl_addr_set_prefixlen" c_rtnl_addr_set_prefixlen :: Ptr RtnlAddr -> CInt -> IO ()
foreign import ccall unsafe "rtnl_addr_get_prefixlen" c_rtnl_addr_get_prefixlen :: Ptr RtnlAddr -> IO CInt

instance HasIfIndex RtnlAddr where
  ifIndex = NlAttribute c_rtnl_addr_set_ifindex c_rtnl_addr_get_ifindex

addrLink :: NlAttribute RtnlAddr RtnlLink
addrLink = NlAttribute (\p link ->
                            withNlObject link $ \linkPtr ->
                              c_rtnl_addr_set_link p linkPtr)
                       ((fromNlPtr =<<) . c_rtnl_addr_get_link)

addrFamily :: NlAttribute RtnlAddr Network.Family
addrFamily = NlAttribute (\p -> c_rtnl_addr_set_family p . Network.packFamily)
                         (fmap Network.unpackFamily . c_rtnl_addr_get_family)

addrLocal :: NlAttribute RtnlAddr (Maybe LinkAddr)
addrLocal = nlSockAddrAttribute c_rtnl_addr_set_local c_rtnl_addr_get_local

addrPeer :: NlAttribute RtnlAddr (Maybe LinkAddr)
addrPeer = nlSockAddrAttribute c_rtnl_addr_set_peer c_rtnl_addr_get_peer

addrBroadcast :: NlAttribute RtnlAddr (Maybe LinkAddr)
addrBroadcast = nlSockAddrAttribute c_rtnl_addr_set_broadcast c_rtnl_addr_get_broadcast

addrMulticast :: NlAttribute RtnlAddr (Maybe LinkAddr)
addrMulticast = nlSockAddrAttribute c_rtnl_addr_set_multicast c_rtnl_addr_get_multicast

addrPrefixLen :: NlAttribute RtnlAddr Word8
addrPrefixLen = NlAttribute (\a -> c_rtnl_addr_set_prefixlen a . fromIntegral)
                            (fmap fromIntegral . c_rtnl_addr_get_prefixlen)


-- * Utilities

changeLink :: NlSock -> RtnlLink -> (RtnlLink -> IO ()) -> IO ()
changeLink nl oldLink mkNewLink = do
  link' <- rtnlLinkAlloc
  nlCopy ifIndex link' oldLink
  mkNewLink link'
  rtnlLinkChange nl oldLink link'

syncLinkAddresses :: NlSock -> RtnlLink -> [ LinkAddressDetails ] -> IO ()
syncLinkAddresses nl link newAddrs = do
  linkIndex <- nlGet ifIndex link

  currentAddresses <- rtnlAddrGetAll
  -- Filter by the link index
  ifAddresses <- filterM (\addr -> do
                            addrIface <- nlGet ifIndex addr
                            pure (addrIface == linkIndex))
                         (toList currentAddresses)

  curAddrDetails <- mapM (\iAddr -> (,) iAddr <$> readLinkAddressDetails iAddr) ifAddresses

  let curAddrs = map snd curAddrDetails

      deletedAddresses = map fst $
                         filter (\(_, addr) ->
                                   case ladAddr addr of
                                     Nothing -> True
                                     Just oldAddr ->
                                       any (not . maybe False (==oldAddr) . ladAddr) newAddrs) curAddrDetails

  forM_ newAddrs $ \newAddr ->
    -- Check if this address exists already
    unless (newAddr `elem` curAddrs) $ -- When it doesn't exist exactly as stated add it
      rtnlAddrAdd nl =<<
      rtnlAddrFromDetails linkIndex newAddr

  forM_ deletedAddresses $ \deleted -> do
    rtnlAddrDelete nl deleted

-- * Routes

-- | A route message
newtype RtnlRoute = RtnlRoute (ForeignPtr RtnlRoute)
    deriving (Show, Eq, Ord)
    deriving HasNlObject via NlObject RtnlRoute

-- | Nexthop object
newtype RtnlNexthop = RtnlNexthop (ForeignPtr RtnlNexthop)
    deriving (Show, Eq, Ord)
    deriving HasNlObject via NlUnownedObject RtnlNexthop

foreign import ccall unsafe "rtnl_route_alloc_cache" c_rtnl_route_alloc_cache :: NlSock -> Ptr (Ptr (NlCache RtnlRoute)) -> IO NlError

rtnlRouteGetAll :: MonadIO m => NlSock -> m (NlCache RtnlRoute)
rtnlRouteGetAll sock =
    liftIO $ alloca $ \resultPtr -> do
      translateNlError (c_rtnl_route_alloc_cache sock resultPtr)
      cachePtr <- peek resultPtr
      NlCache <$> newForeignPtr c_nl_cache_free cachePtr <*> pure id

newtype RouteFamily = RouteFamily Word8
    deriving (Show, Eq, Ord)

foreign import ccall unsafe "rtnl_route_get_family" c_rtnl_route_get_family :: Ptr RtnlRoute -> IO RouteFamily
foreign import ccall unsafe "rtnl_route_set_family" c_rtnl_route_set_family :: Ptr RtnlRoute -> RouteFamily -> IO ()

routeFamily :: NlAttribute RtnlRoute RouteFamily
routeFamily = NlAttribute c_rtnl_route_set_family c_rtnl_route_get_family

foreign import ccall unsafe "rtnl_route_get_dst" c_rtnl_route_get_dst :: Ptr RtnlRoute -> IO (Ptr NetAddr)
foreign import ccall unsafe "rtnl_route_set_dst" c_rtnl_route_set_dst :: Ptr RtnlRoute -> Ptr NetAddr -> IO ()

routeDestination :: NlAttribute RtnlRoute (Maybe NetAddr)
routeDestination = nlNetAddrAttribute c_rtnl_route_set_dst c_rtnl_route_get_dst

type RouteTableId = Word32

foreign import ccall unsafe "rtnl_route_get_table" c_rtnl_route_get_table :: Ptr RtnlRoute -> IO RouteTableId
foreign import ccall unsafe "rtnl_route_set_table" c_rtnl_route_set_table :: Ptr RtnlRoute -> RouteTableId -> IO ()

routeTable :: NlAttribute RtnlRoute RouteTableId
routeTable = NlAttribute c_rtnl_route_set_table c_rtnl_route_get_table

type RoutePriority = Word32

foreign import ccall unsafe "rtnl_route_get_priority" c_rtnl_route_get_priority :: Ptr RtnlRoute -> IO RoutePriority
foreign import ccall unsafe "rtnl_route_set_priority" c_rtnl_route_set_priority :: Ptr RtnlRoute -> RoutePriority -> IO ()

routePriority :: NlAttribute RtnlRoute RoutePriority
routePriority = NlAttribute c_rtnl_route_set_priority c_rtnl_route_get_priority

foreign import ccall unsafe "rtnl_route_get_nnexthops" c_rtnl_route_get_nnexthops :: Ptr RtnlRoute -> IO CInt
foreign import ccall unsafe "rtnl_route_nexthop_n" c_rtnl_route_nexthop_n :: Ptr RtnlRoute -> CInt -> IO (Ptr RtnlNexthop)

routeNexthops :: RtnlRoute -> IO [RtnlNexthop]
routeNexthops route = withNlObject route $ \routePtr -> do
                        count <- c_rtnl_route_get_nnexthops routePtr
                        forM [0..(count - 1)] $ \i ->
                            fromNlPtr =<< c_rtnl_route_nexthop_n routePtr i

foreign import ccall unsafe "rtnl_route_add_nexthop" c_rtnl_route_add_nexthop :: Ptr RtnlRoute -> Ptr RtnlNexthop -> IO ()
foreign import ccall unsafe "rtnl_route_remove_nexthop" c_rtnl_route_remote_nexthop :: Ptr RtnlRoute -> Ptr RtnlNexthop -> IO ()

foreign import ccall unsafe "rtnl_route_nh_get_weight" c_rtnl_route_nh_get_weight :: Ptr RtnlNexthop -> IO Word8
foreign import ccall unsafe "rtnl_route_nh_set_weight" c_rtnl_route_nh_set_weight :: Ptr RtnlNexthop -> Word8 -> IO ()

nexthopWeight :: NlAttribute RtnlNexthop Word8
nexthopWeight = NlAttribute c_rtnl_route_nh_set_weight c_rtnl_route_nh_get_weight

foreign import ccall unsafe "rtnl_route_nh_get_ifindex" c_rtnl_route_nh_get_ifindex :: Ptr RtnlNexthop -> IO CInt
foreign import ccall unsafe "rtnl_route_nh_set_ifindex" c_rtnl_route_nh_set_ifindex :: Ptr RtnlNexthop -> CInt -> IO ()

nexthopIfIndex :: NlAttribute RtnlNexthop CInt
nexthopIfIndex = NlAttribute c_rtnl_route_nh_set_ifindex c_rtnl_route_nh_get_ifindex

foreign import ccall unsafe "rtnl_route_nh_get_gateway" c_rtnl_route_nh_get_gateway :: Ptr RtnlNexthop -> IO (Ptr NlAddr)
foreign import ccall unsafe "rtnl_route_nh_set_gateway" c_rtnl_route_nh_set_gateway :: Ptr RtnlNexthop -> Ptr NlAddr -> IO ()

nexthopGateway :: NlAttribute RtnlNexthop (Maybe LinkAddr)
nexthopGateway = nlSockAddrAttribute c_rtnl_route_nh_set_gateway c_rtnl_route_nh_get_gateway

showRoute :: RtnlRoute -> IO ()
showRoute rt = do
  table <- nlGet routeTable rt
  dst <- nlGet routeDestination rt
  family <- nlGet routeFamily rt
  prio <- nlGet routePriority rt
  nexthops <- routeNexthops rt

  putStrLn ("TABLE " ++ show table ++ "  Route: " ++ show dst)
  putStrLn ("  Prio: " ++ show prio ++ ", family: " ++ show family)

  forM_ (zip [0..] nexthops) $ \(i, nh) -> do
    ifIndex <- nlGet nexthopIfIndex nh
    weight <- nlGet nexthopWeight nh
    gw <- nlGet nexthopGateway nh

    putStrLn ("   Nexthop " ++ show i ++ ": " ++
              show gw ++ " weight: " ++ show weight ++ ", dev: " ++ show ifIndex)
