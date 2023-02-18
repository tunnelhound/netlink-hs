{-# OPTIONS_GHC -fwarn-unused-imports #-}

module System.Network.Netlink.Types where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Exception (Exception, bracket, throwIO)

import           Data.Bits (Bits, (.|.), zeroBits)
import qualified Data.Text as T
import           Data.WideWord.Word128 (byteSwapWord128)
import           Data.Word (Word32, Word8)

import           Foreign.C.String (peekCString)
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Ptr
import           Foreign.Storable

import           Net.IPv4 (IPv4(..))
import qualified Net.IPv4 as IPv4
import           Net.IPv6 (IPv6)
import qualified Net.IPv6 as IPv6
import qualified Network.Socket as Network

import           System.IO.Unsafe

#include <sys/ioctl.h>

#include <netlink/netlink.h>
#include <netlink/cache.h>
#include <netlink/errno.h>

newtype BitFlags a = BitFlags a

instance Bits a => Monoid (BitFlags a) where
  mempty = BitFlags zeroBits

instance Bits a => Semigroup (BitFlags a) where
  BitFlags a <> BitFlags b = BitFlags (a .|. b)

newtype InterfaceIndex = InterfaceIndex CInt
  deriving (Show, Eq, Ord)
  deriving newtype (Num, Integral, Real, Enum, Bounded)

-- * Netlink objects

foreign import ccall unsafe "nl_object_get" c_nl_object_get :: Ptr a -> IO ()
foreign import ccall unsafe "&nl_object_put" c_nl_object_put :: FunPtr (Ptr a -> IO ())

newtype NlObject a = NlObject (ForeignPtr a)
newtype NlUnownedObject a = NlUnownedObject (ForeignPtr a)

class HasNlObject what where
  withNlObject :: what -> (Ptr what -> IO a) -> IO a
  fromNlPtr :: Ptr what -> IO what

takeNlPtr :: HasNlObject what => Ptr what -> IO what
takeNlPtr ptr = do
  c_nl_object_get ptr
  fromNlPtr ptr

data NlAttribute what a
  = NlAttribute
  { setNlAttribute :: Ptr what -> a -> IO ()
  , getNlAttribute :: Ptr what -> IO a }

instance HasNlObject (NlObject a) where
  withNlObject (NlObject a) action = withForeignPtr a (action . castPtr)
  fromNlPtr ptr = NlObject <$> newForeignPtr c_nl_object_put (castPtr ptr)

instance HasNlObject (NlUnownedObject a) where
  withNlObject (NlUnownedObject a) action = withForeignPtr a (action . castPtr)
  fromNlPtr ptr = NlUnownedObject <$> newForeignPtr_ (castPtr ptr)

nlSet :: (MonadIO m, HasNlObject what) => NlAttribute what a -> what -> a -> m ()
nlSet attr what newValue =
  liftIO $ withNlObject what $ \linkPtr ->
    setNlAttribute attr linkPtr newValue

nlGet :: (MonadIO m, HasNlObject what) => NlAttribute what a -> what -> m a
nlGet attr what =
  liftIO $ withNlObject what $ \linkPtr ->
    getNlAttribute attr linkPtr

nlCopy :: (MonadIO m, HasNlObject what) => NlAttribute what a -> what -> what -> m ()
nlCopy attr new old =
    nlSet attr new =<< nlGet attr old

nlUpdate :: (MonadIO m, HasNlObject what) => NlAttribute what a -> what -> (a -> a) -> m ()
nlUpdate attr x modify = do
  old <- nlGet attr x
  let new = modify old
  nlSet attr x new

-- * Nlcaches

data NlCache a where
    NlCache :: HasNlObject underlying => ForeignPtr (NlCache underlying) -> (underlying -> a) -> NlCache a

instance Functor NlCache where
    fmap f (NlCache underlying g) = NlCache underlying (f . g)

foreign import ccall unsafe "nl_cache_get_first" c_nl_cache_get_first :: Ptr (NlCache underlying) -> IO (Ptr underlying)
foreign import ccall unsafe "nl_cache_get_next" c_nl_cache_get_next :: Ptr underlying -> IO (Ptr underlying)
foreign import ccall unsafe "&nl_cache_free" c_nl_cache_free :: FunPtr (Ptr (NlCache underlying) -> IO ())

instance Foldable NlCache where
    foldr build (seed :: b) (NlCache underlying f) =
      unsafePerformIO $
      withForeignPtr underlying $ \(underlyingPtr :: Ptr (NlCache underlying)) -> do
        (firstPtr :: Ptr underlying) <- c_nl_cache_get_first underlyingPtr

        let go :: (b -> IO b) -> Ptr underlying -> IO b
            go a curPtr
              | curPtr == nullPtr = a seed
              | otherwise = do
                  nextPtr <- c_nl_cache_get_next curPtr
                  cur <- takeNlPtr curPtr

                  go (a . build (f cur)) nextPtr

        go pure firstPtr

-- * Net link error handling
newtype NlError = NlError CInt
  deriving (Eq, Ord)
  deriving anyclass (Exception)

pattern NleSuccess, NleFailure, NleIntr, NleBadSock, NleAgain, NleNoMem, NleExist,
        NleInval, NleRange, NleMsgSize, NleOpNotSupp, NleAfNoSupport, NleObjNotFound,
        NleNoAttr, NleMissingAttr, NleAfMismatch, NleSeqMismatch, NleMsgOverflow,
        NleMsgTrunc, NleNoAddr, NleSrcRtNoSupport, NleMsgTooShort, NleMsgTypeNoSupport,
        NleObjMismatch, NleNoCache, NleBusy, NleProtoMismatch, NleNoAccess, NlePerm,
        NlePktLocFile, NleParseErr, NleImmutable, NleDumpIntr :: NlError

pattern NleSuccess     = NlError (#const NLE_SUCCESS)
pattern NleFailure     = NlError (#const NLE_FAILURE)
pattern NleIntr        = NlError (#const NLE_INTR)
pattern NleBadSock     = NlError (#const NLE_BAD_SOCK)
pattern NleAgain       = NlError (#const NLE_AGAIN)
pattern NleNoMem       = NlError (#const NLE_NOMEM)
pattern NleExist       = NlError (#const NLE_EXIST)
pattern NleInval       = NlError (#const NLE_INVAL)
pattern NleRange       = NlError (#const NLE_RANGE)
pattern NleMsgSize     = NlError (#const NLE_MSGSIZE)
pattern NleOpNotSupp   = NlError (#const NLE_OPNOTSUPP)
pattern NleAfNoSupport = NlError (#const NLE_AF_NOSUPPORT)
pattern NleObjNotFound = NlError (#const NLE_OBJ_NOTFOUND)
pattern NleNoAttr      = NlError (#const NLE_NOATTR)
pattern NleMissingAttr = NlError (#const NLE_MISSING_ATTR)
pattern NleAfMismatch  = NlError (#const NLE_AF_MISMATCH)
pattern NleSeqMismatch = NlError (#const NLE_SEQ_MISMATCH)
pattern NleMsgOverflow = NlError (#const NLE_MSG_OVERFLOW)
pattern NleMsgTrunc    = NlError (#const NLE_MSG_TRUNC)
pattern NleNoAddr      = NlError (#const NLE_NOADDR)
pattern NleSrcRtNoSupport = NlError (#const NLE_SRCRT_NOSUPPORT)
pattern NleMsgTooShort = NlError (#const NLE_MSG_TOOSHORT)
pattern NleMsgTypeNoSupport = NlError (#const NLE_MSGTYPE_NOSUPPORT)
pattern NleObjMismatch = NlError (#const NLE_OBJ_MISMATCH)
pattern NleNoCache     = NlError (#const NLE_NOCACHE)
pattern NleBusy        = NlError (#const NLE_BUSY)
pattern NleProtoMismatch = NlError (#const NLE_PROTO_MISMATCH)
pattern NleNoAccess    = NlError (#const NLE_NOACCESS)
pattern NlePerm        = NlError (#const NLE_PERM)
pattern NlePktLocFile  = NlError (#const NLE_PKTLOC_FILE)
pattern NleParseErr    = NlError (#const NLE_PARSE_ERR)
pattern NleImmutable   = NlError (#const NLE_IMMUTABLE)
pattern NleDumpIntr    = NlError (#const NLE_DUMP_INTR)

foreign import ccall unsafe "nl_geterror" c_nl_geterror :: NlError -> Ptr CChar
nlGetError :: NlError -> String
nlGetError (NlError e) = unsafePerformIO . peekCString . c_nl_geterror . NlError . negate $ e

instance Show NlError where
  show err@(NlError errno) = "<NlError " ++ what ++ ": " ++ nlGetError err ++ ">"
    where
      what = case err of
               NleSuccess          -> "NleSucess"
               NleFailure          -> "NleFailure"
               NleIntr             -> "NleIntr"
               NleBadSock          -> "NleBadSock"
               NleAgain            -> "NleAgain"
               NleNoMem            -> "NleNoMem"
               NleExist            -> "NleExist"
               NleInval            -> "NleInval"
               NleRange            -> "NleRange"
               NleMsgSize          -> "NleMsgSize"
               NleOpNotSupp        -> "NleOpNotSupp"
               NleAfNoSupport      -> "NleAfNoSupport"
               NleObjNotFound      -> "NleObjNotFound"
               NleNoAttr           -> "NleNoAttr"
               NleSrcRtNoSupport   -> "NleSrcRtNoSupport"
               NleMsgTooShort      -> "NleMsgTooShort"
               NleMsgTypeNoSupport -> "NleMsgTypeNoSupport"
               NleObjMismatch      -> "NleObjMismatch"
               NleNoCache          -> "NleNoCache"
               NleBusy             -> "NleBusy"
               NleProtoMismatch    -> "NleProtoMismatch"
               NleNoAccess         -> "NleNoAccess"
               NlePerm             -> "NlePerm"
               NlePktLocFile       -> "NlePktLocFile"
               NleParseErr         -> "NleParseErr"
               NleImmutable        -> "NleImmutable"
               NleDumpIntr         -> "NleDumpIntr"
               _                   -> show errno

translateNlError :: IO NlError -> IO ()
translateNlError run = do
  err <- run
  case err of
    NleSuccess -> pure ()
    NlError e -> throwIO (NlError (negate e))

-- * Netlink sockets

newtype NlSock = NlSock (Ptr NlSock)
  deriving (Show, Eq, Ord)
newtype NlFamily = NlFamily CInt
  deriving (Show, Eq, Ord)

foreign import ccall unsafe "nl_socket_alloc" c_nl_socket_alloc :: IO NlSock
foreign import ccall unsafe "nl_socket_free" c_nl_socket_free :: NlSock -> IO ()
foreign import ccall unsafe "nl_connect" c_nl_connect :: NlSock -> NlFamily -> IO NlError

withNlSock :: NlFamily -> (NlSock -> IO a) -> IO a
withNlSock family withSock =
  bracket c_nl_socket_alloc c_nl_socket_free $ \sockPtr -> do
    translateNlError (c_nl_connect sockPtr family)

    withSock sockPtr

-- * NlAddr

newtype NlAddr = NlAddr (Ptr NlAddr)
  deriving (Show, Eq, Ord)

data LinkAddr
  = LinkAddrInet  !IPv4
  | LinkAddrInet6 !IPv6
  deriving (Show, Eq, Ord)

data NetAddr
  = NetAddr LinkAddr Word8
    deriving (Show, Eq, Ord)

encodeLinkAddress :: LinkAddr -> T.Text
encodeLinkAddress (LinkAddrInet a) = IPv4.encode a
encodeLinkAddress (LinkAddrInet6 a) = IPv6.encode a

linkAddrFamily :: LinkAddr -> Network.Family
linkAddrFamily (LinkAddrInet {}) = Network.AF_INET
linkAddrFamily (LinkAddrInet6 {}) = Network.AF_INET6

sizeOfLinkAddress :: Integral a => LinkAddr -> a
sizeOfLinkAddress addr =
  fromIntegral $
  case addr of
    LinkAddrInet  a -> sizeOf a
    LinkAddrInet6 a -> sizeOf a

foreign import ccall unsafe "ntohl" c_ntohl :: Word32 -> Word32
foreign import ccall unsafe "htonl" c_htonl :: Word32 -> Word32

reverseIPv6 :: IPv6 -> IPv6
reverseIPv6 (IPv6.IPv6 addr) = IPv6.IPv6 (byteSwapWord128 addr)

pokeLinkAddress :: Ptr LinkAddr -> LinkAddr -> IO ()
pokeLinkAddress p addr =
  case addr of
    LinkAddrInet  (IPv4 a) -> poke (castPtr p) (c_htonl a)
    LinkAddrInet6 a -> poke (castPtr p) (reverseIPv6 a)

peekLinkAddress :: Network.Family -> Ptr LinkAddr -> IO LinkAddr
peekLinkAddress Network.AF_INET p  = LinkAddrInet . IPv4 . c_ntohl <$> peek (castPtr p)
peekLinkAddress Network.AF_INET6 p = LinkAddrInet6 . reverseIPv6 <$> peek (castPtr p)
peekLinkAddress _ _ = fail "Unknown link address"

withLinkAddr :: LinkAddr -> (Network.Family -> Ptr LinkAddr -> IO a) -> IO a
withLinkAddr addr withAddress =
  allocaBytes (sizeOfLinkAddress addr) $ \addrPtr -> do
    pokeLinkAddress addrPtr addr
    withAddress (linkAddrFamily addr) addrPtr

foreign import ccall unsafe "nl_addr_build" c_nl_addr_build
  :: CInt -> Ptr LinkAddr -> CSize -> IO (Ptr NlAddr)
foreign import ccall unsafe "nl_addr_put" c_nl_addr_put :: Ptr NlAddr -> IO ()
foreign import ccall unsafe "nl_addr_get_binary_addr" c_nl_addr_get_binary_addr :: Ptr NlAddr -> IO (Ptr LinkAddr)
foreign import ccall unsafe "nl_addr_get_family" c_nl_addr_get_family :: Ptr NlAddr -> IO CInt
foreign import ccall unsafe "nl_addr_get_prefixlen" c_nl_addr_get_prefixlen :: Ptr NlAddr -> IO CInt
foreign import ccall unsafe "nl_addr_set_prefixlen" c_nl_addr_set_prefixlen :: Ptr NlAddr -> CInt -> IO ()


withNlAddr :: LinkAddr -> (Ptr NlAddr -> IO a) -> IO a
withNlAddr linkAddr withAddr = do
  withLinkAddr linkAddr $ \family linkAddrPtr ->
    bracket (c_nl_addr_build (Network.packFamily family) linkAddrPtr
                  (sizeOfLinkAddress linkAddr))
            c_nl_addr_put withAddr

withNlNetAddr :: NetAddr -> (Ptr NlAddr -> IO a) -> IO a
withNlNetAddr (NetAddr linkAddr len) withAddr =
  withNlAddr linkAddr $ \nlAddr -> do
    c_nl_addr_set_prefixlen nlAddr (fromIntegral len)
    withAddr nlAddr

nlSockAddrAttribute :: (Ptr what -> Ptr NlAddr -> IO ())
                    -> (Ptr what-> IO (Ptr NlAddr))
                    -> NlAttribute what (Maybe LinkAddr)
nlSockAddrAttribute setNlAddr getNlAddr =
  NlAttribute (\what mSockAddr ->
                 case mSockAddr of
                   Nothing -> setNlAddr what nullPtr
                   Just sockAddr ->
                     withNlAddr sockAddr $ \nlAddr ->
                       setNlAddr what nlAddr)
              (\what -> do
                 nlAddr <- getNlAddr what -- This does not increase the reference count

                 if nlAddr == nullPtr
                    then pure Nothing
                    else do
                      addrPtr <- c_nl_addr_get_binary_addr nlAddr
                      family <- c_nl_addr_get_family nlAddr

                      Just <$> peekLinkAddress (Network.unpackFamily family) addrPtr)

nlNetAddrAttribute :: (Ptr what -> Ptr NetAddr -> IO ())
                    -> (Ptr what-> IO (Ptr NetAddr))
                    -> NlAttribute what (Maybe NetAddr)
nlNetAddrAttribute setNlAddr getNlAddr =
  NlAttribute (\what mSockAddr ->
                 case mSockAddr of
                   Nothing -> setNlAddr what nullPtr
                   Just netAddr ->
                     withNlNetAddr netAddr $ \netAddrPtr ->
                       setNlAddr what (castPtr netAddrPtr))
              (\what -> do
                 nlAddr <- castPtr <$> getNlAddr what -- This does not increase the reference count

                 if nlAddr == nullPtr
                    then pure Nothing
                    else do
                      addrPtr <- c_nl_addr_get_binary_addr nlAddr
                      family <- c_nl_addr_get_family nlAddr
                      prefix <- c_nl_addr_get_prefixlen nlAddr

                      Just <$> (NetAddr <$> peekLinkAddress (Network.unpackFamily family) addrPtr <*> pure (fromIntegral prefix)))
