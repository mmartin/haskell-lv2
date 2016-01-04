{-# LANGUAGE ForeignFunctionInterface #-}

module LV2
    where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

import Control.Monad

data Desc a = Desc { uri    :: String
                   , ports  :: Int
                   , instantiate    :: Ptr () -> CDouble -> CString -> Ptr () -> IO a
                   --, extensionData  :: CString -> IO (Ptr ())
                   , cleanup    :: Handle a -> IO ()
                   , activate   :: Handle a -> IO ()
                   , deactivate :: Handle a -> IO ()
                   , run        :: Handle a -> CUInt -> IO ()
                   }


type Port = Ptr CFloat
type Ports = Ptr Port

data HandleData a = HandleData { hports     :: Ports
                               , desc       :: StablePtr (Desc a)
                               , userData   :: a
                               }

type Handle a = StablePtr (HandleData a)

getPort :: Ports -> Int -> IO Port
getPort = peekElemOff

readPort :: Port -> Int -> IO CFloat
readPort = peekElemOff

writePort :: Port -> Int -> CFloat -> IO ()
writePort = pokeElemOff

getReadPort :: Ports -> Int -> Int -> IO CFloat
getReadPort ps pi si = getPort ps pi >>= \p -> readPort p si

instantiateHs :: StablePtr (Desc a) -> Ptr () -> CDouble -> CString -> Ptr () -> IO (Handle a)
instantiateHs h x1 x2 x3 x4 = do
    d@Desc{instantiate=i} <- deRefStablePtr h
    ud <- i x1 x2 x3 x4
    p <- mallocArray $ ports d
    newStablePtr HandleData { hports = p, desc = h, userData = ud }

rrr f h = liftM f (deRefStablePtr h >>= deRefStablePtr . desc)

cleanupHs :: Handle a -> IO ()
cleanupHs h = do
    rrr cleanup h >>= \f -> f h
    liftM hports (deRefStablePtr h) >>= free
    freeStablePtr h

activateHs :: Handle a -> IO ()
activateHs h = rrr activate h >>= \f -> f h

deactivateHs :: Handle a -> IO ()
deactivateHs h = rrr deactivate h >>= \f -> f h

connectPortHs :: Handle a -> Int -> Ptr CFloat -> IO ()
connectPortHs h i d = do
    p <- liftM hports $ deRefStablePtr h
    pokeElemOff p i d

runHs:: Handle a -> CUInt -> IO ()
runHs h s = rrr run h >>= \f -> f h s

--extensionDataHs :: CString -> Ptr ()
--extensionDataHs _ = nullPtr

foreign export ccall cleanupHs :: Handle a -> IO ()
--foreign export ccall extensionDataHs :: CString -> Ptr ()

foreign export ccall activateHs :: Handle a -> IO ()
foreign export ccall deactivateHs :: Handle a -> IO ()

foreign export ccall connectPortHs :: Handle a -> Int -> Ptr CFloat -> IO ()
foreign export ccall runHs :: Handle a -> CUInt -> IO ()


descUri :: StablePtr (Desc a) -> IO CString
descUri h = deRefStablePtr h >>= newCString . uri

type Inst a = Ptr () -> CDouble -> CString -> Ptr () -> IO (Handle a)
descInst :: StablePtr (Desc a) -> IO (FunPtr (Inst a))
descInst h = mkInst $ instantiateHs h

foreign export ccall descUri :: StablePtr (Desc a) -> IO CString
foreign import ccall "wrapper" mkInst :: Inst a -> IO (FunPtr (Inst a))
foreign export ccall descInst :: StablePtr (Desc a) -> IO (FunPtr (Inst a))



aaa :: Handle a -> CUInt -> IO ()
aaa h s = do
    h' <- deRefStablePtr h
    let p = hports h'
        d = userData h'

    gain <- getReadPort p 0 0
    i <- getPort p 1
    o <- getPort p 2

    let s' = fromIntegral s
        fuu a = a * (10 ** (gain * 0.05))

    liftM (map fuu) (peekArray s' i) >>= pokeArray o

lv2descriptors :: CUInt -> IO (StablePtr (Desc ()))
lv2descriptors 0 = newStablePtr
    Desc { uri   = "http://github.com/mmartin/amp"
         , ports = 3
         , instantiate = \_ _ _ _ -> return ()
         --, extensionData = \_ -> return nullPtr
         , cleanup = \_ -> return ()
         , activate = \_ -> return ()
         , deactivate = \_ -> return ()
         , run = aaa
         }
lv2descriptors _ = return $ castPtrToStablePtr nullPtr
foreign export ccall lv2descriptors :: CUInt -> IO (StablePtr (Desc ()))
