{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface #-}

module LV2
    ( Desc(..)
    , Handle
    , exportDesc
    , getPort
    , readPort
    , writePort
    , buildDesc
    )
    where

import Language.Haskell.TH

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
                   , run        :: Handle a -> Int -> IO ()
                   }


type Port = Ptr CFloat

data HandleData a = HandleData { hports     :: Ptr Port
                               , desc       :: StablePtr (Desc a)
                               , userData   :: a
                               }

type Handle a = StablePtr (HandleData a)

getPort :: Handle a -> Int -> IO Port
getPort h i = deRefStablePtr h >>= \h' -> peekElemOff (hports h') i

readPort :: Handle a -> Int -> Int -> IO [CFloat]
readPort h i s = getPort h i >>= \p -> peekArray s p

writePort :: Handle a -> Int -> [CFloat] -> IO ()
writePort h i d = getPort h i >>= \p -> pokeArray p d

instantiateHs :: StablePtr (Desc a) -> Ptr () -> CDouble -> CString -> Ptr () -> IO (Handle a)
instantiateHs h x1 x2 x3 x4 = do
    d@Desc{instantiate=i} <- deRefStablePtr h
    ud <- i x1 x2 x3 x4
    p <- mallocArray $ ports d
    newStablePtr HandleData { hports = p, desc = h, userData = ud }

callbackWrapper f h = liftM f (deRefStablePtr h >>= deRefStablePtr . desc)

cleanupHs :: Handle a -> IO ()
cleanupHs h = do
    callbackWrapper cleanup h >>= \f -> f h
    liftM hports (deRefStablePtr h) >>= free
    freeStablePtr h

activateHs :: Handle a -> IO ()
activateHs h = callbackWrapper activate h >>= \f -> f h

deactivateHs :: Handle a -> IO ()
deactivateHs h = callbackWrapper deactivate h >>= \f -> f h

connectPortHs :: Handle a -> Int -> Ptr CFloat -> IO ()
connectPortHs h i d = do
    p <- liftM hports $ deRefStablePtr h
    pokeElemOff p i d

runHs:: Handle a -> Int -> IO ()
runHs h s = callbackWrapper run h >>= \f -> f h s

--extensionDataHs :: CString -> Ptr ()
--extensionDataHs _ = nullPtr

foreign export ccall cleanupHs :: Handle a -> IO ()
--foreign export ccall extensionDataHs :: CString -> Ptr ()

foreign export ccall activateHs :: Handle a -> IO ()
foreign export ccall deactivateHs :: Handle a -> IO ()

foreign export ccall connectPortHs :: Handle a -> Int -> Ptr CFloat -> IO ()
foreign export ccall runHs :: Handle a -> Int -> IO ()


descUri :: StablePtr (Desc a) -> IO CString
descUri h = deRefStablePtr h >>= newCString . uri

type Inst a = Ptr () -> CDouble -> CString -> Ptr () -> IO (Handle a)
descInst :: StablePtr (Desc a) -> IO (FunPtr (Inst a))
descInst h = mkInst $ instantiateHs h

foreign export ccall descUri :: StablePtr (Desc a) -> IO CString
foreign import ccall "wrapper" mkInst :: Inst a -> IO (FunPtr (Inst a))
foreign export ccall descInst :: StablePtr (Desc a) -> IO (FunPtr (Inst a))

buildDesc :: [Desc ()] -> (Int -> IO (StablePtr (Desc ())))
buildDesc d = xx
    where xx i | i < length d = newStablePtr (d !! (fromIntegral i))
               | otherwise    = return $ castPtrToStablePtr nullPtr

exportDesc :: Name -> Q [Dec]
exportDesc name = do
    t <- [t| Int -> IO (StablePtr (Desc ())) |]
    return [ForeignD (ExportF CCall "lv2descriptors" name t)]
