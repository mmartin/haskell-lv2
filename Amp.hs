{-# LANGUAGE ForeignFunctionInterface #-}

module Amp where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Array

import Control.Monad

type Port = Ptr CFloat
type Ports = Ptr Port

data HandleData a = HandleData { ports      :: Ports
                               , userData   :: a
                               }

type Handle a = StablePtr (HandleData a)

type AmpPlugin = Handle ()

get_port :: Ports -> Int -> IO Port
get_port = peekElemOff

read_port :: Port -> Int -> IO CFloat
read_port = peekElemOff

write_port :: Port -> Int -> CFloat -> IO ()
write_port = pokeElemOff

get_read_port :: Ports -> Int -> Int -> IO CFloat
get_read_port ps pi si = get_port ps pi >>= \p -> read_port p si

instantiate_hs :: Ptr () -> CDouble -> CString -> Ptr () -> IO AmpPlugin
instantiate_hs _ _ _ _ = do
    p <- mallocArray 3
    newStablePtr HandleData { ports = p, userData = () }

cleanup_hs :: AmpPlugin -> IO ()
cleanup_hs h = freeStablePtr h

activate :: AmpPlugin -> IO ()
activate _ = return ()

deactivate :: AmpPlugin -> IO ()
deactivate _ = return ()

connect_port :: AmpPlugin -> Int -> Ptr CFloat -> IO ()
connect_port h i d = do
    p <- liftM ports $ deRefStablePtr h
    pokeElemOff p i d

run :: AmpPlugin -> CUInt -> IO ()
run h s = do
    p <- liftM ports $ deRefStablePtr h
    gain <- get_read_port p 0 0
    i <- get_port p 1
    o <- get_port p 2

    let s' = fromIntegral s
        fuu :: CFloat -> CFloat
        fuu a = a * (10 ** (gain * 0.05))

    let proc :: Int -> IO ()
        proc n | n < s' = liftM fuu (read_port i n) >>= write_port o n >> proc (n+1)
               | otherwise = return ()

    proc 0

foreign export ccall instantiate_hs :: Ptr () -> CDouble -> CString -> Ptr () -> IO AmpPlugin
foreign export ccall cleanup_hs :: AmpPlugin -> IO ()

foreign export ccall activate :: AmpPlugin -> IO ()
foreign export ccall deactivate :: AmpPlugin -> IO ()

foreign export ccall connect_port :: AmpPlugin -> Int -> Ptr CFloat -> IO ()
foreign export ccall run :: AmpPlugin -> CUInt -> IO ()
