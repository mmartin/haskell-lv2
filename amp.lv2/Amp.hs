{-# LANGUAGE TemplateHaskell #-}
module Amp where


import LV2

proc h g i o s = do
    [gain] <- readPort h g 1
    samples <- readPort h i s
    writePort h o $ map (\a -> a * (10 ** (gain * 0.05))) samples

runMono :: Handle a -> Int -> IO ()
runMono h s = do
    proc h 0 1 2 s

runStereo :: Handle a -> Int -> IO ()
runStereo h s = do
    proc h 0 2 3 s
    proc h 1 4 5 s

lol = buildDesc [ Desc { uri   = "http://github.com/mmartin/amp#Mono"
                       , ports = 3
                       , instantiate = \_ _ _ _ -> return ()
                       --, extensionData = \_ -> return nullPtr
                       , cleanup = \_ -> return ()
                       , activate = \_ -> return ()
                       , deactivate = \_ -> return ()
                       , run = runMono
                       }
                , Desc { uri   = "http://github.com/mmartin/amp#Stereo"
                       , ports = 6
                       , instantiate = \_ _ _ _ -> return ()
                       --, extensionData = \_ -> return nullPtr
                       , cleanup = \_ -> return ()
                       , activate = \_ -> return ()
                       , deactivate = \_ -> return ()
                       , run = runStereo
                       }
                ]

$(exportDesc 'lol)
