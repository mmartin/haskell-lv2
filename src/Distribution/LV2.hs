module Distribution.LV2
    ( confHook
    )
    where

import Distribution.Simple hiding (confHook)
import qualified Distribution.Simple as DS
import Distribution.Simple.PackageIndex
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.InstalledPackageInfo

import System.FilePath.Posix

import Data.List

confHook defaultHooks =
    \pkg flags -> do
        lbi <- DS.confHook defaultHooks pkg flags

        let libFind (a, _, _) = case a of CLibName -> True; _ -> False
            Just (_, libConfig, _) = find libFind $ componentsConfigs lbi
            deps = componentPackageDeps libConfig
            Just (hlv2id, _) = find (\(_, p) -> unPackageName (pkgName p) == "haskell-lv2") deps
            Just hlv2 = lookupInstalledPackageId (installedPkgs lbi) hlv2id
            cStub = head (libraryDirs hlv2) </> "lv2_stub.o"

        let ver = intercalate "." $ map show $ versionBranch $ compilerVersion $ compiler lbi

        let bi = emptyBuildInfo { options = [(GHC, ["--make", "-shared", "-dynamic",
                                                    "-fPIC", "-no-hs-main", "-lHSrts-ghc" ++ ver,
                                                    cStub
                                            ])]
                                }

        return lbi { localPkgDescr = updatePackageDescription (Just bi, []) (localPkgDescr lbi) }
