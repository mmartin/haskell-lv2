import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program

import System.FilePath.Posix
import System.Directory

main = defaultMainWithHooks simpleUserHooks { buildHook = bHook
                                            , copyHook  = cHook
                                            }

bHook pkg lbi h f = do
    buildHook simpleUserHooks pkg lbi h f

    let programs = withPrograms lbi
        Just ghc = lookupKnownProgram "ghc" programs >>=
                   \p -> lookupProgram p programs

    runProgram (fromFlag $ buildVerbosity f) ghc
               ["-c", "src/lv2_stub.c", "-fPIC", "-dynamic",
                "-I" ++ buildDir lbi, "-odir " ++ buildDir lbi]

cHook pkg lbi h f = do
    copyHook simpleUserHooks pkg lbi h f

    let dirs  = installDirTemplates lbi
        outdir = (fromPathTemplate $ libdir dirs) </>
                 (fromPathTemplate $ libsubdir dirs)
        indir = buildDir lbi

    copyFile (indir </> "src" </> "lv2_stub.o") (outdir </> "lv2_stub.o")
