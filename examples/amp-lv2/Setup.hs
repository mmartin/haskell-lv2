import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program

import qualified Distribution.LV2 as LV2

main = defaultMainWithHooks simpleUserHooks { confHook = LV2.confHook simpleUserHooks }
