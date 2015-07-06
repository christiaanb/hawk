module ProxyVisualHawk where

import ObjectScript
import VisualHawk

instance Component VisualHawk where
  className     = const "VisualHawk"

visualHawkQueryInterface iidstr st = return (iidstr == iidIVisualHawk)
  where
    iidIVisualHawk = "{C0EA2BF5-AFD1-11D2-B49F-006008D1BF8C}"

main :: IO ()
main = do addScriptConstructor new
          addScriptProperty   "VersionString" readonly getVersionString
          addScriptMethod_2_0 "GenHawk" genHawk
          addScriptMethod_0_0 "CheckWire" checkWire
          addScriptMethod_0_0 "UpdateMenu" updateMenu
          addScriptMethod_1_0 "ReWriteWith" rewriteWith
