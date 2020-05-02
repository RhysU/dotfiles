import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.StackSet
import XMonad.Util.EZConfig
import XMonad.Util.WorkspaceCompare

desktop "gnome"        = gnomeConfig
desktop "kde"          = kde4Config
desktop "xfce"         = xfceConfig
desktop "xmonad-gnome" = gnomeConfig
desktop _              = desktopConfig

main = do
  session <- getEnv "DESKTOP_SESSION"
  let config = personalize (maybe desktopConfig desktop session)
  xmonad $ config

personalize c = c
    { modMask    = mod4Mask
    , manageHook = manageHook c <+> composeOne
                   [
                     fmap not isDialog -?> doF avoidMaster
                   , return True       -?> doF swapDown
                   ]
    , layoutHook = mkToggle (single REFLECTX) $
                   mkToggle (single REFLECTY) $
                     maximize (layoutHook c ||| simpleTabbed)
    } `additionalKeysP`
    [ ("M-d" ,      spawn "dmenu_run")
    , ("M-\\",      withFocused (sendMessage . maximizeRestore))
    , ("M-f",       moveTo Next EmptyWS)
    , ("M-<D>",     moveTo Next HiddenNonEmptyWS)
    , ("M-<U>",     moveTo Prev HiddenNonEmptyWS)
    , ("S-M-f",     followTo Next EmptyWS   )
    , ("S-M-<D>",   followTo Next HiddenNonEmptyWS)
    , ("S-M-<U>",   followTo Prev HiddenNonEmptyWS)
    , ("M-<R>",     nextScreen)
    , ("M-<L>",     prevScreen)
    , ("S-M-<R>",   shiftNextScreen)
    , ("S-M-<L>",   shiftPrevScreen)
    , ("C-S-M-<R>", swapNextScreen)
    , ("C-S-M-<L>", swapPrevScreen)
    , ("S-M-x",     sendMessage $ Toggle REFLECTX)
    , ("S-M-y",     sendMessage $ Toggle REFLECTY)
    , ("M-z",       toggleWS)
    ]

-- Written by Marshall Lochbaum on xmonad@haskell.org mailing list
-- http://www.haskell.org/pipermail/xmonad/2013-August/013778.html
followTo :: Direction1D -> WSType -> X ()
followTo dir t = doTo dir t getSortByIndex (\w -> (windows (shift w)) >> (windows (greedyView w)))

-- Section 3 of http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions
-- and, in particular, including the tweak to keep focus with the master window
-- FIXME May be cleaner to use the version given there
avoidMaster :: StackSet i l a s sd -> StackSet i l a s sd
avoidMaster = modify' $ \c -> case c of
    Stack t [] (r:rs) ->  Stack r [] (t:rs)
    otherwise         -> c
