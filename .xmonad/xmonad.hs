import System.IO (hPutStr, hClose)
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
import XMonad.Util.EZConfig (mkNamedKeymap)
import XMonad.Util.NamedActions
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare

sessionConfig "gnome"        = gnomeConfig
sessionConfig "kde"          = kde4Config
sessionConfig "xfce"         = xfceConfig
sessionConfig "xmonad-gnome" = gnomeConfig
sessionConfig _              = desktopConfig

main = do
  session <- getEnv "DESKTOP_SESSION"
  let config = personalize (maybe desktopConfig sessionConfig session)
  xmonad $ config

hiddenNonEmptyWS = hiddenWS :&: Not emptyWS

-- Press M-/ at any time to pop up a cheat sheet of every binding below.
personalize c = addDescrKeys' ((mod4Mask, xK_slash), showKeybindings) myKeys $ c
    { modMask    = mod4Mask
    , manageHook = manageHook c <+> composeOne
                   [
                     fmap not isDialog -?> doF avoidMaster
                   , return True       -?> doF swapDown
                   ]
    , layoutHook = mkToggle (single REFLECTX) $
                   mkToggle (single REFLECTY) $
                     maximize (layoutHook c ||| simpleTabbed)
    }

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show keybindings" $ io $ do
    h <- spawnPipe "xmessage -file -"
    hPutStr h (unlines $ showKm x)
    hClose h

myKeys c =
       section "Launchers"
         [ ("M-d",          addName "dmenu launcher"          $ spawn "dmenu_run")
         , ("M-S-<Return>", addName "Terminal (gnome-terminal)" $ spawn "gnome-terminal")
         , ("M-b",          addName "Browser (firefox)"       $ spawn "firefox")
         , ("M-S-l",        addName "Lock screen"             $ spawn "loginctl lock-session")
         ]
    ^++^ section "Layout"
         [ ("M-\\",         addName "Maximize / restore"      $ withFocused (sendMessage . maximizeRestore))
         , ("S-M-x",        addName "Reflect X"               $ sendMessage $ Toggle REFLECTX)
         , ("S-M-y",        addName "Reflect Y"               $ sendMessage $ Toggle REFLECTY)
         ]
    ^++^ section "Workspaces"
         [ ("M-f",          addName "Move to next empty WS"   $ moveTo Next emptyWS)
         , ("M-<D>",        addName "Move to next non-empty"  $ moveTo Next hiddenNonEmptyWS)
         , ("M-<U>",        addName "Move to prev non-empty"  $ moveTo Prev hiddenNonEmptyWS)
         , ("S-M-f",        addName "Follow to next empty WS" $ followTo Next emptyWS)
         , ("S-M-<D>",      addName "Follow to next non-empty"$ followTo Next hiddenNonEmptyWS)
         , ("S-M-<U>",      addName "Follow to prev non-empty"$ followTo Prev hiddenNonEmptyWS)
         , ("M-z",          addName "Toggle last workspace"   $ toggleWS)
         ]
    ^++^ section "Screens"
         [ ("M-<R>",        addName "Focus next screen"       $ nextScreen)
         , ("M-<L>",        addName "Focus prev screen"       $ prevScreen)
         , ("S-M-<R>",      addName "Shift to next screen"    $ shiftNextScreen)
         , ("S-M-<L>",      addName "Shift to prev screen"    $ shiftPrevScreen)
         , ("C-S-M-<R>",    addName "Swap with next screen"   $ swapNextScreen)
         , ("C-S-M-<L>",    addName "Swap with prev screen"   $ swapPrevScreen)
         ]
    ^++^ section "Media"
         [ ("<XF86AudioRaiseVolume>", addName "Volume up"      $ spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")
         , ("<XF86AudioLowerVolume>", addName "Volume down"    $ spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
         , ("<XF86AudioMute>",        addName "Mute toggle"    $ spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
         , ("<XF86MonBrightnessUp>",  addName "Brightness up"  $ spawn "brightnessctl set +5%")
         , ("<XF86MonBrightnessDown>",addName "Brightness down" $ spawn "brightnessctl set 5%-")
         ]
    ^++^ section "Screenshots"
         [ ("<Print>",      addName "Region screenshot"       $ spawn "gnome-screenshot -a")
         , ("M-<Print>",    addName "Full screenshot"         $ spawn "gnome-screenshot")
         ]
  where
    section name ks = subtitle name : mkNamedKeymap c ks

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
