import System.Exit (exitSuccess)
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
import qualified XMonad.StackSet as W
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
         , ("S-M-<Return>", addName "Terminal (gnome-terminal)" $ spawn "gnome-terminal")
         , ("M-b",          addName "Browser"                 $ spawn "xdg-open https://")
         , ("S-M-l",        addName "Lock screen"             $ spawn "loginctl lock-session")
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
    -- These are xmonad's built-in defaults; they would work even if not listed
    -- here, but enumerating them lets the M-/ cheat sheet act as a full reference.
    ^++^ section "Standard bindings"
         ([ ("S-M-c",       addName "Close focused window"    $ kill)
          , ("M-q",         addName "Restart xmonad"          $ spawn "xmonad --recompile && xmonad --restart")
          , ("S-M-q",       addName "Quit xmonad"             $ io exitSuccess)
          , ("M-<Space>",   addName "Cycle layout"            $ sendMessage NextLayout)
          , ("S-M-<Space>", addName "Reset layout"            $ setLayout $ layoutHook c)
          , ("M-<Tab>",     addName "Focus next window"       $ windows W.focusDown)
          , ("S-M-<Tab>",   addName "Focus prev window"       $ windows W.focusUp)
          , ("M-j",         addName "Focus next in stack"     $ windows W.focusDown)
          , ("M-k",         addName "Focus prev in stack"     $ windows W.focusUp)
          , ("S-M-j",       addName "Swap with next"          $ windows W.swapDown)
          , ("S-M-k",       addName "Swap with prev"          $ windows W.swapUp)
          , ("M-<Return>",  addName "Promote to master"       $ windows W.swapMaster)
          , ("M-h",         addName "Shrink master"           $ sendMessage Shrink)
          , ("M-l",         addName "Expand master"           $ sendMessage Expand)
          , ("M-t",         addName "Sink floating window"    $ withFocused $ windows . W.sink)
          , ("M-,",         addName "Increment master count"  $ sendMessage (IncMasterN 1))
          , ("M-.",         addName "Decrement master count"  $ sendMessage (IncMasterN (-1)))
          , ("M-w",         addName "Focus screen 1"          $ screenWorkspace 0 >>= flip whenJust (windows . W.view))
          , ("M-e",         addName "Focus screen 2"          $ screenWorkspace 1 >>= flip whenJust (windows . W.view))
          , ("M-r",         addName "Focus screen 3"          $ screenWorkspace 2 >>= flip whenJust (windows . W.view))
          , ("S-M-w",       addName "Move window to screen 1" $ screenWorkspace 0 >>= flip whenJust (windows . W.shift))
          , ("S-M-e",       addName "Move window to screen 2" $ screenWorkspace 1 >>= flip whenJust (windows . W.shift))
          , ("S-M-r",       addName "Move window to screen 3" $ screenWorkspace 2 >>= flip whenJust (windows . W.shift))
          ] ++
          [ ("M-"   ++ [n], addName ("View workspace "    ++ [n]) $ windows $ W.greedyView [n]) | n <- "123456789" ] ++
          [ ("S-M-" ++ [n], addName ("Move to workspace " ++ [n]) $ windows $ W.shift      [n]) | n <- "123456789" ])
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
