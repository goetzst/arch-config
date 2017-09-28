import Control.Monad (void)

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Spacing
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

import qualified XMonad.StackSet as W

import System.IO
import System.Posix.Files (touchFile)

import Data.Maybe (fromJust, maybeToList, fromMaybe)
import qualified Data.Map as M (toList)
import Data.Ord (comparing)
import qualified Data.List as L

import Control.Monad.Error.Class (MonadError)

---------------------------------------------------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------------------------------------------------

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad
    $ withUrgencyHook NoUrgencyHook
    $ fullscreenFix
    $ myConfig xmproc `additionalKeys` extraKeys

myConfig pipe = defaultConfig
          { borderWidth         = myBorderWidth
          , startupHook         = myStartupHook
          , manageHook          = myManageHook
          , layoutHook          = myLayoutHook
          , handleEventHook     = myHandleEventHook
          , logHook             = myLogHook pipe
          , modMask             = myModMask
          , terminal            = myTerminal
          , normalBorderColor   = myNormalBorderColor
          , focusedBorderColor  = myFocusedBorderColor
          , focusFollowsMouse   = myFocusFollowsMouse
          , clickJustFocuses    = myClickJustFocuses
          , workspaces          = myWorkspaces
          }

---------------------------------------------------------------------------------------------------------------------
-- Applications
---------------------------------------------------------------------------------------------------------------------

myAltBrowser  = "chromium"
myBrowser     = "qutebrowser --backend webengine"
myEditor      = "zsh -c -i code"
myFileManager = "thunar"
myLauncher    = "rofi -show run"
myMailClient  = "thunderbird"
myMusicClient = "spotify"
myResetMouse  = "swarp 0 0"
myScreenLock  = "import -depth 3 -window root /tmp/i3lockscreen.png"
                ++ " && convert -blur 6x6 -colorspace Gray /tmp/i3lockscreen.png /tmp/i3lockscreen.png"
                ++ " && i3lock -i /tmp/i3lockscreen.png"
mySuspend     = myScreenLock ++ " && systemctl suspend"
myTerminal    = "urxvt"

myPlayerToggle = "playerctl play-pause"
myPlayerStop   = "playerctl stop"
myPlayerNext   = "playerctl next"
myPlayerPrev   = "playerctl previous"

-- names for appShifts
myAltBrowserClassName = "Chromium"
myBrowserTitle        = "qutebrowser"
myChatClientTitle     = "TeamSpeak 3"
myMailClientTitle     = "Mozilla Thunderbird"

-- for starup
myInitializeWorkspaces = do
  spawn "urxvt"
  spawn "urxvt"
  spawn "urxvt"
  spawn "qutebrowser --backend webengine"
  spawn "chromium"
  spawn "thunderbird"
  spawn "teamspeak3"

---------------------------------------------------------------------------------------------------------------------
-- KeyBindings
---------------------------------------------------------------------------------------------------------------------

myModMask = mod4Mask

extraKeys =
  [ ((mod4Mask .|. shiftMask .|. controlMask, xK_i), myInitializeWorkspaces)

  -----------------------------------------------------------------------------
  -- Kill apps
  -----------------------------------------------------------------------------
  , ((mod4Mask .|. shiftMask,  xK_c), kill)

  -----------------------------------------------------------------------------
  -- X
  -----------------------------------------------------------------------------
  , ((mod4Mask .|. shiftMask, xK_q), return ())

  -----------------------------------------------------------------------------
  -- Start Applications
  -----------------------------------------------------------------------------
  , ((mod4Mask              , xK_p)     , spawn myLauncher)
  , ((mod4Mask              , xK_b)     , spawn myBrowser)
  , ((mod4Mask              , xK_d)     , spawn myEditor)
  , ((mod4Mask              , xK_f)     , spawn myFileManager)
  , ((mod4Mask              , xK_m)     , spawn myMailClient)
  , ((mod4Mask              , xK_n)     , spawn myMusicClient)
  , ((mod4Mask .|. shiftMask, xK_Return), spawn myTerminal)

  -----------------------------------------------------------------------------
  -- Locking
  -----------------------------------------------------------------------------
  , ((mod4Mask              , xK_s), spawn myScreenLock)
  , ((mod4Mask .|. shiftMask, xK_s), spawn mySuspend)

  -----------------------------------------------------------------------------
  -- Navigation
  -----------------------------------------------------------------------------
  , ((mod4Mask,     xK_Escape), spawn myResetMouse)

  -----------------------------------------------------------------------------
  -- MusicPlayback Control
  -----------------------------------------------------------------------------
  , ((mod4Mask, xK_Home)     , spawn myPlayerToggle)
  , ((mod4Mask, xK_End)      , spawn myPlayerStop)
  , ((mod4Mask, xK_Page_Up)  , spawn myPlayerNext)
  , ((mod4Mask, xK_Page_Down), spawn myPlayerPrev)

  ]

---------------------------------------------------------------------------------------------------------------------
-- Layout
---------------------------------------------------------------------------------------------------------------------

myLayoutHook = smartBorders $ avoidStruts myLayout

myLayout = navi (GridRatio 1) ||| Full
  where navi = configurableNavigation noNavigateBorders

---------------------------------------------------------------------------------------------------------------------
-- Log
---------------------------------------------------------------------------------------------------------------------

myLogHook xmproc = dynamicLogWithPP xmobarPP
                { ppOutput  = hPutStrLn xmproc
                , ppTitle   = xmobarColor "white" ""
                , ppCurrent = xmobarColor "#85c600" ""
                }

---------------------------------------------------------------------------------------------------------------------
-- ManageHooks
---------------------------------------------------------------------------------------------------------------------

myManageHook = appShifts <+> manageFloats <+> manageDocks <+> manageHook defaultConfig

appShifts = composeAll
  [ title     =? myChatClientTitle     --> doShift wTwo
  , title     =? myBrowserTitle        --> doShift wFour
  , className =? myAltBrowserClassName --> doShift wFive
  , title     =? myMailClientTitle     --> doShift wEight
  ]

-- Float exceptions
manageFloats = composeAll $ fullF : [ title =? x --> doFloat | x <- floatTitles ]
floatTitles = []

-- Programs that should start in fullscreen mode. Normally EWMH handles this
-- properly, but eg. mpv does something weird on initial startup so we have to
-- do it manually.
fullF = fmap (\t -> any (`L.isPrefixOf` t) fullTitles) title --> doFullFloat
fullTitles = [ "mpv"]

---------------------------------------------------------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------------------------------------------------------

myHandleEventHook = fullscreenEventHook <+> docksEventHook

-- Fullscreen fixes. For some reason ewmh doesn't advertise _NET_WM_STATE_FULLSCREEN
fullscreenFix :: XConfig a -> XConfig a
fullscreenFix c = c { startupHook = startupHook c <+> setSupportedWithFullscreen }

setSupportedWithFullscreen :: X ()
setSupportedWithFullscreen = withDisplay $ \dpy -> do
  r <- asks theRoot
  a <- getAtom "_NET_SUPPORTED"
  c <- getAtom "ATOM"
  supp <- mapM getAtom [ "_NET_WM_STATE_FULLSCREEN" ]
  io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)

---------------------------------------------------------------------------------------------------------------------
-- StartupHook
---------------------------------------------------------------------------------------------------------------------

myStartupHook = setWMName "LG3D" -- fix Java swing bug

---------------------------------------------------------------------------------------------------------------------
-- Theme
---------------------------------------------------------------------------------------------------------------------

myBorderWidth        = 2
myClickJustFocuses   = False
myFocusedBorderColor = "#85919b"
myFocusFollowsMouse  = False
myNormalBorderColor  = "#0e1112"

---------------------------------------------------------------------------------------------------------------------
-- Workspaces
---------------------------------------------------------------------------------------------------------------------

myWorkspaces = [wOne, wTwo, wThree, wFour, wFive, wSix, wSeven, wEight, wNine]
wOne   = "1:urxvt"
wTwo   = "2:ts"
wThree = "3:urxvt"
wFour  = "4:qute"
wFive  = "5:chromium"
wSix   = "6:-"
wSeven = "7:code"
wEight = "8:comm"
wNine  = "9:-"
