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
import qualified XMonad.Layout.BinarySpacePartition as BSP
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
  spawn "mkfifo /tmp/mpvfifo"
  xmproc <- spawnPipe "xmobar"
  xmonad
    $ withUrgencyHook NoUrgencyHook
    $ fullscreenFix
    $ myConfig xmproc `additionalKeys` extraKeys

myConfig pipe = def
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
myBrowser     = "vivaldi-stable"--"qutebrowser --backend webengine"
myEditor      = "zsh -c -i code"
myFileManager = "thunar"
myLauncher    = "rofi -show run"
myMailClient  = "thunderbird"
myResetMouse  = "swarp 0 0"
myScreenLock  = "i3lock -i ~/Pictures/hitagi/hitagiScreenSaver.png -t"
mySuspend     = myScreenLock ++ " && systemctl suspend"
myTerminal    = "urxvt"

myPlayerToggle = "echo 'cycle pause\n' > /tmp/mpvfifo"
myPlayerNext   = "echo 'add chapter 1\n' > /tmp/mpvfifo"
myPlayerPrev   = "echo 'add chapter -1\n' > /tmp/mpvfifo"
myPlayerNextPL = "echo 'playlist_next force\n' > /tmp/mpvfifo"
myPlayerAddVol = "echo 'add volume 1\n' > /tmp/mpvfifo"
myPlayerSubVol = "echo 'add volume -1\n' > /tmp/mpvfifo"
myPlayerAddD   = "echo 'seek 5 exact\n' > /tmp/mpvfifo"
myPlayerSubD   = "echo 'seek -5 exact\n' > /tmp/mpvfifo"
myPlayerAddMD  = "echo 'seek 60\n' > /tmp/mpvfifo"
myPlayerSubMD  = "echo 'seek -60\n' > /tmp/mpvfifo"

myPlayer2Toggle= "playerctl play-pause"
myPlayer2Stop  = "playerctl stop"
myPlayer2Next  = "playerctl next"
myPlayer2Prev  = "playerctl previous"

-- names for appShifts
myAltBrowserClassName = "Chromium"
myBrowserTitle        = "vivaldi-stable"--"qutebrowser"
myChatClientTitle     = "TeamSpeak 3"
myMailClientTitle     = "Mozilla Thunderbird"

-- for starup
myInitializeWorkspaces = do
  spawn myTerminal
  spawn myTerminal
  spawn myTerminal
  spawn myBrowser
  spawn myAltBrowser
  spawn myMailClient
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
  -- BSP control
  -----------------------------------------------------------------------------
  , ((mod4Mask .|. controlMask, xK_l), sendMessage $ BSP.ExpandTowards R)
  , ((mod4Mask .|. controlMask, xK_h), sendMessage $ BSP.ExpandTowards L)
  , ((mod4Mask .|. controlMask, xK_j), sendMessage $ BSP.ExpandTowards D)
  , ((mod4Mask .|. controlMask, xK_k), sendMessage $ BSP.ExpandTowards U)
  , ((mod4Mask                , xK_y), sendMessage BSP.Rotate)
  , ((mod4Mask .|. controlMask, xK_y), sendMessage BSP.Swap)

  -----------------------------------------------------------------------------
  -- Start Applications
  -----------------------------------------------------------------------------
  , ((mod4Mask              , xK_p)     , spawn myLauncher)
  , ((mod4Mask              , xK_b)     , spawn myBrowser)
  , ((mod4Mask              , xK_d)     , spawn myEditor)
  , ((mod4Mask              , xK_f)     , spawn myFileManager)
  , ((mod4Mask              , xK_m)     , spawn myMailClient)
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
  -------------------------------------------------------------------------
  -- MPV
  -------------------------------------------------------------------------
  -- state
  , ((mod4Mask, xK_Home)     , spawn myPlayerToggle)
  , ((mod4Mask, xK_Page_Up)  , spawn myPlayerNext)
  , ((mod4Mask, xK_Page_Down), spawn myPlayerPrev)
  , ((mod4Mask, xK_Return)   , spawn myPlayerNextPL)
  -- volume
  , ((mod4Mask, xK_equal)    , spawn myPlayerAddVol)
  , ((mod4Mask, xK_minus)    , spawn myPlayerSubVol)
  -- seek
  , ((mod4Mask, xK_Right)    , spawn myPlayerAddD)
  , ((mod4Mask, xK_Left)     , spawn myPlayerSubD)
  , ((mod4Mask, xK_Up  )     , spawn myPlayerAddMD)
  , ((mod4Mask, xK_Down)     , spawn myPlayerSubMD)

  -------------------------------------------------------------------------
  -- playerctl
  -------------------------------------------------------------------------
  -- state
  , ((mod4Mask .|. shiftMask, xK_Home)     , spawn myPlayer2Toggle)
  , ((mod4Mask .|. shiftMask, xK_Page_Up)  , spawn myPlayer2Next)
  , ((mod4Mask .|. shiftMask, xK_Page_Down), spawn myPlayer2Prev)
  ]

---------------------------------------------------------------------------------------------------------------------
-- Layout
---------------------------------------------------------------------------------------------------------------------

myLayoutHook = smartBorders $ avoidStruts myLayout

myLayout = navi BSP.emptyBSP ||| Full
  where navi = configurableNavigation noNavigateBorders

---------------------------------------------------------------------------------------------------------------------
-- Log
---------------------------------------------------------------------------------------------------------------------

myLogHook xmproc = dynamicLogWithPP xmobarPP
                { ppOutput  = hPutStrLn xmproc
                , ppTitle   = xmobarColor "white" ""
                , ppCurrent = xmobarColor "#85c600" ""
                , ppSep     = "  "
                }

---------------------------------------------------------------------------------------------------------------------
-- ManageHooks
---------------------------------------------------------------------------------------------------------------------

myManageHook = appShifts <+> manageFloats <+> manageDocks <+> manageHook def

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
wOne   = "1:\x269D"
wTwo   = "2:\x1F5E9"
wThree = "3:\x269D"
wFour  = "4:\x1F30E"
wFive  = "5:\x1F30E"
wSix   = "6:\x1F5C0"
wSeven = "7:\x1F6E0"
wEight = "8:\x1F582"
wNine  = "9:\x1F39C"
