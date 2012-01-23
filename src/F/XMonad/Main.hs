module F.XMonad.Main
( fmain
) where
  
import Control.Concurrent (forkOS)
  
import DBus.Client
  (clientName, getSessionBus, mainLoop, newClient, runDBus)
--import DBus.Constants

import F.XMonad.Config
import F.XMonad.Control
import F.XMonad.Layout
import F.XMonad.Manage

import Graphics.X11.Xlib.Misc (initThreads)

import Network.BSD (getHostName)

import XMonad (Window, mod5Mask, xmonad)
import XMonad.Config (defaultConfig)
import XMonad.Core (SomeMessage(..), XConfig(..))

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhDesktopsStartup)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.UrgencyHook 
  (NoUrgencyHook(..), withUrgencyHook)
  
import XMonad.Layout.IndependentScreens 
  (PhysicalWorkspace, VirtualWorkspace, countScreens, withScreens)
  
import XMonad.Util.EZConfig (mkKeymap)

import System.Environment (getEnv)

{- ########################################################################################## -}                                                         
{- # TAB THEME                                                                              # -}
{- ########################################################################################## -}

fmonad lHook mHook = do
  (eHook, startup)      <- fControlStart
  numScreens            <- countScreens

  initThreads
  xmonad $ 
    withUrgencyHook NoUrgencyHook $ 
    ewmh defaultConfig
    { focusFollowsMouse = True
    , handleEventHook   = eHook
    , keys              = (\x -> mkKeymap x [])
    , layoutHook        = lHook
    , logHook           = return ()
    , manageHook        = mHook
    , modMask           = mod5Mask
    , startupHook       = startup >> ewmhDesktopsStartup >> setWMName "LG3D"
    , workspaces        = withScreens numScreens fcWorkspaces
    }

{- ########################################################################################## -}

fmain :: IO ()
fmain = do
  host <- getHostName
  dpy  <- getEnv "DISPLAY"
  
  case (host, dpy !! 1) of
    ("east",  '5' ) -> fmonad fcLayout1 fcManage1
    ("east",  _   ) -> fmonad fcLayout0 fcManage0
    (_,       _   ) -> return ()

{- ########################################################################################## -}
