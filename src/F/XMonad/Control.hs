{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module F.XMonad.Control
( fControlStart
) where
  
{- ########################################################################################## -}

import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.Chan
import Control.Monad ((>>=), when)
import Control.Monad.IO.Class (liftIO)

import Data.Int
import Data.Maybe (fromJust)
import Data.Monoid (All(..))
import Data.Text.Lazy (pack)

import DBus.Client

import F.XMonad.Config

import Graphics.X11.Types
import Graphics.X11.Xlib.Atom (internAtom)
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types

import List (transpose)

import XMonad (ScreenId, X, io, kill, layoutHook, restart, sendMessage, setLayout, windows, withFocused)
import XMonad.Actions.CycleWS (nextScreen, swapNextScreen)
import XMonad.Actions.GridSelect (goToSelected)
import XMonad.Core (ScreenId(..), fromMessage, whenJust, withDisplay)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout (ChangeLayout(..), IncMasterN(..), Resize(..))
import XMonad.Layout.IndependentScreens (onCurrentScreen)
import XMonad.Layout.SubLayouts (GroupMsg(..), onGroup, pullGroup)
import XMonad.Layout.LayoutModifier (LayoutModifier(..), ModifiedLayout(..))
import XMonad.Layout.WindowNavigation (Direction2D(..), Navigate(..))
import XMonad.Operations (screenWorkspace)
import qualified XMonad.StackSet as S
import XMonad.Util.EZConfig (mkKeymap)

import System.Exit (ExitCode(..), exitWith)

{- ########################################################################################## -}                                                         
{- # WORKSPACES                                                                             # -}
{- ########################################################################################## -}
{-

x_go dir        = sendMessage $ Go dir
x_merge dir     = sendMessage $ pullGroup dir
x_swap dir      = sendMessage $ Swap dir

x_resetLayout c = setLayout $ layoutHook c -- XMonad.

x_dbind :: String -> [String] -> (Direction2D -> X()) -> [(String, X())]
x_dbind m d f   = [(m ++ key, f dir) | (key, dir) <- zip d (cycle [U, D, L, R])]

     x_dbind "M-"              x_dirKeys x_go
  ++ x_dbind "M-M3-"           x_dirKeys x_swap
  ++ x_dbind "M-M4-"           x_dirKeys x_merge

  -}

{- ########################################################################################## -}                                                         
{- # COMMAND CHANNEL                                                                        # -}
{- ########################################################################################## -}

type CommandChan  = Chan (X ())
data CommandState = CommandState 
  { fsDisplay :: Display
  , fsRoot    :: Window
  , fsAtom    :: Atom
  , fsChan    :: CommandChan
  }

fdInterface :: CommandState -> Object
fdInterface state = let
  met   m = fmMet state ""  $ \_      -> m
  metI  m = fmMet state "i" $ \[a0]   -> m (fromIntegral $ (fromJust $ fromVariant a0 :: Int32))
  metC  m = fmMet state "i" $ \[a0]   -> m (S $ fromIntegral $ ((fromJust $ fromVariant a0) :: Int32))
  metS  m = fmMet state "s" $ \[a0]   -> m (fromJust $ fromVariant a0 :: String)
  in object
  -- Core
  [ ("f.xmonad.core", interface
    [ ("exit",          met  $ io $ exitWith ExitSuccess)
    , ("restart",       met  $ restart "FMonad" True)
    , ("setWMName",     metS $ \name -> setWMName name)
    , ("logBegin",      MemberSignal "")
    , ("logEnd",        MemberSignal "")
    ])
    
  -- Layout
  , ("f.xmonad.layout", interface
    [ ("expand",        met  $ sendMessage Expand)
    , ("shrink",        met  $ sendMessage Shrink)
    , ("next",          met  $ sendMessage NextLayout)
    --, ("prev",       met  $ sendMessage PrevLayout)
    --, ("reset",      met  $ setLayout $ layoutHook conf)
    ])
    
  -- Master
  , ("f.xmonad.master", interface
    [ ("focus",         met  $ windows S.focusMaster)
    , ("mod",           metI $ \d -> sendMessage (IncMasterN d))
    , ("swap",          met  $ windows S.swapMaster)
    ])
    
  -- Navigation
  , ("f.xmonad.nav", interface
    [ ("gridSelect",    met  $ goToSelected $ fcGridConfig)
    ])
    
  -- Screen
  , ("f.xmonad.screen", interface
    [ ("setCurr",       metC $ \sc -> screenWorkspace sc >>= flip whenJust (windows . (S.view)))
    , ("moveWin",       metC $ \sc -> screenWorkspace sc >>= flip whenJust (windows . (S.shift)))
    , ("next",          met  $ nextScreen)
    , ("swapNext",      met  $ swapNextScreen)
    , ("focused",       MemberSignal "ii")
    , ("wkChanged",     MemberSignal "iss")
    ])
    
  -- Tab
  , ("f.xmonad.tab", interface
    [ ("prev",          met  $ onGroup S.focusUp')
    , ("next",          met  $ onGroup S.focusDown')
    , ("unmerge",       met  $ withFocused (sendMessage . UnMerge))
    ])
    
  -- Window
  , ("f.xmonad.win", interface
    [ ("close",         met  $ kill)
    , ("sink",          met  $ withFocused $ windows . S.sink)
    , ("focused",       MemberSignal "iss")
    ])
    
  -- Workspace
  , ("f.xmonad.wk", interface
    [ ("setCurr",       metS $ \wk -> windows $ onCurrentScreen S.greedyView wk)
    , ("setCurrH",      metS $ \wk -> windows $ S.greedyView wk)
    , ("moveWin",       metS $ \wk -> windows $ onCurrentScreen S.shift wk)
    , ("moveWinG",      metS $ \wk -> windows $ S.shift wk)
    , ("focused",       MemberSignal "ixx")
    , ("layoutChanged", MemberSignal "iss")
    ])
  ]

fmMet :: CommandState -> Signature -> ([Variant] -> X ()) -> Member
fmMet s args met = method args "" $ \call -> liftIO $ do
  writeChan (fsChan s) (met $ methodCtxBody call)
  fdSendExposeEvent s
  
{- ########################################################################################## -}
  
fdRequestName :: String -> DBus ()
fdRequestName sfx = requestName name opts err (\r -> return ())
  where name = mkBusName_ . pack $ ("f.XMonad-" ++ sfx)
        opts = [AllowReplacement, ReplaceExisting]
        err  = (\e -> liftIO $ putStrLn "error requesting unique name")

fdLoop :: CommandState -> DBus ()
fdLoop state = do
  fdRequestName "X"
  export "/xmonad" (fdInterface state)
  mainLoop
  
fdSendExposeEvent :: CommandState -> IO ()
fdSendExposeEvent s = allocaXEvent $ \e -> do
  setEventType e clientMessage
  setClientMessageEvent e (fsRoot s) (fsAtom s) 32 0 currentTime
  sendEvent (fsDisplay s) (fsRoot s) False structureNotifyMask e
  flush (fsDisplay s) 

{- ########################################################################################## -}
  
fhEvent :: CommandChan -> Event -> X All
fhEvent chan _ = do
  empty <- io $ isEmptyChan chan
  when (not empty) $ do
    act <- io $ readChan chan
    act
  return (All True)
  
fhStartup :: CommandChan -> X ()
fhStartup chan = withDisplay $ \dpy -> liftIO $ do
  client  <- newClient =<< getSessionBus
  root    <- rootWindow dpy $ defaultScreen dpy
  atom    <- internAtom dpy "XMONAD_PING" False
  putStrLn $ "Connected as: " ++ show (clientName client)
  forkOS $ runDBus client $ fdLoop $ CommandState 
    { fsDisplay = dpy
    , fsRoot    = root
    , fsAtom    = atom
    , fsChan    = chan
    }
  return ()
  
fControlStart :: IO (Event -> X All, X ())
fControlStart = do 
  chan <- newChan     
  return (fhEvent chan, fhStartup chan)
  
{- ########################################################################################## -}
