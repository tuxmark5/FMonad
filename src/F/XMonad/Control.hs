{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module F.XMonad.Control
( fControlStart
) where
  
{- ########################################################################################## -}

import            Control.Concurrent (forkIO, forkOS)
import            Control.Concurrent.Chan
import            Control.Monad ((>>=), when)
import            Control.Monad.IO.Class (liftIO)

import            Data.Int
import            Data.Maybe (fromJust)
import            Data.Monoid (All(..))
import            Data.Text (pack)

import            DBus.Client (Client(..), Method(..))
import            DBus.Client.Simple (AutoReply(..), AutoSignature(..), RequestNameFlag (..), connectSession, export, method, 
                  requestName)
import            DBus.Introspection (Object(..))
import            DBus.Types 

import            F.XMonad.Config

import            Graphics.X11.Types
import            Graphics.X11.Xlib.Atom (internAtom)
import            Graphics.X11.Xlib.Display
import            Graphics.X11.Xlib.Event
import            Graphics.X11.Xlib.Extras
import            Graphics.X11.Xlib.Types

import            List (transpose)

import            XMonad (ScreenId, X, io, kill, layoutHook, restart, sendMessage, setLayout, 
                  windows, withFocused)
import            XMonad.Actions.CycleWS (nextScreen, swapNextScreen)
import            XMonad.Actions.GridSelect (goToSelected)
import            XMonad.Core (ScreenId(..), fromMessage, whenJust, withDisplay)
import            XMonad.Hooks.SetWMName (setWMName)
import            XMonad.Layout (ChangeLayout(..), IncMasterN(..), Resize(..))
import            XMonad.Layout.IndependentScreens (onCurrentScreen)
import            XMonad.Layout.SubLayouts (GroupMsg(..), onGroup, pullGroup)
import            XMonad.Layout.LayoutModifier (LayoutModifier(..), ModifiedLayout(..))
import            XMonad.Layout.WindowNavigation (Direction2D(..), Navigate(..))
import            XMonad.Operations (screenWorkspace)
import qualified  XMonad.StackSet as S
import            XMonad.Util.EZConfig (mkKeymap)

import            System.Exit (ExitCode(..), exitWith)

{- ########################################################################################## -}
{- # WORKSPACES                                                                             # -}
{- ########################################################################################## -}

-- x_resetLayout c = setLayout $ layoutHook c -- XMonad.

toDir :: Int32 -> Direction2D
toDir 0 = L
toDir 1 = R
toDir 2 = U
toDir 3 = D

{- ########################################################################################## -}
{- # COMMAND CHANNEL                                                                        # -}
{- ########################################################################################## -}

type CommandChan  = Chan (X ())
data CommandState = CommandState 
  { fsClient  :: Client
  , fsDisplay :: Display
  , fsRoot    :: Window
  , fsAtom    :: Atom
  , fsChan    :: CommandChan
  }

--method' :: (AutoSignature fun, AutoReply fun) => InterfaceName -> MemberName -> fun -> Method
--method' ifc mbr fun = 

instance IsVariant Int where
  fromVariant v = maybe Nothing (Just . fromIntegral) $ (fromVariant v :: Maybe Int32)
  toVariant   a = toVariant $ (fromIntegral a :: Int32)
instance IsVariant ScreenId where
  fromVariant v = maybe Nothing (Just . fromIntegral) $ (fromVariant v :: Maybe Int32)
  toVariant   a = toVariant $ (fromIntegral a :: Int32)

fdInterface :: CommandState -> [Method]
fdInterface state =
  [ method "f.xmonad.core"    "exit"       $          mIO $ exitWith ExitSuccess
  , method "f.xmonad.core"    "restart"    $          mX  $ restart "fmonad" True
  , method "f.xmonad.core"    "setWMName"  $ \name -> mX  $ setWMName name
  , method "f.xmonad.layout"  "expand"     $          mX  $ sendMessage Expand
  , method "f.xmonad.layout"  "shrink"     $          mX  $ sendMessage Shrink
  , method "f.xmonad.layout"  "next"       $          mX  $ sendMessage NextLayout
  --, ("prev",       met  $ sendMessage PrevLayout)
  --, ("reset",      met  $ setLayout $ layoutHook conf)
  , method "f.xmonad.master"  "focus"      $          mX  $ windows S.focusMaster
  , method "f.xmonad.master"  "mod"        $ \d ->    mX  $ sendMessage (IncMasterN $ fromIntegral (d :: Int32))
  , method "f.xmonad.master"  "swap"       $          mX  $ windows S.swapMaster
  , method "f.xmonad.nav"     "gridSelect" $          mX  $ goToSelected $ fcGridConfig
  , method "f.xmonad.nav"     "move"       $ \d ->    mX  $ sendMessage $ Go $ toDir d
  , method "f.xmonad.nav"     "swap"       $ \d ->    mX  $ sendMessage $ Swap $ toDir d
  , method "f.xmonad.screen"  "setCurr"    $ \sc ->   mX  $ screenWorkspace (fromIntegral (sc :: Int32)) >>= flip whenJust (windows . (S.view))
  , method "f.xmonad.screen"  "moveWin"    $ \sc ->   mX  $ screenWorkspace (fromIntegral (sc :: Int32)) >>= flip whenJust (windows . (S.shift))
  , method "f.xmonad.screen"  "next"       $          mX  $ nextScreen
  , method "f.xmonad.screen"  "swapNext"   $          mX  $ swapNextScreen
  , method "f.xmonad.tab"     "merge"      $ \d ->    mX  $ sendMessage $ pullGroup $ toDir d
  , method "f.xmonad.tab"     "prev"       $          mX  $ onGroup S.focusUp'
  , method "f.xmonad.tab"     "next"       $          mX  $ onGroup S.focusDown'
  , method "f.xmonad.tab"     "unmerge"    $          mX  $ withFocused (sendMessage . UnMerge)
  , method "f.xmonad.win"     "close"      $          mX  $ kill
  , method "f.xmonad.win"     "sink"       $          mX  $ withFocused $ windows . S.sink
  , method "f.xmonad.wk"      "setCurr"    $ \wk ->   mX  $ windows $ onCurrentScreen S.greedyView wk
  , method "f.xmonad.wk"      "setCurrH"   $ \wk ->   mX  $ windows $ S.greedyView wk
  , method "f.xmonad.wk"      "moveWin"    $ \wk ->   mX  $ windows $ onCurrentScreen S.shift wk
  , method "f.xmonad.wk"      "moveWinG"   $ \wk ->   mX  $ windows $ S.shift wk
  ]
  where mX      = forwardCall state
        mIO a   = mX $ io a

  {-

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
  -}
{-fmMet :: CommandState -> Signature -> ([Variant] -> X ()) -> Member
fmMet s args met = method args "" $ \call -> liftIO $ do
  writeChan (fsChan s) (met $ methodCtxBody call)
  fdSendExposeEvent s
  -}
{- ########################################################################################## -}
  
forwardCall :: CommandState -> X () -> IO ()
forwardCall state m = do
  writeChan (fsChan state) m
  fdSendExposeEvent state
  
fdRequestName :: Client -> String -> IO ()
fdRequestName cl sfx = requestName cl name opts >> return ()
  where name = busName_ . pack $ ("f.xmonad" ++ sfx)
        opts = [AllowReplacement, ReplaceExisting]

fdLoop :: CommandState -> IO ()
fdLoop state = do
  fdRequestName (fsClient state) ""
  export (fsClient state) "/xmonad" (fdInterface state)
  
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
  client  <- connectSession
  root    <- rootWindow dpy $ defaultScreen dpy
  atom    <- internAtom dpy "XMONAD_PING" False
  putStrLn $ "Connected as: " ++ show "??"
  fdLoop $ CommandState 
    { fsClient  = client
    , fsDisplay = dpy
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
