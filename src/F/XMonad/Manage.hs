module F.XMonad.Manage 
( fcManage0
, fcManage1
) where

import XMonad.ManageHook

{- ########################################### F+ ########################################### -}
{- #                                    Window Managament                                   # -}
{- ########################################################################################## -}

fcManage0 = composeAll
  [ className =? "Skype"              --> doShift "0_A"
  , className =? "Guayadeque"         --> doShift "0_B"
  , className =? "Rhythmbox"          --> doShift "0_B"
  , className =? "Smplayer"           --> doShift "0_C"

  , className =? "Firefox"            --> doShift "1_1"
  , className =? "Opera"              --> doShift "1_2"
  , className =? "Transmission-Gtk"   --> doShift "1_4"

  , title     =? "Downloads"          --> doFloat
  , appName   =? "Komodo_find2"       --> doFloat
  , className =? "SmartGit"           --> doFloat
  , className =? "stalonetray"        --> doIgnore
  , title     =? "xfce4-notifyd"      --> doIgnore
  , title     =? "xfce4-panel"        --> doIgnore
  ]
  
------------------------------------------------------------------------------------------------

fcManage1 = composeAll
  [ title     =? "chat-1"             --> doShift "0_2"
  , className =? "Skype"              --> doShift "0_A"
  , title     =? "rgs-1"              --> doShift "0_4"
  ]
  
{- ########################################################################################## -}
