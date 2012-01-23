{-# LANGUAGE NoMonomorphismRestriction #-}

module F.XMonad.Config
( fcGridConfig
, fcTabTheme
, fcWorkspaces
) where

import qualified Data.Map as M (fromList, map, singleton, unions)
import XMonad (xK_space, xK_a, xK_d, xK_s, xK_w)
import XMonad.Actions.GridSelect
import XMonad.Layout.Tabbed (Theme(..)) 

{- ########################################################################################## -}                                                         
{- # GRIDSELECT CONFIG                                                                      # -}
{- ########################################################################################## -}

fcGridConfig = defaultGSConfig
{-
  {
    gs_cellwidth      = 100,
    gs_cellheight     = 30,
    gs_navigate       = M.unions
      [
        reset,
        navKeys,
        gs_navigate $ defaultGSConfig `asTypeOf` fcGridConfig
      ]
  }
  where
    addPair (a, b) (x, y) = (a + x, b + y)
    navKeys = M.map addPair $ M.fromList
      [
        ((0, xK_a), (-1,  0)),
        ((0, xK_d), ( 1,  0)),
        ((0, xK_w), ( 0, -1)),
        ((0, xK_s), ( 0,  1))
      ]
      reset = M.singleton (0, xK_space) (const (0, 0))-}

{- ########################################################################################## -}                                                         
{- # TAB THEME                                                                              # -}
{- ########################################################################################## -}

fcTabTheme = Theme 
  { 
    activeColor         = "#999999",
    inactiveColor       = "#666666",
    urgentColor         = "#FFFF00",
    activeBorderColor   = "#FFFFFF",
    inactiveBorderColor = "#BBBBBB",
    urgentBorderColor   = "#00FF00",
    activeTextColor     = "#FFFFFF",
    inactiveTextColor   = "#BFBFBF",
    urgentTextColor     = "#FF0000",
    fontName            = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*",
    decoWidth           = 200,
    decoHeight          = 16
  }

{- ########################################################################################## -}                                                         
{- # WORKSPACES                                                                             # -}
{- ########################################################################################## -}

fcWorkspaces  = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C"]

{- ########################################################################################## -}
