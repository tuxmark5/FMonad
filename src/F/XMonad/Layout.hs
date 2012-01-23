module F.XMonad.Layout
( fcLayout0
, fcLayout1
) where
  
import F.XMonad.Config

import XMonad.Hooks.ManageDocks (avoidStruts)

import XMonad.Layout ((|||), Full(..), Mirror(..), Tall(..))
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.LayoutCombinators ((*|***), (****||*))
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz, reflectVert)
import XMonad.Layout.Renamed (Rename(..), renamed)
import XMonad.Layout.Simplest (Simplest(..))
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.SubLayouts (subLayout)
import XMonad.Layout.Tabbed (addTabs, shrinkText)
import XMonad.Layout.WindowNavigation (windowNavigation)
  
{- ########################################### F+ ########################################### -}
{- #                                         layout                                         # -}
{- ########################################################################################## -}

flStandard n l  = avoidStruts $ renamed [CutWordsLeft 1, Replace n] $ l
flSubTabbed l   = addTabs shrinkText fcTabTheme $ subLayout [] Simplest l

flTiled         = Tall numMaster delta ratio
  where
    numMaster   = 1
    ratio       = 1/2
    delta       = 1/16

flRotWest   l   = l
flRotNorth  l   = Mirror l
flRotEast   l   = reflectHoriz l
flRotSouth  l   = reflectVert $ Mirror l

flConky         = flStandard  "[CC]"  (Full *|*** Grid)
flIM            = flStandard  "[IM]"  (Grid ****||* Full)

flGrid          = flStandard  "GRID"        (Grid)
flFull          = renamed [Replace "FULL"]  (noBorders Full)
flFloat         = flStandard  "<><"         (simplestFloat)
flNEWS          =
  (   flStandard "[W]=" (flRotWest  flTiled) 
  ||| flStandard "[N]=" (flRotNorth flTiled)
  ||| flStandard "[E]=" (flRotEast  flTiled)
  ||| flStandard "[S]=" (flRotSouth flTiled)
  )

flDefault       = flNEWS ||| flGrid ||| flFull ||| flFloat
flLayout l      =
  windowNavigation  $ --  nameTail          $
  flSubTabbed       $ l

------------------------------------------------------------------------------------------------

fcLayout0       = flLayout $
  onWorkspace "0_A" (flIM   ||| flNEWS) $
  onWorkspace "0_C" (flFull ||| flNEWS ||| flGrid  ) $
  flDefault

fcLayout1       = flLayout $
  onWorkspace "0_1" (flConky) $
  flDefault
  
{- ########################################################################################## -}
