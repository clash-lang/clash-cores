{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
--  Copyright   :  (C) 2024, Hoang Minh Le <minhxecole@gmail.com>
--  License     :  BSD2
--  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--  This module contains example about InfoEntity used in the
--  module FloPoCoExample
module Clash.Cores.ClashFloPoCo.Help
  ( infoEnPlusExample,
    infoEnFMAExample,
    infoEnExpExample,
    infoEnVGAControllerExample,
  )
where

import Clash.Cores.ClashFloPoCo.GenTemDSL
import Clash.Cores.ClashFloPoCo.Helpargs
import Clash.Cores.ClashFloPoCo.InfoEn
import Prelude

-- | This InfoEntity is used for blackbox of the plusFloatExample
--
--  ==== __Example:__
--  > infoEnPlusExample = $(genFloPoCoInfoEntity floPoCoPathExample argsPlusExample filePlusExample)
infoEnPlusExample :: InfoEntity
infoEnPlusExample =
  InfoEntity
    { name = Just "plusFloat",
      freq = Just 100,
      pipedep = Just 2,
      insig = Just ["clk", "X", "Y"],
      outsig = Just ["R"]
    }

-- | This InfoEntity is used for blackbox of the fmaFloatExample
--
--  ==== __Example:__
--  > infoEnFMAExample = $(genFloPoCoInfoEntity floPoCoPathExample argsFMAExample fileFMAExample)
infoEnFMAExample :: InfoEntity
infoEnFMAExample =
  InfoEntity
    { name = Just "fmaFloat",
      freq = Just 100,
      pipedep = Just 1,
      insig = Just ["clk", "A", "B", "C", "negateAB", "negateC", "RndMode"],
      outsig = Just ["R"]
    }

-- | This InfoEntity is used for blackbox of the expFloatExample
--
--  ==== __Example:__
--  > infoEnExpExample = $(genFloPoCoInfoEntity floPoCoPathExample argsExpExample fileExpExample)
infoEnExpExample :: InfoEntity
infoEnExpExample =
  InfoEntity
    { name = Just "expFloat",
      freq = Just 100,
      pipedep = Just 3,
      insig = Just ["clk", "X"],
      outsig = Just ["R"]
    }

-- | This InfoEntity is used for blackbox of the vga_controller
infoEnVGAControllerExample :: InfoEntity
infoEnVGAControllerExample =
  InfoEntity
    { name = Just "vga_controller",
      freq = Just 100,
      pipedep = Nothing,
      insig = Just ["clk_100MHz", "reset"],
      outsig = Just ["video_on", "hsync", "vsync", "p_tick", "x", "y"]
    }
