{- Copyright (C) 2009 Riad S Wahby <rsw@jfet.org>
 - 
-- This file is part of hset
--
--  hset is free software.  It comes without any warranty, to
--  to the extent permitted by applicable law.  You can redistribute it
--  and/or modify it under the terms of the Do What The Fuck You Want To
--  Public License, Version 2, as published by Sam Hocevar.  See
--  the COPYING file or http://sam.zoy.org/wtfpl/COPYING for more details
 -
 -}

module Main where

import Control.Monad (liftM)
import Control.Monad.State (evalState,runState)
import HSetCore
import System.Random (mkStdGen,randomIO)
import Data.List (delete)

doTable _ _ [] = 0
doTable f n d  = if sSort == []
                  then if n >= (length d) then (length d) else doTable f (n+3) d
                  else doTable f nextN nextD
    where sSort = f (take n d)
          nextN = if n > 12 then n-3 else 12
          nextD = deleteSet d $ head sSort

mkRandDecks 0 lz _ = lz
mkRandDecks n lz sg = mkRandDecks (n-1) (d:lz) sn
    where (d,sn) = runState mkRandDeck sg

main = do
    sg <- liftM mkStdGen randomIO
    let theDecks = mkRandDecks 1000 [] sg
    let theResults1 = map (doTable setsMostDis 12) theDecks
    let theResults2 = map (doTable setsLeastDis 12) theDecks
    putStrLn.show $ sum theResults1
    putStrLn.show $ sum theResults2
