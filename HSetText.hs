{- Copyright (C) 2009 Riad S Wahby <rsw@jfet.org>
 - 
## This file is part of hset
##
##  hset is free software.  It comes without any warranty, to
##  to the extent permitted by applicable law.  You can redistribute it
##  and/or modify it under the terms of the Do What The Fuck You Want To
##  Public License, Version 2, as published by Sam Hocevar.  See
##  the COPYING file or http://sam.zoy.org/wtfpl/COPYING for more details
 -
 -}

module Main where

import Control.Monad (liftM)
import Control.Monad.State (evalState)
import HSetCore
import System.Random (mkStdGen,randomIO)
import Data.List (delete)

pInput i0 d = 
  let ncl = ['0'..'9']
      dnl = dropWhile (`notElem` ncl)
      snl = span (`elem` ncl)
      i1 = dnl i0
      (n1,i2) = snl i1
      i3 = dnl i2
      (n2,i4) = snl i3
      i5 = dnl i4
      (n3,i6) = snl i5
      d12 = take 12 d
  in (d12 !! read n1, d12 !! read n2, d12 !! read n3)

doTable [] = putStrLn "Congratulations!  You win."
doTable d  = do
    putStrLn $ unlines.(map show) $ zip [0..11] (take 12 d)
    putStr "Set (comma delimited): "
    inpStr <- getLine
    let (n1,n2,n3) = pInput inpStr d
    if isSet n1 n2 n3 then do putStrLn "Correct!"
                              doTable ((delete n1).(delete n2).(delete n3) $ d)
                      else do putStrLn "Sorry."
                              doTable d

main = do
    sg <- liftM mkStdGen randomIO
    let theDeck = evalState mkRandDeck sg
    doTable theDeck
