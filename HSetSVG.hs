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
import Control.Monad.State (evalState)
import HSetCore
import HSetGenSVG
import System.Random (mkStdGen,randomIO)
import Data.List (delete)

-- this should be done with Parsec
-- but I don't remember the syntax
-- so instead we hack it
pInput i0 d n =
  let nf = if length d > n then n else length d
      ncl = ['0'..'9']
      dnl = dropWhile (`notElem` ncl)
      snl = span (`elem` ncl)
      zzz = \(x,y) -> ((read x) `mod` nf, y)
      i1 = dnl i0
      (n1,i2) = zzz $ snl i1
      i3 = dnl i2
      (n2,i4) = zzz $ snl i3
      i5 = dnl i4
      (n3,i6) = zzz $ snl i5
  in (d !! n1, d !! n2, d !! n3)

doTable [] _ = putStrLn "Congratulations!  You win."
doTable d n | (n <= length d) && (length (identifySets (take n d)) == 0) = doTable d (n+3)
            | take 1 (identifySets d) == [] = putStrLn "No more sets."
            | otherwise = do
    writeFile "set.svg" $ svgHeader ++ showCards (take n d) ++ svgTail
    putStr "Set (comma delimited): "
    inpStr <- getLine
    case head inpStr of
     'r' -> doTable d (n+3)
     'c' -> do
        putStrLn $ "Cheater!\n"++(show $ identifySets (take n d))
        doTable d n
     _   -> do
        let (c1,c2,c3) = pInput inpStr d n
        if isSet c1 c2 c3 then do 
                                putStrLn "Correct!"
                                if n == 12 && length d > 12
                                 then doTable 
                                  (replaceCards [c1,c2,c3] ((drop n).(take (n+3)) $ d)
                                    ((take n d)++(drop (n+3) d)))
                                  12
                                 else doTable 
                                  ((delete c1).(delete c2).(delete c3) $ d)
                                  (min (n-3) (length d))
                        else do putStrLn "Sorry."
                                doTable d n

main = do
    sg <- liftM mkStdGen randomIO
    let theDeck = evalState mkRandDeck sg
    doTable theDeck 12
