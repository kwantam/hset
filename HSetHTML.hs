
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

import HSetCore
import HSetCompress
import Control.Monad (mapM,liftM)
import Network.CGI
import Text.XHtml
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Control.Monad.State (evalState)
import System.Random (mkStdGen,randomIO)

onClick = strAttr "onClick"

clickScript o n cx = "if (this.src == "++o++") { this.src = "++n++"; "++cx++".checked=true; } else { this.src = "++o++"; "++cx++".checked=false; }"

newPage [] _ _ = endGame "You win!"
newPage d  n r | take 1 (identifySets $ decompressDeck d) == [] = endGame "No more sets."
               | take 1 (identifySets $ decompressDeck (take n d)) == [] = newPage d (n+3) r
               | otherwise = if isSetResponse $ decompressDeck r 
                              then if n==12 && length d > 12
                                    then writePage (replaceCards r ((drop n).(take (n+3)) $ d)
                                                                   ((take n d)++(drop (n+3) d)))
                                                   12 "Correct!"
                                    else writePage (d \\ r) (min (n-3) (length d)) "Correct!"
                              else writePage d n "Sorry, try again."

endGame s = h1 << s +++ form << [paragraph << submit "" "Play again?"]

isSetResponse (r1:r2:r3:[]) = isSet r1 r2 r3
isSetResponse _             = False

everyThree k (a:b:c:ds) = a:b:c:k:everyThree k ds
everyThree _ ds         = ds

writePage d n m = h3 << m +++ 
                  paragraph << instr +++ 
		  form ! [method "POST"]
                       << ( hidden "setDeck" dc : 
                            hidden "numCards" nc :
                            cardsOnTable ++
                            [br +++ submit "" "Submit"] )
    where dc = show d
          nc = show n
          cardsOnTable = everyThree br $ map cardHTML $ zip [0..] (take n d)
          cardHTML (x,c) = (image ! [src oImg, onClick $ clickScript oImg nImg cx])
                           +++ spaceHtml +++ checkbox cx "c"
              where oImg = "/~kwantam/images/cards/" ++ show c ++ ".png"
                    nImg = "/~kwantam/images/cards/" ++ show c ++ "r.png"
                    cx = 'c':show x
	  instr = "Choose a set, or just hit Submit if you think there aren't any."

takeChecked _  [] = []
takeChecked [] _  = []
takeChecked (d:dc) (r:rc) = if r == "c"
                             then d : takeChecked dc rc
                             else takeChecked dc rc

showGreeting n = writePage dd 12 "Welcome!"
    where dd = compressDeck $ evalState mkRandDeck n

cgiMain n = do nc <- liftM (read.(fromMaybe "0")) $ getInput "numCards"
               dc <- liftM (read.(fromMaybe "[]")) $ getInput "setDeck"
               rz <- mapM ((liftM $ fromMaybe "[]").getInput) $ take nc (map (('c':).show) [0..])
               if nc == 0
                then output $ renderHtml $ body << (showGreeting n)
                else output $ renderHtml $ body << (newPage dc nc $ takeChecked dc rz)

main = do sg <- liftM mkStdGen randomIO
          runCGI $ handleErrors (cgiMain sg)
