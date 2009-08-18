{-# LANGUAGE CPP #-}

{- Copyright (C) 2009 Riad S Wahby <rsw@jfet.org>
 - 
--  This file is part of hset
--
--  hset is free software.  It comes without any warranty, to
--  to the extent permitted by applicable law.  You can redistribute it
--  and/or modify it under the terms of the Do What The Fuck You Want To
--  Public License, Version 2, as published by Sam Hocevar.  See
--  the COPYING file or http://sam.zoy.org/wtfpl/COPYING for more details
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
import qualified Data.Digest.MD5 as MD5
import qualified Codec.Binary.BubbleBabble as BuBa
import Data.Char (ord)

#ifndef __SERVER_SECRET_KEY__
#define __SERVER_SECRET_KEY__ "asdf"
#endif

serverSecretKey = __SERVER_SECRET_KEY__

onClick = strAttr "onclick"

clickScript o cx = "document.sform."++cx++".checked = ! document.sform."++cx++".checked;"

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
		  form ! [method "POST", name "sform"]
                       << ( hidden "setDeck" dc : 
                            hidden "numCards" nc :
                            hidden "verToken" ( vtok d n ) :
                            cardsOnTable ++
                            [br +++ submit "" "Submit"] )
    where dc = show d
          nc = show n
          cardsOnTable = everyThree br $ map cardHTML $ zip [0..] (take n d)
          cardHTML (x,c) = (image ! [src oImg,
			             onClick $ clickScript oImg cx])
                            +++ spaceHtml +++ checkbox cx "c"
              where oImg = "/~kwantam/images/cards/" ++ show c ++ ".png"
                    cx = 'c':show x
	  instr = "Choose a set, or just hit Submit if you think there aren't any."

takeChecked _  [] = []
takeChecked [] _  = []
takeChecked (d:dc) (r:rc) = if r == "c"
                             then d : takeChecked dc rc
                             else takeChecked dc rc

vtok d n = BuBa.encode $ MD5.hash $ map (fromIntegral.ord) $ show (serverSecretKey,d,n)

showGreeting n = writePage dd 12 "Welcome!"
    where dd = compressDeck $ evalState mkRandDeck n

cgiMain n = do nc <- liftM (read.(fromMaybe "0")) $ getInput "numCards"
               dc <- liftM (read.(fromMaybe "[]")) $ getInput "setDeck"
               rz <- mapM ((liftM $ fromMaybe "[]").getInput) $ take nc (map (('c':).show) [0..])
               vt <- liftM (read.(fromMaybe "")) $ getInput "verToken"
               if nc == 0 || vt /= vtok dc nc
                then output $ renderHtml $ body << (showGreeting n)
                else output $ renderHtml $ body << (newPage dc nc $ takeChecked dc rz)

main = do sg <- liftM mkStdGen randomIO
          runCGI $ handleErrors (cgiMain sg)
