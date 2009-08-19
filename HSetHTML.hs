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
import System.Time (getClockTime,ClockTime(..))

#ifndef __SERVER_SECRET_KEY__
#define __SERVER_SECRET_KEY__ "asdf"
#endif

-- directory where images are stored
imgDir = "/~kwantam/images/cards/"

-- extract seconds from a ClockTime
getClockSeconds :: (Integral a) => ClockTime -> a
getClockSeconds (TOD a _) = fromIntegral a

-- this gets set either in the ifndef above or (preferably)
-- during build with the -D commandline switch
serverSecretKey = __SERVER_SECRET_KEY__

-- construct the "onClick" attribute, e.g., for <img> tags
onClick = strAttr "onclick"

-- JavaScript for (de)activating checkbox when image is clicked
clickScript o cx = "document.sform."++cx++".checked = ! document.sform."++cx++".checked;"

-- kind of the central thingie
-- handle inputs from the user, dispatch display, update the deck, et cetera
newPage [] _ s t ct _ = endGame "You win!" s t ct
newPage d  n s t ct r
        | take 1 (identifySets $ decompressDeck d) == [] = endGame "No more sets." s t ct
        | take 1 (identifySets $ decompressDeck (take n d)) == [] = newPage d (n+3) s t ct [-1]
        | otherwise = if r == [-1] then writePage d n "Here, have some more cards." s t ct else
                       if isSetResponse $ decompressDeck r 
                        then if n==12 && length d > 12
                              then writePage (replaceCards r ((drop n).(take (n+3)) $ d)
                                                             ((take n d)++(drop (n+3) d)))
                                             12 "Correct!" (s+1) t ct
                              else writePage (d \\ r) (min (n-3) (length d)) "Correct!" (s+1) t ct
                        else writePage d n "Sorry, try again." (s-1) t ct

-- we display this when the game is over
endGame m s t ct = h1 << m +++ h3 << ("Final score: " ++ show s)
                           +++ h3 << ("Time elapsed: " ++ show (t - ct))
			   +++ h3 << ("Composite: " ++ show score)
                           +++ form << [paragraph << submit "" "Play again?"]
    where score | t - ct > 600 = s
		| otherwise = s * ceiling ((600 / fromIntegral (t - ct))^2)

-- is the response from the user a set?
isSetResponse (r1:r2:r3:[]) = isSet r1 r2 r3
isSetResponse _             = False

-- insert k after every third member of ds
everyThree k (a:b:c:ds) = a:b:c:k:everyThree k ds
everyThree _ ds         = ds

-- given the deck, number of cards, and a message,
-- display the page
writePage d n m s t ct = h3 << m +++ 
                      paragraph << instr +++ 
                      form ! [method "POST", name "sform"] <<
                           ( hidden "setDeck" dc : 
                             hidden "numCards" nc :
                             hidden "verToken" ( vtok d n s ct ) :
                             hidden "userScore" sc :
			     hidden "clockTime" tc :
                             cardsOnTable ++
                             [br +++ submit "" "Submit" +++ 
			      spaceHtml +++ ("Score: " ++ sc) +++ 
			      spaceHtml +++ ("Time elapsed: " ++ etc) ] )
    where dc = show d
          nc = show n
          sc = show s
	  tc = show ct
	  etc = show (t - ct)
          cardsOnTable = everyThree br $ map cardHTML $ zip [0..] (take n d)
          cardHTML (x,c) = (image ! [src oImg,
			             onClick $ clickScript oImg cx])
                            +++ spaceHtml +++ checkbox cx "c"
              where oImg = imgDir ++ show c ++ ".png"
                    cx = 'c':show x
	  instr = "Choose a set, or just hit Submit if you think there aren't any."

-- take a bunch of checkbox inputs and return a list
-- of those checkboxes whose value is equal to "c"
takeChecked _  [] = []
takeChecked [] _  = []
takeChecked (d:dc) (r:rc) = if r == "c"
                             then d : takeChecked dc rc
                             else takeChecked dc rc

-- verification token: BubbleBabble encoded md5 hash of the ascii encoding
-- of the server secret key, the deck, the number of cards dealt, and the score
vtok d n s ct = BuBa.encode $ MD5.hash $ map (fromIntegral.ord) $ show (serverSecretKey,d,n,s,ct)

-- generate a new shuffled deck and display the welcome page
showGreeting n t = writePage dd 12 "Welcome!" 0 t t
    where dd = compressDeck $ evalState mkRandDeck n

-- dispatch
-- get the form inputs
-- dispatch a response if we get a good verification token,
-- otherwise start a new game via showGreeting
cgiMain n t = do nc <- liftM (read.(fromMaybe "0")) $ getInput "numCards"
                 dc <- liftM (read.(fromMaybe "[]")) $ getInput "setDeck"
                 rz <- mapM ((liftM $ fromMaybe "[]").getInput) $ take nc (map (('c':).show) [0..])
                 vt <- liftM (fromMaybe "") $ getInput "verToken"
                 sc <- liftM (read.(fromMaybe "0")) $ getInput "userScore"
		 ct <- liftM (read.(fromMaybe "0")) $ getInput "clockTime"
                 if vt /= vtok dc nc sc ct
                  then output $ renderHtml $ body << (showGreeting n t)
                  else output $ renderHtml $ body << (newPage dc nc sc t ct $ takeChecked dc rz)

-- not much here, keep the IO monad stuff in the main function
-- and dispatch the CGI handler
main = do sg <- liftM mkStdGen randomIO
	  ct <- liftM getClockSeconds getClockTime
          runCGI $ handleErrors (cgiMain sg ct)
