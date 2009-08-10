
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

module HSetCore where

-- core of set game
-- implemented here: 
--   datatypes for Set cards
--   generate set deck
--   randomize set deck
--   given two cards, generate matching third
--   check three cards for set

import qualified Data.List as DL
import HRandomState
import System.Random (mkStdGen)

-- an algebraic type per attribute
data SetColor = Red | Green | Blue
        deriving (Ord, Eq, Show)
data SetShape = Squiggle | Oval | Diamond
        deriving (Ord, Eq, Show)
data SetFill = Solid | Outline | Hashed
        deriving (Ord, Eq, Show)
data SetNumb = One | Two | Three
        deriving (Ord, Eq, Show)

-- put all this together into a datatype for the card
data SetCard = SCard {
        c_color :: SetColor
      , c_shape :: SetShape
      , c_fill :: SetFill
      , c_numb :: SetNumb
} deriving (Eq, Ord, Show)

-- the set deck --- one of each card
setDeck = [ SCard a b c d | a <- [Red, Green, Blue], 
                            b <- [Squiggle, Oval, Diamond],
                            c <- [Solid, Outline, Hashed],
                            d <- [One, Two, Three] ]

-- shuffle the cards in the deck
-- this uses a StdGen wrapped in the State monad
mkRandDeck = takeRand (81 :: Int) setDeck []

-- checking and making sets of three
thirdFoo tz a b | a == b    = a
                | otherwise = head (DL.delete a $ DL.delete b tz)

-- third of each attribute
thirdColor = thirdFoo [Red,Green,Blue]
thirdShape = thirdFoo [Squiggle,Oval,Diamond]
thirdFill = thirdFoo [Solid,Outline,Hashed]
thirdNumb = thirdFoo [One,Two,Three]

-- compute the third card in a set
thirdCard a b = SCard (thirdColor (c_color a) (c_color b))
                      (thirdShape (c_shape a) (c_shape b))
                      (thirdFill (c_fill a) (c_fill b))
                      (thirdNumb (c_numb a) (c_numb b))

-- check whether three cards make a set
isSet a b c = c == (thirdCard a b)

-- replace members of rs with ss in the list ts
replaceCards [] [] ts = ts
replaceCards [] ss ts = ss++ts
replaceCards rs [] ts = deleteAll rs ts
    where deleteAll []    ts  = ts
          deleteAll rs (t:ts) = if t `elem` rs 
                                 then deleteAll (DL.delete t rs) ts
                                 else t : deleteAll rs ts
replaceCards rs ss (t:ts) = if t `elem` rs 
                             then head ss : replaceCards (DL.delete t rs) (tail ss) ts
                             else t : replaceCards rs ss ts

identifySets lz = filter (\(x,y,z) -> isSet (lz !! x) (lz !! y) (lz !! z))
                    [ (x,y,z) | x <- [0..llz], y <- [x+1..llz], z <- [y+1..llz] ]
    where llz = length lz - 1
