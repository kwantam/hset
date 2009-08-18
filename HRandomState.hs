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

module HRandomState where

{- a very small bit of monadic shorthand
 - to keep the random number generation tidy
 -}

import Control.Monad (liftM2)
import Control.Monad.State (State(..), get, put)
import System.Random (StdGen(..), randomR, Random(..))

-- shorthand
type RandomState a = State StdGen a

-- an infinite list of randomly generated numbers in the state monad
lstRandRng :: (Random a) => (a,a) -> RandomState [a]
lstRandRng rng = liftM2 (:) (getRandRng rng) (lstRandRng rng)

-- state monad wrapper around randomR
getRandRng :: (Random a) => (a,a) -> RandomState a
getRandRng rng =
    get >>= \gen ->
    let (rndRes, newGen) = randomR rng gen
    in put newGen >> return rndRes

-- remove the ith entry in a list, or the
-- last one if i > length
removeIth bs (e:es) 0 = (e,bs++es)
removeIth bs (e:[]) _ = (e,bs)
removeIth bs (e:es) n = removeIth (e:bs) es (n-1)

-- take random entry from list of length n
takeRand _ [] r = return r
takeRand 0 l  r = return r
takeRand n l  r = getRandRng (0,n) >>= \x ->
                   let (e,ln) = removeIth [] l x
                   in takeRand (n-1) ln (e:r)

