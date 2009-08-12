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
import HSetGenSVG
import HSetCompress
import Control.Monad (mapM)

main = do
    let theDeck = map (\x -> (x, compressCard x)) setDeck
    mapM (\(x,y) -> writeFile ("setSVG/" ++ show y ++ ".svg") (svgHeader ++ mkCard x 0 0 ++ svgTail)) theDeck
