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

module HSetGenSVG where

import HSetCore

svgHeader = "<svg xmlns=\"http://www.w3.org/2000/svg\"><title>Set</title>\
\<defs><pattern id=\"Patblue\" x=\"0\" y=\"0\" width=\".125\" height=\".0625\">\
\<circle cx=\"3\" cy=\"3\" r=\"2\" fill=\"blue\" /></pattern>\
\<pattern id=\"Patgreen\" x=\"0\" y=\"0\" width=\".125\" height=\".0625\">\
\<circle cx=\"3\" cy=\"3\" r=\"2\" fill=\"green\" /></pattern>\
\<pattern id=\"Patred\" x=\"0\" y=\"0\" width=\".125\" height=\".0625\">\
\<circle cx=\"3\" cy=\"3\" r=\"2\" fill=\"red\" /></pattern></defs>"

svgTail = "</svg>"

cardBox xoff yoff =
  "<path stroke=\"black\" stroke-width=\"2\" fill=\"none\" \
 \d=\"m "++show (5+xoff)++","++show (5+yoff)++" l 0,170 l 190,0 l 0,-170 z\" />"

toColString col = case col of
                   Red -> "red"
                   Green -> "green"
                   Blue -> "blue"

toFillString col fil = case fil of
                        Solid -> col
                        Outline -> "none"
                        Hashed -> "url(#Pat"++col++")"

toOffsets num = case num of
                 One -> [100]
                 Two -> [75,125]
                 Three -> [50,100,150]

mkSquiggle col fil num xoff yoff = concat $ cardBox xoff yoff : map (\x ->
  "<path stroke=\""++colString++"\" stroke-width=\"4\" fill=\""++fillString++"\" \
 \d=\"m "++show (10+x+xoff)++","++show (20+yoff)++" q -50,-5 -20,20 q 20,20 0,40 \
 \q -50,40 0,80 q 50,5 20,-20 q -20,-20 0,-40 q 50,-40 0,-80\" />") foffsets
    where colString = toColString col
          fillString = toFillString colString fil
          foffsets = toOffsets num

mkOval col fil num xoff yoff = concat $ cardBox xoff yoff : map (\x ->
  "<ellipse cx=\""++show (x+xoff)++"\" cy=\""++show (90+yoff)++"\" rx=\"20\" ry=\"75\" \
 \fill=\""++fillString++"\" stroke-width=\"4\" stroke=\""++colString++"\" />") foffsets
    where colString = toColString col
          fillString = toFillString colString fil
          foffsets = toOffsets num

mkDiamond col fil num xoff yoff = concat $ cardBox xoff yoff : map (\x ->
  "<path stroke=\""++colString++"\" stroke-width=\"4\" fill=\""++fillString++"\" \
 \d=\"m "++show (x+xoff)++","++show (20+yoff)++" l -20,70 l 20,70 l 20,-70 z\" />") foffsets
   where colString = toColString col
         fillString = toFillString colString fil
         foffsets = toOffsets num

mkCard card xoff yoff = showFn col fil num xoff yoff
    where showFn = case c_shape card of
                     Squiggle -> mkSquiggle
                     Oval -> mkOval
                     Diamond -> mkDiamond
          col = c_color card
          fil = c_fill card
          num = c_numb card

showCards cs = concat $ map (\(c,(x,y)) -> mkCard c x y) $
               zip cs [ (200*x,180*y) | x <- [0..], y <- [0..2] ]
