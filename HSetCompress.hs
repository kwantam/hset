
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

module HSetCompress where

import HSetCore
import qualified Data.ByteString.Lazy as DL
import Data.Word (Word8(..))
import qualified Codec.Compression.Zlib as CZ

compressDeck :: [SetCard] -> [Word8]
compressDeck = map compressCard

zipCompressDeck = CZ.compress.DL.pack.compressDeck

decompressDeck :: [Word8] -> [SetCard]
decompressDeck = map decompressCard

zipDecompressDeck = decompressDeck.DL.unpack.CZ.decompress

colorToNum col = case col of { Red -> 0 ; Green -> 1; Blue -> 2; }
numToColor = ((!!) [Red,Green,Blue]).fromIntegral

shapeToNum shp = case shp of { Squiggle -> 0; Oval -> 1; Diamond -> 2; }
numToShape = ((!!) [Squiggle,Oval,Diamond]).fromIntegral

fillToNum fil = case fil of { Solid -> 0; Outline -> 1; Hashed -> 2; }
numToFill = ((!!) [Solid,Outline,Hashed]).fromIntegral

numbToNum numb = case numb of { One -> 0; Two -> 1; Three -> 2; }
numToNumb = ((!!) [One,Two,Three]).fromIntegral

divmod :: (Integral a) => a -> a -> (a,a)
divmod x y = (y`mod`x,y`div`x)

divmod3 = divmod 3

compressCard :: SetCard -> Word8
compressCard c = fromIntegral $ n_color + n_shape + n_fill + n_numb
  where n_color = colorToNum $ c_color c
        n_shape = 3*(shapeToNum $ c_shape c)
        n_fill = 9*(fillToNum $ c_fill c)
        n_numb = 27*(numbToNum $ c_numb c)
        
decompressCard :: Word8 -> SetCard
decompressCard wc0 = SCard (numToColor ncol) 
                           (numToShape nshp)
                           (numToFill nfil)
                           (numToNumb nnumb)
  where (ncol,wc1) = divmod3 wc0
        (nshp,wc2) = divmod3 wc1
        (nfil,wc3) = divmod3 wc2
        (nnumb,_) = divmod3 wc3
