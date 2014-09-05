{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.RopeUtils
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Collection of helpers to work with 'Rope's easier.
module Yi.RopeUtils where

import           Control.Applicative
import qualified Codec.Binary.UTF8.Generic as G
import           Data.Binary
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as U
import           Data.Monoid
import           Data.Rope
import           Prelude hiding (null, last)

-- | Orphan instance.
--
-- https://github.com/ekmett/rope/issues/7
instance Binary Rope where
  put r = put $ toLazyByteString r
  get = fromLazyByteString <$> get

-- | Split before the specified line. Lines are indexed from 0.
splitAtLine :: Int -> Rope -> (Rope, Rope)
splitAtLine n r | n <= 0     = (mempty, r)
                | otherwise = splitAtLine' (n - 1) r

-- | Split after the specified line. Lines are indexed from 0.
splitAtLine' :: Int -> Rope -> (Rope, Rope)
splitAtLine' n r = let ls = Prelude.take (n + 1) (G.lines' r)
                   in G.splitAt (sum $ map G.length ls) r

-- | Counts the number of lines encoded by a 'Rope'.
countLines :: Rope -> Int
countLines = Prelude.length . G.lines

-- | Reads in and 'LB.ByteString' from the file and converts it to
-- 'Rope'.
fileToRope :: FilePath -> IO Rope
fileToRope p = fromLazyByteString <$> LB.readFile p

-- | Converts a 'Rope' to a 'LB.ByteString' and writes the result to
-- the file.
ropeToFile :: FilePath -> Rope -> IO ()
ropeToFile p = LB.writeFile p . toLazyByteString

-- | Checks if there's a @\n@ at the end of the 'Rope'.
trailingNewline :: Rope -> Bool
trailingNewline r = case null r of
  False -> last r == (10 :: Word8)
  True -> False
