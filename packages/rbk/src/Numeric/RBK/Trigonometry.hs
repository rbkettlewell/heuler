module Numeric.RBK.Trigonometry(
  cosAngleDifference
  ) where

cosAngleDifference :: (Floating a) => a -> a -> a
cosAngleDifference a b = cos a * cos b + sin a * sin b
