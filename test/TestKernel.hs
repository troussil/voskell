{-# LANGUAGE TemplateHaskell #-}
module TestKernel where

import qualified Test.QuickCheck as Q
import Kernel

-- | 'Vec3' is an instance of 'Arbitrary' so that
-- we can randomly generate 3d vectors for testing
instance (Q.Arbitrary a, Num a) => Q.Arbitrary (Vec3 a) where
  arbitrary = fmap mkV3 Q.arbitrary :: (Q.Arbitrary a, Num a) => Q.Gen (Vec3 a)

{-----------------------------------------}
{---------- properties to test -----------}
{-----------------------------------------}

type IVec3 = Vec3 Int

prop_scale :: IVec3 -> Bool
prop_scale x = scale 0 x == mkZero3

prop_minuscommut :: IVec3 -> IVec3 -> Bool
prop_minuscommut x y = minus x y == neg (minus y x)

prop_dim :: IVec3 -> Bool
prop_dim x = dim x == 3

prop_l1linf :: IVec3 -> Bool
prop_l1linf x = linf x <= l1 x

prop_l2 :: IVec3 -> Q.Property
prop_l2 x = (x /= mkZero3) Q.==>
  (l2 x) > 0

prop_dotcommut :: IVec3 -> IVec3 -> Bool
prop_dotcommut x y = dot x y == dot y x
prop_dotbilinear :: IVec3 -> IVec3 -> IVec3 -> Int -> Bool
prop_dotbilinear x y z r = dot x ((r `scale` y) `add` z) == r * (dot x y) + (dot x z)

prop_crossself :: IVec3 -> Bool
prop_crossself x = x `cross` x == mkZero3

prop_crosscommut :: IVec3 -> IVec3 -> Bool
prop_crosscommut x y = x `cross` y == neg (y `cross` x)

prop_crossdistrib :: IVec3 -> IVec3 -> IVec3 -> Bool
prop_crossdistrib x y z = x `cross` (y `add` z) == (x `cross` y) `add` (x `cross` z) 

prop_crossmultip :: IVec3 -> IVec3 -> Int -> Bool
prop_crossmultip x y r = (r `scale` x) `cross` y == r `scale` (x `cross` y) 

prop_isInEq :: IVec3 -> IVec3 -> Int -> Int -> Bool
prop_isInEq x y a b = let z = (a `scale` x) `add` (b `scale` y)
                      in isInHalfSpace (x,y) z == EQ

prop_isInNonEq :: IVec3 -> IVec3 -> Int -> Q.Property
prop_isInNonEq x y r = (r /= 0) && (x /= mkZero3) && (y /= mkZero3)
  && ( (x `dot` y) ^ 2 /= (square x) * (square y) ) -- i.e. linearly independant 
  Q.==> test
  where test
          | r > 0     = res == LT
          | otherwise = res == GT
        res = isInHalfSpace (x,y) z
        z = r `scale` (x `cross` y)

prop_isInCirc :: IVec3 -> IVec3 -> IVec3 -> Bool
prop_isInCirc x y z = (isInHalfSpace (x,y) z == isInHalfSpace (y,z) x)
  && (isInHalfSpace (y,z) x == isInHalfSpace (z,x) y)

prop_access :: IVec3 -> Bool
prop_access u = u == mkV3 (getX u, getY u, getZ u)

{-----------------------------------------}
{------------ main function --------------}
{-----------------------------------------}
-- weird trick to test all properties
return []
runTestKernel :: IO Bool
runTestKernel = $(Q.quickCheckAll)
