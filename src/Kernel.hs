{-|
Module      : Kernel
Description : All basics: vectors, ...
Copyright   : (c) Tristan Roussillon, 2018
License     : None
Maintainer  : tristan.roussillon@liris.cnrs.fr
Stability   : experimental
-}
module Kernel (
  -- * Type classes
  Vector, Vector2, Vector3,
  -- * Concrete types
  Vec2, Vec3,
  -- *Operations
  -- ** arithmetic
  add, scale, neg, minus,
  -- ** dot, cross and by-products
  dot, square, cross, isInHalfSpace,
  -- ** norms
  l1, linf, l2,
  -- * others
  mkV2, mkV3, mkZero2, mkZero3, getX, getY, getZ, dim
              ) where

-- | This class gathers functions shared by all types
-- that represent vectors of arbitrary dimension.  
class Vector v where

  {- to be implemented in instances -}
  -- | function that takes two vectors and
  -- returns their sum
  add :: (Num a) => v a -> v a -> v a
  -- | function that takes a scalar k and
  -- a vector v and returns k * v 
  scale :: (Num a) => a -> v a -> v a
  -- | function that takes a vector and
  -- returns its dimension
  dim :: v a -> Int
  -- | function that takes a vector and
  -- returns its norm L1, ie. the sum of
  -- the absolute value of its components
  l1 :: (Num a) => v a -> a
  -- | function that takes a vector and
  -- returns its infinite norm, ie. the maximum
  -- of the absolute value of its components
  linf :: (Real a) => v a -> a
  -- | function that takes a vector and
  -- returns its euclidean norm
  l2 :: (Real a) => v a -> Double
  -- | function that takes two vectors and
  -- returns their dot product
  dot :: (Num a) => v a -> v a -> a
  -- | returns the x(first)-component 
  getX :: (Num a) => v a -> a
  -- | retruns the y(second)-component 
  getY :: (Num a) => v a -> a

  {- default implementations -}
  neg :: (Num a) => v a -> v a
  neg x = (-1) `scale` x 
  minus :: (Num a) => v a -> v a -> v a
  x `minus` x' = x `add` (neg x') 
  square :: (Num a) => v a -> a
  square x = x `dot` x

-- | This class gathers functions shared by all types
-- that represent 3d vectors   
class (Vector v) => Vector3 v where
  -- | 'cross' is a function that basically takes
  -- two 3d vectors and returns their cross product 
  cross :: (Num a) => v a -> v a -> v a
  -- | 'getZ' returns the z-component of a 3d vector
  getZ :: (Num a) => v a -> a
  -- | makes a 3d vector from a tuple of three components
  mkV3 :: (Num a) => (a,a,a) -> v a
  {- default implementations -}
  -- | makes the zero vector
  mkZero3 :: (Num a) => v a
  mkZero3 = mkV3 (0,0,0)
  -- | Function that checks whether the third vector is
  -- in the half-space bounded by the plane spanned by
  -- the two first vectors (orientation given by the
  -- right-hand rule). 
  isInHalfSpace :: (Num a, Ord a) =>
    (v a, v a) -> v a -> Ordering
  isInHalfSpace (x, y) z = case (x `cross` y) `dot` z of
    res | res > 0 -> LT
        | res < 0 -> GT
        | otherwise -> EQ

-- | 'Vec3' is a generic data type
-- that represents 3d vectors.
-- It is simply based on a tuple
-- of three components
newtype Vec3 a = Vec3 (a, a, a)
  deriving (Show, Read, Eq, Ord)

-- | 'Vec3' is an instance of 'Vector'. 
instance Vector Vec3 where
  (Vec3 (x, y, z)) `add` (Vec3 (x', y', z')) = Vec3 (x+x', y+y', z+z') 
  k `scale` (Vec3 (x, y, z)) = Vec3 (k*x, k*y, k*z)
  dim v = 3
  (Vec3 (x, y, z)) `dot` (Vec3 (x', y', z')) = (x*x') + (y*y') + (z*z')
  l1 (Vec3 (x, y, z)) = (abs x) + (abs y) + (abs z)
  linf (Vec3 (x, y, z)) = maximum [ abs x, abs y, abs z ]
  l2 (Vec3 (x, y, z)) = sqrt $ (realToFrac x)^2
                    + (realToFrac y)^2
                    + (realToFrac z)^2
  getX (Vec3 (x, _, _)) = x 
  getY (Vec3 (_, y, _)) = y 

-- | 'Vec3' is an instance of 'Vector3'. 
instance Vector3 Vec3 where
  (Vec3 (x, y, z)) `cross` (Vec3 (x', y', z'))
    = Vec3 (y*z' - z*y', z*x' - x*z', x*y' - y*x')  
  getZ (Vec3 (_, _, z)) = z 
  mkV3 (x, y, z) = Vec3 (x, y, z)

-- | This class gathers functions shared by all types
-- that represent 2d vectors   
class (Vector v) => Vector2 v where
  -- | makes a 2d vector from a pair of components
  mkV2 :: (Num a) => (a,a) -> v a
  {- default implementations -}
  -- | makes the zero vector
  mkZero2 :: (Num a) => v a
  mkZero2 = mkV2 (0,0)
  -- | Function that checks whether the second vector is
  -- in the half-plane bounded by the line incident to 
  -- the first vector (oriented on the left when looking
  -- at the direction pointed to by the first vector).
  isInHalfPlane :: (Num a, Ord a) =>
    v a -> v a -> Ordering
  isInHalfPlane x y = let rotate90ccw z = mkV2 (- (getY z), getX z)
                      in case (rotate90ccw x) `dot` y of
                           res | res > 0 -> LT
                               | res < 0 -> GT
                               | otherwise -> EQ

-- | 'Vec2' is a generic data type
-- that represents 2d vectors.
-- It is simply based on a pair of components
newtype Vec2 a = Vec2 (a, a)
  deriving (Show, Read, Eq, Ord)

-- | 'Vec2' is an instance of 'Vector'. 
instance Vector Vec2 where
  (Vec2 (x, y)) `add` (Vec2 (x', y')) = Vec2 (x+x', y+y') 
  k `scale` (Vec2 (x, y)) = Vec2 (k*x, k*y)
  dim v = 2
  (Vec2 (x, y)) `dot` (Vec2 (x', y')) = (x*x') + (y*y')
  l1 (Vec2 (x, y)) = (abs x) + (abs y)
  linf (Vec2 (x, y)) = max (abs x) (abs y)
  l2 (Vec2 (x, y)) = sqrt $ (realToFrac x)^2 + (realToFrac y)^2
  getX (Vec2 (x, _)) = x 
  getY (Vec2 (_, y)) = y 

-- | 'Vec2' is an instance of 'Vector2'. 
instance Vector2 Vec2 where
  mkV2 (x, y) = Vec2 (x, y)


