module Vector
       ( Vect,
         xComp,
         yComp,
         zComp,
         vect,
         (^+^),
         (^-^),
         (*^),
         (^*),
         (^/),
         (<.>),
         (><),
         magnitude,
         zeroVect,
         negateVect,
         sumVect
       )
       where

infixl 6 ^+^
infixl 6 ^-^
infixl 7 *^
infixl 7 ^*
infixl 7 ^/
infixl 7 ><
infixl 7 <.>

-- | data type for vectors.
data Vect = Vect { xComp :: Double,
                   yComp :: Double,
                   zComp :: Double
                 }
          deriving Eq

instance Show Vect where
  show (Vect x y z) = "vect" ++ showDouble x ++ " "
                             ++ showDouble y ++ " "
                             ++ showDouble z

showDouble :: Double -> String
showDouble x
  | x < 0     = "(" ++ show x ++ ")"
  | otherwise = show x


-- | smart constructor for vectors.
vect :: Double -> Double -> Double -> Vect
vect = Vect


-- | addition.
(^+^) :: Vect -> Vect -> Vect
Vect ax ay az ^+^ Vect bx by bz = Vect (ax + bx) (ay + by) (az + bz)


-- | subtraction.
(^-^) :: Vect -> Vect -> Vect
Vect ax ay az ^-^ Vect bx by bz = Vect (ax - bx) (ay - by) (az - bz)


-- | scalar multiplication (vector on right).
(*^) :: Double -> Vect -> Vect
c *^ Vect ax ay az = Vect (c * ax) (c * ay) (c * az)


-- | scalar multiplication (vector on left).
(^*) :: Vect -> Double -> Vect
Vect ax ay az ^* c = Vect (c * ax) (c * ay) (c * az)


-- | scalar division (vector on left).
(^/) :: Vect -> Double -> Vect
Vect ax ay az ^/ c = Vect (ax / c) (ay / c) (az / c)


-- | cross product.
(><) :: Vect -> Vect -> Vect
Vect ax ay az >< Vect bx by bz = Vect ((ay * bz) - (az * by)) ((az * bx) - (ax * bz)) ((ax * by) - (ay * bx))


-- | dot product.
(<.>) :: Vect -> Vect -> Double
Vect ax ay az <.> Vect bx by bz = ax * bx + ay * by + az * bz


-- | magnitude of a vector
magnitude :: Vect -> Double
magnitude v = sqrt(v <.> v)


-- | zero vector.
zeroVect :: Vect
zeroVect = Vect 0 0 0


-- | additive inverse of a vector.
negateVect :: Vect -> Vect
negateVect (Vect ax ay az) = Vect (-ax) (-ay) (-az)


-- | sum of a list of vectors.
sumVect :: [Vect] -> Vect
sumVect = foldr (^+^) zeroVect
