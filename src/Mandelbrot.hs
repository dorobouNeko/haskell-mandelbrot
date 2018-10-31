module Mandelbrot where

import Data.Complex
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Function (on)
import Data.List (partition)
--import Data.Ratio

c = 0 :+ 0
man :: RealFloat a => Complex a -> Complex a -> Complex a
man c z = z*z + c

pixelSize
	:: (Int, Int)
	-> Double
	-> Double
	-> (Int, Int)
pixelSize maxSize w h =
	if (w/h) < (divInt mw mh)
		then (ceiling $ (fromIntegral mh)*w/h , mh)
		else (mw , ceiling $ (fromIntegral mw)*h/w)
	where
		(mw, mh) = maxSize

-- size = (px,py)
points
	:: (Int, Int)
	-> (Double, Double, Double, Double)
	-> ([((Int, Int), (Complex Double, Complex Double))], Int)
points size bounds =
	([
		(
			(x,(y+offset)) ,
			(\z -> (z,z))
			(
				( w * (divInt x (px-1)) + lB)
				:+
				(-h * (divInt y (py-1)) + uB)
			)
		)
		| y <- ys , x <- [0..(px-1)]
	], py) where
		ys = [0..(py-1)]
		rys = reverse ys
		(l, r, u, d) = bounds
		(lB, rB, uB, dB, offset) =
			if (u > 0.0) && (d < 0.0)
				then
					if u > -d
						then (l, r, u, 0, 0)
						else (l, r, 0, d, ((snd size) - py))
				else (l,r,u,d,0)
		(w, h) = (rB - lB, uB - dB)
		(px,py) = pixelSize size w h

divInt = (/) `on` fromIntegral



reduce = partition (\(x,(c,z)) -> magnitude z <= 2)
apply = map (\(x,(c,z)) -> (x,(c, man c z)))

