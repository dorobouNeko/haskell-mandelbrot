{-# LANGUAGE OverloadedStrings #-}

import qualified SDL
import Control.Monad (unless)
import qualified Mandelbrot as M
import System.Environment
import Data.Function (on)
import Foreign.C.Types (CFloat,CInt)


main :: IO ()
main = do
	args <- getArgs -- maxW maxH lB rB uB dB green=255 blue=255
	case args of
		[mw, mh, lB, rB, uB, dB] -> do
			let
				(maxRes, c) = splitAt 2 args
				(b,color) = splitAt 4 c
				([mw, mh],[lB, rB, uB, dB]) =
				 	(map strToInt maxRes
				 	, map strToDouble b)
			runProgram mw mh lB rB uB dB 255 255
		_ -> do print "failure"

runProgram mw mh lB rB uB dB green blue = do
	let
		bounds = (lB,rB,uB,dB)
 		(w,h) = (rB - lB, uB - dB)
		(px,py) = M.pixelSize (mw,mh) w h
		windowSize = v2CInt (px, py)
		(s0,py') = M.points (px,py) bounds
		offset =
			if uB > -dB
			then   py-py'
				else -(py-py')
		hue =
			cycle $
				filter (\x -> mod x 23 == 0) ([0..255]++(reverse [1..254]))
	SDL.initializeAll
{- debug option
			print (px,py,"dimensions")
			print (py',"point height")
			print (offset,"offset")
			print (length s0)
			print (head s0)
			print (last s0)
-}
	window <- SDL.createWindow "SDL Application" SDL.defaultWindow
			{ SDL.windowInitialSize  = windowSize }{-
			, SDL.windowOpenGL = Just SDL.defaultOpenGL }
			-}
{-
		glContext <- SDL.glCreateContext window
		SDL.swapInterval SDL.$= SDL.ImmediateUpdates
-}
	renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
{-
		SDL.rendererScale renderer SDL.$= (v2CFloat (s, s))
-}
	appLoop renderer s0 [] 0 hue offset py green blue


v2CFloat :: (Int,Int) -> (SDL.V2 CFloat)
v2CFloat      (x, y)   = (SDL.V2 `on` fromIntegral) x y

v2CInt :: (Int,Int) -> (SDL.V2 CInt)
v2CInt      (x, y)   = (SDL.V2 `on` fromIntegral) x y

strToDouble = \x -> read x :: Double
strToInt    = \x -> read x :: Int
strToFloat  = \x -> read x :: Float

drawP rend = (SDL.drawPoint rend) . SDL.P . v2CInt

drawMirrored n r (x,y) =
	drawP r (x,y) >>
	drawP r (x, -y+2*n)

divInt = (/) `on` fromIntegral

drawColored r n i [] = return ()
drawColored r n i (x:xs) =
	SDL.rendererDrawColor r SDL.$= SDL.V4 i 85 170 255 >>
	mapM_ (drawMirrored n r) x >>
	drawColored r n (mod (i+5) 255) xs

drawColor _ [] _ _ = return ()
drawColor r (x:xs) j l =
	SDL.rendererDrawColor r SDL.$= SDL.V4 hue 85 170 255 >>
	mapM_ (drawP r) (snd x) >>
	drawColor r xs (j+1) l
	where
		hue = ceiling $ 255*(divInt j l)


drawColor' _ [] _ = return ()
drawColor' r (x:xs) total =
	SDL.rendererDrawColor r SDL.$= SDL.V4 hue 85 170 255 >>
	mapM_ (drawP r) esc >>
	drawColor' r xs total
	where
		(i,esc) = x
		hue = ceiling $ 255*(divInt i total)

drawColor'' _ [] (h:hs) _ _ = return ()
drawColor'' r (x:xs) h g b =
	SDL.rendererDrawColor r SDL.$= SDL.V4 hue (fromIntegral g) (fromIntegral b) 255 >>
	mapM_ (drawP r) esc >>
	drawColor'' r xs hs g b
	where
		(i,esc) = x
		(hue:hs) = h

drawColor''' _ [] (h:hs) _ _ _ _ = return ()
drawColor''' r (x:xs) h offset py g b =
	SDL.rendererDrawColor r SDL.$= SDL.V4 hue (fromIntegral g) (fromIntegral b) 255 >>
	mapM_ (drawMirrored' r offset py) esc >>
	drawColor''' r xs hs offset py g b
	where
		(i,esc) = x
		(hue:hs) = h


drawMirrored' r offset py (x,y) =
	drawP r (x,y) >>
	--drawP r (x, 150) >> drawP r (x,300) >> drawP r (x,450) >> drawP r (x,599)
	if offset < 0
		then
			if y <= -offset*2
				then drawP r (x, -y-2*offset)
				else return ()
		else
			if y >= offset-1
				then drawP r (x, -y+2*(py-offset-1))
				else return ()


--appLoop :: SDL.Renderer -> [Cells] -> IO ()
appLoop renderer s list i hues offset py g b = do
	events <- SDL.pollEvents

	let
		(newState, esc) = M.reduce s
		noEsc = null esc
		newEscList = if noEsc then list else list ++ [(i,(map fst esc))]
--	print (i, newList)
	if noEsc
		then print ("No Escape",i) >> return ()
		else do
			SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
			SDL.clear renderer
--			drawColored renderer scale 0 newList
--			print (length newList)
--			drawColor renderer newEscList 1 (length newEscList)
--			drawColor' renderer newEscList i
			if offset == 0
				then drawColor''  renderer newEscList hues g b
				else drawColor''' renderer newEscList hues offset py g b
--			SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 255
--			mapM_ ((drawMirrored scale renderer) . fst) sn

			SDL.present renderer

	unless
		(qPressed events)
		(appLoop renderer (M.apply newState) newEscList (i+1) hues offset py g b)

eventIsQPress event =
	case SDL.eventPayload event of
		SDL.KeyboardEvent keyboardEvent ->
			SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
			SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
		_ -> False

qPressed events = not (null (filter eventIsQPress events))
