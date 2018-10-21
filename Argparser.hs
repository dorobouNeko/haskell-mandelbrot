module Argparser where

import System.Exit
import Data.List.Split (splitOn)

--  [-s horizontal vertical] [-b left right top bottom] [-c greenHue blueHue]
argParser :: [String] -> IO [String]
argParser args =
	case listOfArgList of
		-- no args. using default values
		[]               ->	return ["640", "480", "-2", "2", "2", "-2", "255", "255"]
		[["noDashes"]]   -> failedToParse
		[["h"]]          -> help >> exitSuccess
		[[],["help"]]    -> help >> exitSuccess
		["s", mw, mh]:bs ->	case bs of
			-- only -s flag
			[]                   ->	return [mw, mh, "-2", "2", "2", "-2", "255", "255"]
			["b",lB,rB,uB,dB]:cs ->	case cs of
				-- -s and -b flag
				[]                  -> return [mw, mh, lB, rB, uB, dB, "255", "255"]
				["c",green,blue]:xs -> case xs of
					-- all flags
					[] -> return [mw, mh, lB, rB, uB, dB, green, blue]
					_  -> failedToParse
				_ -> failedToParse
			["c",green,blue]:xs -> case xs of
				-- -s and -c flags
				[] -> return [mw, mh, "-2", "2", "2", "-2", green, blue]
				_  -> failedToParse
			_ -> failedToParse
		["b",lB,rB,uB,dB]:cs ->	case cs of
			-- only -b flag
			[]                  -> return ["640", "480", lB, rB, uB, dB, "255", "255"]
			["c",green,blue]:xs -> case xs of
				-- -b and -c flags
				[] -> return ["640", "480", lB, rB, uB, dB, green, blue]
				_  -> failedToParse
		["c",green,blue]:xs -> case xs of
			-- only -c flag
			[] -> return ["640", "480", "-2", "2", "2", "-2", green, blue]
			_  -> failedToParse
		_ -> failedToParse
	where
		jointArgs = unwords args
		splitArgs =
			if jointArgs == []
				then []
			else
				if elem '-' jointArgs
					then (drop 1) . (splitOn "-") $ jointArgs
					else ["noDashes"]
		listOfArgList = map words splitArgs


help =
	putStrLn "Main [-s horizontal vertical] [-b left right top bottom] [-c greenHue blueHue]" >>
	putStrLn "-s : maximum horizontal and vertical size in pixels" >>
	putStrLn "-b : bounding coordinates for drawing the Mandelbrot set" >>
	putStrLn "-c : set color hues for green and blue (0 to 255)" >>
	exitSuccess

failedToParse = die "Failed to parse arguments. Use -h or --help for more information."
