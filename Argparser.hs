module Argparser where

import System.Exit
import Data.List.Split (splitOn)
import SDL2graphics (strToDouble, strToInt)

-- [-h | --help] [[-s horizontal vertical] [-b left right top bottom] [-c greenHue blueHue]]
argParser :: [String] -> IO (Bool, ([Int], [Double], [Int]))
argParser args =
	case listOfArgList of
		-- no args. using default values
		[]               ->	return $ (False, defValues)
		[["noDashes"]]   -> failedToParse
		[["h"]]          -> help >> exitSuccess
		[[], ["help"]]   -> help >> exitSuccess
		-- debug on
		[["d"]]          -> return $ (True, defValues)
		[[], ["debug"]]  -> return $ (True, defValues)
		["s", mw, mh]:bs ->	case bs of
			-- only -s flag
			[]                   ->	return $ (False,
				( map strToInt [mw, mh]
				, defBounds
				, defColor ))
			["b",lB,rB,uB,dB]:cs ->	case cs of
				-- -s and -b flag
				[]                  -> return $ (False,
					( map strToInt [mw, mh]
					, map strToDouble [ lB, rB, uB, dB]
					, defColor ))
				["c",green,blue]:xs -> case xs of
					-- all flags
					[] -> return $ (False,
						( map strToInt [mw, mh]
						, map strToDouble [lB, rB, uB, dB]
						, map strToInt [green, blue] ))
					_  -> failedToParse
				_ -> failedToParse
			["c",green,blue]:xs -> case xs of
				-- -s and -c flags
				[] -> return $ (False,
					( map strToInt [mw, mh]
					, defBounds
					, map strToInt [green, blue] ))
				_  -> failedToParse
			_ -> failedToParse
		["b",lB,rB,uB,dB]:cs ->	case cs of
			-- only -b flag
			[]                  -> return $ (False,
				( defSize
				, map strToDouble [lB, rB, uB, dB]
				, defColor ))
			["c",green,blue]:xs -> case xs of
				-- -b and -c flags
				[] -> return $ (False,
					( defSize
					, map strToDouble [lB, rB, uB, dB]
					, map strToInt [green, blue] ))
				_  -> failedToParse
		["c",green,blue]:xs -> case xs of
			-- only -c flag
			[] -> return $ (False,
				( defSize
				, defBounds
				, map strToInt [green, blue] ))
			_  -> failedToParse
		_ -> failedToParse
	where
		defSize   = map strToInt    ["640", "480"]
		defBounds = map strToDouble ["-2", "2", "2", "-2"]
		defColor  = map strToInt    ["255", "255"]
		defValues = (defSize, defBounds, defColor)
		jointArgs = unwords args
		splitArgs =
			if jointArgs == []
				then []
			else
				if elem '-' jointArgs
					then (drop 1) . (splitOn "-") $ jointArgs
					else ["noDashes"]
		listOfArgList = map words splitArgs

help :: IO b
help =
	putStrLn "Usage:" >>
	putStrLn "" >>
	putStrLn "Main -h" >>
	putStrLn "Main --help" >>
	putStrLn "Main -d" >>
	putStrLn "Main --debug" >>
	putStrLn "Main [-s horizontal vertical] [-b left right top bottom] [-c greenHue blueHue]" >>
	putStrLn "" >>
	putStrLn "-h, --h : This help text :)" >>
	putStrLn "-s      : Maximum horizontal and vertical size in pixels of display window" >>
	putStrLn "-b      : Bounding coordinates for drawing the Mandelbrot set" >>
	putStrLn "-c      : Set color hues for green and blue (0 to 255)" >>
	putStrLn "-d, --d : Spits out debug information." >>
	exitSuccess

failedToParse :: IO a
failedToParse = die "Failed to parse arguments. Use -h or --help for more information."
