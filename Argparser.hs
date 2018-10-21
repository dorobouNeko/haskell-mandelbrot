module Argparser where

import System.Exit
import Data.List.Split (splitOn)

-- [-h | --help] [[-s horizontal vertical] [-b left right top bottom] [-c greenHue blueHue]]
argParser :: [String] -> IO [String]
argParser args =
	case listOfArgList of
		-- no args. using default values
		[]               ->	return $ defSize ++ defBounds ++ defColor
		[["noDashes"]]   -> failedToParse
		[["h"]]          -> help >> exitSuccess
		[[],["help"]]    -> help >> exitSuccess
		[["d"]]          -> return ["DEBUG"]
		[[],["debug"]]   -> return ["DEBUG"]
		["s", mw, mh]:bs ->	case bs of
			-- only -s flag
			[]                   ->	return $ [mw, mh] ++ defBounds ++ defColor
			["b",lB,rB,uB,dB]:cs ->	case cs of
				-- -s and -b flag
				[]                  ->
					return $ [mw, mh, lB, rB, uB, dB] ++ defColor
				["c",green,blue]:xs -> case xs of
					-- all flags
					[] -> return $ [mw, mh, lB, rB, uB, dB, green, blue]
					_  -> failedToParse
				_ -> failedToParse
			["c",green,blue]:xs -> case xs of
				-- -s and -c flags
				[] -> return $ [mw, mh] ++ defBounds ++ [green, blue]
				_  -> failedToParse
			_ -> failedToParse
		["b",lB,rB,uB,dB]:cs ->	case cs of
			-- only -b flag
			[]                  -> return $ defSize ++ [lB, rB, uB, dB] ++ defColor
			["c",green,blue]:xs -> case xs of
				-- -b and -c flags
				[] -> return $ defSize ++ [lB, rB, uB, dB, green, blue]
				_  -> failedToParse
		["c",green,blue]:xs -> case xs of
			-- only -c flag
			[] -> return $ defSize ++ defBounds ++ [green, blue]
			_  -> failedToParse
		_ -> failedToParse
	where
		defSize   = ["640", "480"]
		defBounds = ["-2", "2", "2", "-2"]
		defColor  = ["255", "255"]
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
	putStrLn "Main [-s horizontal vertical] [-b left right top bottom] [-c greenHue blueHue]" >>
	putStrLn "" >>
	putStrLn "-h, --h : This help text :)" >>
	putStrLn "-s      : Maximum horizontal and vertical size in pixels of display window" >>
	putStrLn "-b      : Bounding coordinates for drawing the Mandelbrot set" >>
	putStrLn "-c      : Set color hues for green and blue (0 to 255)" >>
	exitSuccess

failedToParse :: IO a
failedToParse = die "Failed to parse arguments. Use -h or --help for more information."
