module Argparser where

import System.Exit
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.List (isInfixOf)

strToMaybeDouble = \x -> readMaybe x :: Maybe Double
strToMaybeInt    = \x -> readMaybe x :: Maybe Int

-- [--debug] [--help] [[--s horizontal vertical] [--b left right top bottom] [--c greenHue blueHue]]
argParser :: [String] -> IO (Bool, ([Maybe Int], [Maybe Double], [Maybe Int]))
argParser args =
	case listOfArgList of
		-- no args. using default values
		[]               ->	return $ (False, defValues)
		[["noDashes"]]   -> failedToParse
		[[], ["help"]]   -> help >> exitSuccess
		-- debug on
		[[], ["debug"]]  -> return $ (True, defValues)
		["s", mw, mh]:bs ->	case bs of
			-- only -s flag
			[]                   ->	return $ (False,
				( map strToMaybeInt [mw, mh]
				, defBounds
				, defColor ))
			["b",lB,rB,uB,dB]:cs ->	case cs of
				-- -s and -b flag
				[]                  -> return $ (False,
					( map strToMaybeInt [mw, mh]
					, map strToMaybeDouble [ lB, rB, uB, dB]
					, defColor ))
				["c",green,blue]:xs -> case xs of
					-- all flags
					[] -> return $ (False,
						( map strToMaybeInt [mw, mh]
						, map strToMaybeDouble [lB, rB, uB, dB]
						, map strToMaybeInt [green, blue] ))
					_  -> failedToParse
				_ -> failedToParse
			["c",green,blue]:xs -> case xs of
				-- -s and -c flags
				[] -> return $ (False,
					( map strToMaybeInt [mw, mh]
					, defBounds
					, map strToMaybeInt [green, blue] ))
				_  -> failedToParse
			_ -> failedToParse
		["b",lB,rB,uB,dB]:cs ->	case cs of
			-- only -b flag
			[]                  -> return $ (False,
				( defSize
				, map strToMaybeDouble [lB, rB, uB, dB]
				, defColor ))
			["c",green,blue]:xs -> case xs of
				-- -b and -c flags
				[] -> return $ (False,
					( defSize
					, map strToMaybeDouble [lB, rB, uB, dB]
					, map strToMaybeInt [green, blue] ))
				_  -> failedToParse
		["c",green,blue]:xs -> case xs of
			-- only -c flag
			[] -> return $ (False,
				( defSize
				, defBounds
				, map strToMaybeInt [green, blue] ))
			_  -> failedToParse
		_ -> failedToParse
	where
		defSize   = map strToMaybeInt    ["640", "480"]
		defBounds = map strToMaybeDouble ["-2", "2", "2", "-2"]
		defColor  = map strToMaybeInt    ["255", "255"]
		defValues = (defSize, defBounds, defColor)
		jointArgs = unwords args
		splitArgs =
			if jointArgs == []
				then []
			else
				if "--" `isInfixOf` jointArgs
					then (drop 1) . (splitOn "--") $ jointArgs
					else ["noDashes"]
		listOfArgList = map words splitArgs

help :: IO b
help =
	putStrLn "Usage:" >>
	putStrLn "" >>
	putStrLn "Main --help" >>
	putStrLn "Main --debug" >>
	putStrLn "Main [--s horizontal vertical] [--b left right top bottom] [--c greenHue blueHue]" >>
	putStrLn "" >>
	putStrLn "--help : This help text :)" >>
	putStrLn "--s      : Maximum horizontal and vertical size in pixels of display window" >>
	putStrLn "--b      : Bounding coordinates for drawing the Mandelbrot set" >>
	putStrLn "--c      : Set color hues for green and blue (0 to 255)" >>
	putStrLn "--debug : Spits out debug information." >>
	exitSuccess

failedToParse :: IO a
failedToParse = die "Failed to parse arguments. Use --help for more information."
