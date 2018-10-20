import System.Environment
import System.Exit
import Data.List.Split (splitOn)


main :: IO ()
main = do
	args <- getArgs
	let
		jointArgs = unwords args
		splitArgs =
			if jointArgs == []
				then []
			else
				if elem '-' jointArgs
					then (drop 1) . (splitOn "-") $ jointArgs
					else ["noDashes"]
		listOfArgList = map words splitArgs


	case listOfArgList of
		[]               -> print "no args. using default values"
		[["noDashes"]]   -> failedToParse
		[["h"]]          -> help
		["s", mw, mh]:bs ->	case bs of
			[]                   -> print "only -s flag"
			["b",lB,rB,uB,dB]:cs ->	case cs of
				[]                  -> print "-s and -b flag"
				["c",green,blue]:xs -> case xs of
					[] -> print "all flags"
					_  -> failedToParse
				_                   -> failedToParse
			["c",green,blue]:xs -> case xs of
				[] -> print "s and c flags"
				_  -> failedToParse
			_                    -> failedToParse
		["b",lB,rB,uB,dB]:cs ->	case cs of
			[]                  -> print "only -b flag"
			["c",green,blue]:xs -> case xs of
				[] -> print "b and c flags"
				_  -> failedToParse
		["c",green,blue]:xs -> case xs of
			[] -> print "only -c flag"
			_  -> failedToParse
		_                -> failedToParse



help = putStrLn "This is a help message. Very helpful indeed."
failedToParse = die "Failed to parse arguments. Use -h for more information."
