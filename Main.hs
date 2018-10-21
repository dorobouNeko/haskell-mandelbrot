import SDL2graphics (runProgram, strToInt, strToDouble)
import Argparser (argParser)
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	parsedArgs <- argParser args

	let
		(maxRes,c) = splitAt 2 parsedArgs
		(bounds, color) = splitAt 4 c
		[mw, mh] = map strToInt maxRes
		[lB, rB, uB, dB] =  map strToDouble bounds
		[green, blue] = map strToInt color
	runProgram mw mh lB rB uB dB green blue
