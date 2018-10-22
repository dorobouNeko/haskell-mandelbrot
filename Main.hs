import SDL2graphics (runProgram)
import Argparser (argParser)
import System.Environment
import System.Exit
import Data.Maybe (fromJust)

main :: IO ()
main = do
	args <- getArgs
	parsedArgs <- argParser args
	(debug, ([mw,mh], [lB,rB,uB,dB], [green,blue])) <- validateValues parsedArgs
	runProgram debug mw mh lB rB uB dB green blue

validateValues args = do
	let
		(debug, maybeArgs) =  args
		-- there has to be a better way to unpack a n-tuple or map a function
		-- maybe change from n-tuple to [Maybe a] if possible (?)
		(maybeA1, maybeA2, maybeA3) = maybeArgs
	if	(Nothing `elem` maybeA1) ||
		(Nothing `elem` maybeA2) ||
		(Nothing `elem` maybeA3)
			then die "Invalid value(s) found in arguments. Check --help for more information"
			else
				return $ (debug,
					( map fromJust maybeA1
					, map fromJust maybeA2
					, map fromJust maybeA3 ))
