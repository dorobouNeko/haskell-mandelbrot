import SDL2graphics (runProgram)
import Argparser (argParser)
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	parsedArgs <- argParser args
	let	(debug, ([mw,mh], [lB,rB,uB,dB], [green,blue])) = parsedArgs
	runProgram debug mw mh lB rB uB dB green blue
