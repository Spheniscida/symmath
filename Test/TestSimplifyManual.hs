import System.IO
import Data.Maybe
import Symmath

main = rspl

rspl :: IO ()
rspl = do
    putStr "simplify> "
    hFlush stdout
    eof <- isEOF
    if not eof
        then do
            str <- getLine
            let rawterm = parseStr str
            if Nothing == rawterm
                then rspl
                else do
                    let (Just term) = rawterm
                    print . simplify $ term
                    rspl
        else do
            putStrLn "[EOF]"
            return ()
