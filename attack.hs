import System.IO
import Data.Char

main = do
    cipher <- prompt "Enter cipher: "
    let (shift, plaintext) = attack cipher
    putStrLn ("\nShift: " ++ show shift)
    putStrLn ("Plaintext: " ++ plaintext)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

attack :: String -> (Int, String)
attack cipher = (0, cipher)

encipher :: Int -> String -> String
encipher shift [] = []
encipher shift (c:cs) = shiftChar shift c : encipher shift cs

shiftChar :: Int -> Char -> Char
shiftChar shift c
    | isSpace c = c
    | otherwise = let shifted = ord c + shift
                      -- Assume lowercase
                      newOrd = (shifted - ord 'a') `mod` 26 + ord 'a'
                  in  chr newOrd
