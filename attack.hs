import System.IO

main = do
    cipher <- prompt "Enter cipher: "
    let (shift, plaintext) = decipher cipher
    putStrLn ("\nShift: " ++ show shift)
    putStrLn ("Plaintext: " ++ plaintext)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

decipher :: String -> (Int, String)
decipher cipher = (0, cipher)
