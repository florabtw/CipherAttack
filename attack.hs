import System.IO
import Control.Monad
import Data.List
import Data.Char
import Database.HDBC
import Database.HDBC.Sqlite3

main = do
    cipher <- prompt "Enter cipher: "
    let allShiftedTexts = getAllShiftedTexts cipher
    likelyText <- getLikelyText allShiftedTexts
    let shift = getShift cipher likelyText
    putStrLn ("\nShift: " ++ show shift)
    putStrLn ("Plaintext: " ++ likelyText)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

getAllShiftedTexts :: String -> [String]
getAllShiftedTexts cipher = [encipher x cipher | x <- [1..26]]

getLikelyText :: [String] -> IO String
getLikelyText allTexts = do
    let realWordsPerText = map numRealWords allTexts
    maxRealWords <- liftM maximum $ sequence realWordsPerText
    maybeIndex <- (liftM $ elemIndex maxRealWords) $ sequence realWordsPerText
    let index = case maybeIndex of
                    Just x -> x
                    Nothing -> 0
    return (allTexts !! index)

numRealWords :: String -> IO Int
numRealWords text = do
    let splitText = words text
        filtered = filterM exists splitText
    liftM length $ filtered

-- Database functions
exists :: String -> IO Bool
exists word = do
    conn <- connectSqlite3 "words.db"
    let statement = "SELECT EXISTS(SELECT 1 FROM words WHERE word=?)"
    results <- quickQuery' conn statement [toSql word]
    let result = head $ head results
        wordExists = (fromSql result :: Int) == 1
    disconnect conn
    return wordExists

getShift :: String -> String -> Int
getShift source target = 1

-- Shift cipher implementation
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
