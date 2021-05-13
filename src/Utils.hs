{-# LANGUAGE OverloadedStrings #-}
module Utils where

import           Data.Text             as T (Text, pack, replace, unpack)
import           Data.Void             (Void)
import           System.Console.Pretty (Color (Green, Red, Yellow),
                                        Pretty (color, style), Style (Bold))
import           System.IO             (hPutStr, hPutStrLn, stderr, stdout)
import           Text.Megaparsec       as P (Parsec)

type Parser = Parsec Void T.Text

logSuccess :: Bool -> String -> IO ()
logSuccess stdoutMode toLog = do
    let stream = if stdoutMode then stdout else stderr
    hPutStr stream $ style Bold . color Green $ "[✓] "
    hPutStrLn stream toLog

logSuccessLn :: Bool -> String -> IO ()
logSuccessLn stdoutMode toLog = do
    let stream = if stdoutMode then stdout else stderr
    hPutStr stream $ style Bold . color Green $ "\n[✓] "
    hPutStrLn stream toLog

logWarning :: Bool -> String -> IO ()
logWarning stdoutMode toLog = do
    let stream = if stdoutMode then stdout else stderr
    hPutStr stream $ style Bold . color Yellow $ "[!] "
    hPutStrLn stream toLog

logError :: String -> IO ()
logError toLog = do
    hPutStr stderr $ style Bold . color Red $ "[×] "
    hPutStrLn stderr toLog

logMultiLineError :: String -> String -> IO ()
logMultiLineError heading toLog = do
    hPutStr stderr $ style Bold . color Red $ "[×| "
    hPutStr stderr $ heading <> "\n" <> (style Bold . color Red $ "  | ")
    hPutStrLn stderr $ T.unpack $ replace "\n" ("\n" <> (style Bold . color Red $ "  | ")) (T.pack toLog)
