{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception       (IOException, SomeException, catch)
import           Control.Monad           (filterM, foldM, when)
import           Data.Either.Combinators (fromLeft', fromRight, fromRight',
                                          isRight)
import           Data.Ini                as I (keys, lookupValue, readIniFile)
import           Data.List               (isSuffixOf)
import qualified Data.Map                as M
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Version            (showVersion)
import           Options.Applicative     as O (Parser, execParser, flag,
                                               fullDesc, header, help, helper,
                                               info, infoOption, long, metavar,
                                               progDesc, short, strOption,
                                               switch, value, (<**>))
import           Paths_MonType           (version)
import qualified System.Console.Pretty   as SP (Color (Red, Yellow),
                                                Pretty (color, style),
                                                Style (Bold))
import           System.Directory        (doesDirectoryExist, doesFileExist,
                                          getDirectoryContents, listDirectory,
                                          withCurrentDirectory)
import           Text.Megaparsec         as P (ShowErrorComponent (showErrorComponent),
                                               errorBundlePretty, parse)

import           Codegen.Codegen         (makeInterface)
import           Codegen.Helpers         (myFoldM)
import           Parser.TopLevel         (schema)
import           Utils                   (logError, logMultiLineError,
                                          logSuccess, logSuccessLn, logWarning)


data CliArgs = CliArgs
  { target     :: String
  , stdoutMode :: Bool
  , strictMode :: Bool
  , configPath :: String
  , outputPath :: String }
  deriving (Show)

cliArgs :: O.Parser CliArgs
cliArgs = CliArgs
      <$> strOption
          ( long "from"
         <> short 'f'
         <> metavar "TARGET"
         <> help "File or folder containing files which define the schema(s)" )
      <*> flag False True
          ( long "stdout"
         <> help "Print to stdout instead of saving to file" )
      <*> flag False True
          ( long "strict"
         <> short 's'
         <> help "Halt execution when an error is found at an intermediate point in directory mode" )
      <*> strOption
          ( long "config"
         <> short 'c'
         <> metavar "PATH"
         <> value "montype.ini"
         <> help "Path to configuration file, defaults to 'montype.ini'" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "PATH"
         <> value "MonType.ts"
         <> help ("File to output the generated interface to, ignored if the stdout flag is set. Defaults to 'MonType.ts'"
                    <> (SP.style SP.Bold . SP.color SP.Red) " Will overwrite if the file already exists." ) )


-- map but with the function coming later
myMap :: [a] -> (a -> b) -> [b]
myMap list func = map func list

-- read the config and return a map
-- of custom types defined by the user
readConfig :: String -> IO (M.Map Text Text)
readConfig path = do
  parsedIni <- readIniFile path
  if isRight parsedIni
    then do
      let typeKeys = fromRight [] $ I.keys "types" $ fromRight' parsedIni
      pure $
        M.fromList $ myMap typeKeys $ \typeKey -> do (typeKey, fromRight' (lookupValue "types" typeKey (fromRight' parsedIni)))
    else pure M.empty

-- output log to stderr when the program is running in
-- stdout mode, since it can pollute the target if the
-- result is piped to something else
excCatcher :: Bool -> IOException -> IO (M.Map Text Text)
excCatcher stdoutMode e = do
  logWarning stdoutMode "using default type maps\n"
  pure M.empty

removeTrailing :: String -> Char -> String
removeTrailing str char = if last str == char then removeTrailing (init str) char else str

montype :: CliArgs -> IO ()
montype (CliArgs target stdoutMode strictMode config out) = do
  config <- catch (readConfig config) $ excCatcher stdoutMode

  isDir <- doesDirectoryExist target
  isFile <- doesFileExist target

  if isDir then do
    -- get files from dir
    dirContents <- listDirectory target

    -- only keep _files_ that end with .ts
    contents <- filterM (\x -> if ".ts" `isSuffixOf` x then withCurrentDirectory target (doesFileExist x) else pure False) dirContents

    -- fold through the files list and generate a collective interface
    interface <- myFoldM contents "import mongoose from 'mongoose';\n" $ \acc curr -> do
      content <- (withCurrentDirectory target . readFile) curr

      let currPath = removeTrailing target '/' <> "/" <> curr
      let result = P.parse schema curr (T.pack content)

      if isRight result then do
        -- AST for current file generated
        let currInterfaceStr = uncurry makeInterface (fromRight' result) config

        if isRight currInterfaceStr then do
          -- interface for current file generated
          logSuccess stdoutMode currPath
          pure $ acc <> "\n" <> fromRight' currInterfaceStr
        else do
          -- couldn't codegen current file, show error
          logMultiLineError currPath $ fromLeft' currInterfaceStr
          when strictMode $ error "\nstrict flag enabled, exiting"
          pure acc
      else do
        -- couldn't parse current file, show error
        logMultiLineError currPath $ errorBundlePretty (fromLeft' result)
        when strictMode $ error "\nstrict flag enabled, exiting"
        pure acc

    -- write folded interface to file/stdout
    if stdoutMode then putStrLn $ "\n" <> T.unpack interface else do
      logSuccessLn stdoutMode $ "saved to '" <> out <> "'"
      writeFile out (T.unpack interface)
  else if isFile then do
    -- target is a file
    content <- readFile target
    let result = P.parse schema target (T.pack content)

    if isRight result then do
        -- AST generated
        let interfaceStr = uncurry makeInterface (fromRight' result) config

        if isRight interfaceStr then do
          -- interface generated
          let interface = T.unpack ("import mongoose from 'mongoose';\n\n" <> fromRight' interfaceStr)

          if stdoutMode then putStr interface else do
            logSuccessLn stdoutMode $ "saved to '" <> out <> "'"
            writeFile out interface
          else do
            -- couldn't codegen
            logMultiLineError target $ fromLeft' interfaceStr
    else do
      -- couldn't parse
      logMultiLineError target $ errorBundlePretty (fromLeft' result)
  else do
    -- nothing exists at target dir
    logError $ "No file or directory found at '" <> target <> "'!"

main :: IO ()
main = montype =<< execParser opts
  where
    opts = info (cliArgs <**> helper <**> infoOption ("MonType v" <> (SP.style SP.Bold . SP.color SP.Yellow) (showVersion version)) (O.long "version" <> O.short 'v' <> O.help "Show version"))
      ( fullDesc
     <> progDesc "Generate TypeScript interfaces from Mongoose schemas"
     <> header "MonType" )
