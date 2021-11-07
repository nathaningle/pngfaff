{- |
Module      : Options
Copyright   : (c) 2021 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Read command-line arguments.
-}
module Options where

import           Options.Applicative


data FaffCommand = Dump FilePath
                 | DumpZdata FilePath
                 | ListChunks FilePath
                 | Reencode FilePath FilePath
                 | Demo FilePath
                 deriving (Eq, Ord, Show)


optionParser :: ParserInfo FaffCommand
optionParser = info (helper <*> faffCommand) (fullDesc <> progDesc "Faff around with PNG files.")

faffCommand :: Parser FaffCommand
faffCommand = hsubparser
  (dumpCommand <> dumpZdataCommand <> listChunksCommand <> reencodeCommand <> demoCommand)

dumpCommand :: Mod CommandFields FaffCommand
dumpCommand = command "dump" (info dumpOptions (progDesc "Dump PNG file data"))

dumpOptions :: Parser FaffCommand
dumpOptions =
  Dump <$> strArgument (metavar "FILE" <> value "-" <> showDefault <> help "Input PNG file")

dumpZdataCommand :: Mod CommandFields FaffCommand
dumpZdataCommand = command "dump-zdata" (info dumpZdataOptions (progDesc "Dump zlib IDAT data"))

dumpZdataOptions :: Parser FaffCommand
dumpZdataOptions =
  DumpZdata <$> strArgument (metavar "FILE" <> value "-" <> showDefault <> help "Input PNG file")

listChunksCommand :: Mod CommandFields FaffCommand
listChunksCommand = command "list" (info listChunksOptions (progDesc "List PNG file chunks"))

listChunksOptions :: Parser FaffCommand
listChunksOptions =
  ListChunks <$> strArgument (metavar "FILE" <> value "-" <> showDefault <> help "Input PNG file")

reencodeCommand :: Mod CommandFields FaffCommand
reencodeCommand = command
  "reencode"
  (info reencodeOptions (progDesc "Round-trip a PNG file through the parser and encoder"))

reencodeOptions :: Parser FaffCommand
reencodeOptions =
  Reencode
    <$> strArgument (metavar "INFILE" <> value "-" <> showDefault <> help "Input PNG file")
    <*> strArgument (metavar "OUTFILE" <> value "-" <> showDefault <> help "Output PNG file")

demoCommand :: Mod CommandFields FaffCommand
demoCommand = command "demo" (info demoOptions (progDesc "Demo PNG file data"))

demoOptions :: Parser FaffCommand
demoOptions =
  Demo <$> strArgument (metavar "FILE" <> value "-" <> showDefault <> help "Output PNG file")
