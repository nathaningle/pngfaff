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


data FaffCommand = Dump FilePath | DumpZdata FilePath deriving (Eq, Ord, Show)


optionParser :: ParserInfo FaffCommand
optionParser = info (helper <*> faffCommand) (fullDesc <> progDesc "Faff around with PNG files.")

faffCommand :: Parser FaffCommand
faffCommand = hsubparser (dumpCommand <> dumpZdataCommand)

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