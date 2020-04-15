module Options.Applicative.Simple
    ( parseOptions
    , module Options.Applicative
    )
where

import Prelude

import Options.Applicative

parseOptions :: Parser a -> IO a
parseOptions p = execParser $ info (p <**> helper) fullDesc
