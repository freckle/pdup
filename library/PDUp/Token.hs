module PDUp.Token
  ( Token(..)
  )
where

import RIO

newtype Token = Token
  { unToken :: Text
  }
