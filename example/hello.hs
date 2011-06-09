{-# LANGUAGE OverloadedStrings #-}

import Hack2

app :: Application
app = \env -> return $
  Response 200 [ ("Content-Type", "text/plain") ] ("Hello World")

