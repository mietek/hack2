{-# LANGUAGE OverloadedStrings #-}

import Hack2
import Hack2.Handler.HappstackServer

app :: Application
app = \env -> return $
  Response 200 [ ("Content-Type", "text/plain") ] "Hello World"

main = run app