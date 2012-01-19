{-# LANGUAGE OverloadedStrings #-}

import Hack2
import Hack2.Contrib.Response (set_body_bytestring)
import Hack2.Handler.SnapServer

app :: Application
app = \env -> 
  return $ 
    set_body_bytestring "Hello World" $
      Response 
        200 [ ("Content-Type", "text/plain") ] ""

main = run app