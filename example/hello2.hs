{-# LANGUAGE OverloadedStrings #-}

import Hack2
import Hack2.Handler.HappstackServer

app :: Application
app = \env -> return $ Response 
    { status  = 200
    , headers = [ ("Content-Type", "text/plain") ]
    , body    = "Hello World"
    }

main = run app