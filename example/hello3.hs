{-# LANGUAGE OverloadedStrings #-}

import Hack2
import Hack2.Handler.HappstackServer
import Hack2.Contrib.Utils
import Hack2.Contrib.Middleware.URLMap
import Data.ByteString.Lazy.Char8 (pack)
import Data.Default

say :: Application
say = \env -> return $ def {body = pack $ show env, status = 200}

app :: Application
app = url_map [("/hello", say), ("/there", say)] empty_app

main = run app