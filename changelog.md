2009.10.30
----------

### Feature

* add remoteHost field

2009.7.15
---------

### Feature

* new hackCache header using strict bytestrings

2009.5.19
---------

### Feature

* Use lazy bytestring for performance and hinting utf-8 encoding
* remove some unused fields
* use haskell naming convention

2009.4.52
---------

### Feature

* Remove useless type alias

### Fix

* Default error stream should take utf8 string


2009.4.51
---------

### Feature

* add documentation

2009.4.50
---------

### Feature

* add version info in default

2009.4.30
----------

### Feature

* seperate handler from core module
* seperate contrib from Hack.hs
* now Hack.hs is only an interface file, where everything links to. This allows me to soft link handler in Hack.Contrib and Hack.Handler in development mode :)

2009.4.29
-----------

### Fix

* -Wall clean


2009.4.28
-----------

### Fix

* File return status code 200 on success

2009.4.27
---------

### Fix

* hyena handler show real message ( Safari won't work without this fix )

2009.4.26
---------

### Fix

* add hyena handler

2009.4.25
---------

### Featuer

* more middleware
* hyena handler for simple use server

2009.4.23
---------

### Feature

* more middleware
    * ContentSize
    * SimpleAccessLogger

* request helper

2009.4.22
---------

### Feature

* more middleware
  * ContentType
  * RawRouter
  * SimpleRouter

2009.4.21
------------

### Feature

* demo SimpleRoute middleware

### Fix

* env for kibro adapter
* homepage in hack.cabal

2009.4.20
-----------

### Feature

* raw specification
* shiny kibro hack handler
