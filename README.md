[![Travis CI Status](https://travis-ci.org/jfischoff/tmp-postgres.svg?branch=master)](http://travis-ci.org/jfischoff/tmp-postgres)
# tmp-postgres

`tmp-postgres` is a libary for greating a temporary postgres instance on a random port for testing.

```haskell
result <- start []
case result of
  Left err -> print err
  Right tempDB -> do
     -- Do stuff
     stop tempDB
```