[![Travis CI Status](https://travis-ci.org/jfischoff/tmp-postgres.svg?branch=master)](http://travis-ci.org/jfischoff/tmp-postgres)
# tmp-postgres

`tmp-postgres` is a libary for greating a temporary `postgres` instance on a random port for testing.

```haskell
result <- start []
case result of
  Left err -> print err
  Right tempDB -> do
     -- Do stuff
     stop tempDB
```

#Installation

## macOS
```
$ brew install postgres
$ stack install tmp-postgres
```

## Ubuntu

Ubuntu's PostgreSQL installation does not put `initdb` on the PATH. We need to add it manually.

```
$ sudo apt-get install postgresql-VERSION
$ echo "export PATH=$PATH:/usr/lib/postgresql/VERSION/bin/" >> /home/ubuntu/.bashrc
$ stack install tmp-postgres
```
