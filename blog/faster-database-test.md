# Faster Database Testing with the New `tmp-postgres`

`tmp-postgres` has hit 1.0.0.0 ... more like blown past 1.0.0.0. The latest version is 1.31.0.0 as I write this.

The latest version brings with it a host of improvements. It is faster, more configurable and has improved exception handling behavior. There are a lot of bits to cover but the speed improvements are the most fun so let's start there.

## The Baseline

`tmp-postgres` 0.3.0.1 is last "old" version before the rewrite that led to 1.0.0.0. The default startup and shutdown times I got are:

```
Baseline ~ 1.44 sec
```

## Doing Less is Faster

`tmp-postgres` was based heavily on [`pg_tmp`](http://eradman.com/ephemeralpg/). Like `pg_tmp` it would create a `test` database using `createdb` by default. However for most purposes this is not necessary. `initdb` creates a `postgres` database we can use for testing.

```
Creating a test database ~ 1.44 sec
No create                ~ 1.11 sec
```

## Faster Setup with `initdb` caching

Before a ephemeral postgres process can start a temporary database cluster needs to be created. This means calling `initdb` with the appriopiate arguments. This is the slowest part of naive `tmp-postgres` startup.

However for a given `initdb` version and input the process is referentially transparent so we can cache the output of `initdb`. This works great in practice because the input to `initdb` rarely changes.

```
No caching   ~ 1.11 sec
With caching ~ 0.353 sec?
```

## Cow is Faster

The start up time is now mostly copying the cached cluster. Most of the files in the cluster are not modified during the duration of a test. On newer operating systems we can use "copy on write" to make the copy faster.

```
No cow ~ 0.353 sec
cow    ~ 0.248 sec
```

The improvements are even more dramactic on linux which went from ? to *0.080* sec.

## But Wait, There's more!

The new `tmp-postgres` is 4x faster on macOS and 5x faster on linux but the story doesn't end here. In part 2 and 3 of this blog post series I'll show how to use additionally features to keep database testing fast as the size of the project grows.
