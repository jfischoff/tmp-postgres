# Faster Database Testing with the New `tmp-postgres`

`tmp-postgres` has hit 1.0.0.0 ... more like blown past 1.0.0.0. The latest version is 1.27.0.4 as I write this.

The latest version brings with it a host of improvements. It is faster, more configurable and has improved exception handling behavior. There are a lot of bits to cover but the speed improvements are the most fun so let's start there.

## The Baseline

`tmp-postgres` 0.3.0.1 is last "old" version before the rewrite that led to 1.0.0.0. The default startup and shutdown times I got are:

```
Baseline ~ 2.0 sec
```

## Doing Less is Faster

`tmp-postgres` was based heavily on [`pg_tmp`](http://eradman.com/ephemeralpg/). Like `pg_tmp` it would create a `test` database using `createdb` by default. However for most purposes this is not necessary. `initdb` creates a `postgres` database we can use for testing.

```
Creating a test database ~ 2.0  sec
No create                ~ 1.75 sec
```

## Faster Setup with `initdb` caching

Before a ephemeral postgres process can start a temporary database cluster needs to be created. This means calling `initdb` with the appriopiate arguments. This is the slowest part of naive `tmp-postgres` startup.

However for a given `initdb` version and input the process is referentially transparent so we can cache the output of `initdb`. This works great in practice because the input to `initdb` rarely changes.

```
No caching   ~ 1.0 sec
With caching ~ ?
```

## Cow is Faster

The start up time is now mostly copying the cached cluster. Most of the files in the cluster are not modified during the duration of a test. On newer operating systems we can use "copy on write" to make the copy faster.

```
No cow ~ 0.38 sec
cow    ~ 0.19 (0.08) sec
```

## Faster Shutdown

`postgres` has several [shutdown modes](https://www.postgresql.org/docs/current/server-shutdown.html). To shutdown `postgres` pre-rewrite I was sending a `SIGINT` signal which triggers "fast shutdown". However there is faster shutdown mode "Immediate Shutdown" which is triggered by a `SIGQUIT`. This is now the default.

```
Fast Shutdown       ~ 0.19 sec
Immediate Shutdown  ~ 0.18 sec
```

Not a lot but there are other reasons to use `SIGQUIT` as a default for our purposes.

## But Wait, There's more!

The new `tmp-postgres` is 4x faster on macOS and 5x faster on linux. That is a great improvement and increases the applicability of isolated database testing.

The story doesn't end here. In part 2 and 3 of this blog post series I'll show how to use additionally features to keep database testing fast even in increasingly complex situations like one finds in real projects.
