## A Quick `tmp-postgres` History

Before `tmp-postgres` existed I used [`pg_tmp`](http://eradman.com/ephemeralpg/) for my testing purposes. `pg_tmp` is a great little utility which I still use when I want to do some quick throwaway postgres testing.
I just have to type:
```
$ psql $(pg_tmp)
```
and I get a client to play around with a ephemeral postgres.

However I found the lack of prompt resource clean up problematic when running automatic tests.

To ensure that the temporary postgres resources were cleaned up reliably `tmp-postgres` was born.

`tmp-postgres` started as a file in a package. I then copied it to another package and as I was about to copy it to a third package I realized it should live as a standalone project.
