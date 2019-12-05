However the majority of the overhead in database testing in many situations is not from `tmp-postgres` directly but is from the cost of migrating the test database. This can easily take 10 secs depending on the complexity of the migrations.

In the next blog post I'll show how temp post
