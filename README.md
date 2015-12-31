# Advent of Code 2015 in Clojure

Clojure solutions for the [2015 Advent of Code](http://adventofcode.com/)

There is an active [subreddit](https://www.reddit.com/r/adventofcode) where solutions are discussed.

## Running

To run all the days:

```shell
$ lein run
```

This will take some time to complete.

To run a specific day, e.g. day 19:

```shell
$ lein run 19
```

## Testing

To run all the tests:

```shell
$ lein test
```

Not all of the days have tests.

To run a specific test, e.g. tests for day 25:

```shell
$ lein test :only advent-of-code.core-test/day-25
```
