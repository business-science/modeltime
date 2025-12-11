# Start parallel clusters / plans

Start parallel clusters / plans

## Usage

``` r
parallel_start(
  ...,
  .method = c("parallel", "spark", "future"),
  .export_vars = NULL,
  .packages = NULL
)

parallel_stop()
```

## Arguments

- ...:

  Parameters passed to underlying functions (See Details Section)

- .method:

  The method to create the parallel backend. Supports:

  - "future" - Uses the `future` package; foreach bridged via `doFuture`

  - "parallel" - Uses the `parallel` + `doParallel` packages

  - "spark" - Uses the `sparklyr` package

- .export_vars:

  Environment variables that can be sent to the workers (not needed for
  "future")

- .packages:

  Packages that can be sent to the workers (auto-handled by "future")

## Details

### future (`.method = "future"`)

Sets a
[`future::multisession`](https://future.futureverse.org/reference/multisession.html)
plan (portable across OSes) and registers a foreach backend via
[`doFuture::registerDoFuture()`](https://doFuture.futureverse.org/reference/registerDoFuture.html).
This avoids the `tune` foreach/future warning.

- Pass the first unnamed `...` argument as worker count (numeric) or
  omit to default to `parallelly::availableCores(logical = FALSE)` or 2
  if unknown.

### parallel (`.method = "parallel"`)

1.  `parallel::makeCluster(...)` 2) `doParallel::registerDoParallel(cl)`

2.  Set [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) on
    workers; optional `clusterExport` and package loads.

### spark (`.method = "spark"`)

Requires
[`sparklyr::spark_connect()`](https://rdrr.io/pkg/sparklyr/man/spark-connections.html);
registers foreach via `sparklyr::registerDoSpark(...)`.

## Examples

``` r
# Starts 2 clusters
parallel_start(2)

# Returns to sequential processing
parallel_stop()


```
