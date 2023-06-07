[![R build
status](https://github.com/UrbanAnalyst/uta-engine/workflows/R-CMD-check/badge.svg)](https://github.com/UrbanAnalyst/uta-engine/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/UrbanAnalyst/uta-engine/branch/main/graph/badge.svg)](https://app.codecov.io/gh/UrbanAnalyst/uta-engine)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)

# UA Engine

Routing and aggregation engine for [‘Urban
Analyst’](https://www.urbananalyst.city/).

## Why?

Urban Analyst Engine aims to be the fastest and most scalable open
source multi-modal routing engine. Other notable options include:

1.  [“valhalla”, for single-mode
    routing](https://github.com/valhalla/valhalla)
2.  [“r5” for multi-modal routing](https://github.com/conveyal/r5)

In comparison, the `ua-engine`:

- Is considerably faster than either of these (see the “benchmarks”
  vignette);
- Permits far larger queries (`valhalla`, for example, is restricted to
  maximal queries of 2,000 pairwise comparisons, `uta-engine` easily
  extends to millions of comparisons);
- Allows true multi-modal travel times to be calculated, with specified,
  and potentially different, initial and final modes of transport.
- Automatically returns ratios of multi-modal to private automobile
  travel times.
- Includes estimates of travel times with private automobile that are
  empirically calibrated to extensive data, and are generally more
  accurate than equivalent times from `valhalla` or `r5`.
- Unlike all other routing engines, UA includes locality-specific
  estimates of additional times required to park automobiles at starts
  and ends of journeys.

## How?

Usage of this engine is not yet documented. Feel free to open an issue
if you’re interested in learning how to use this engine.
