# elm-review-import-graph

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.


## Provided rules

- [`ExtractImportGraph`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-import-graph/1.0.0/ExtractImportGraph) - Reports REPLACEME.


## Configuration

```elm
module ReviewConfig exposing (config)

import ExtractImportGraph
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ ExtractImportGraph.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-import-graph/example
```
