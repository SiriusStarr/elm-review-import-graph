# elm-review-import-graph

Provides an [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule to display out the
import graph of your project.


## Provided rules

- [`ExtractImportGraph`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-import-graph/1.0.0/ExtractImportGraph) - Extract the import graph of your project's modules.


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
elm-review --template SiriusStarr/elm-review-import-graph/preview --extract --report=json
```
