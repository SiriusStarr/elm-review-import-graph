module ExtractImportGraphTest exposing (all)

import ExtractImportGraph exposing (rule)
import Json.Encode as Encode
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ExtractImportGraph"
        [ test "should report all imports" <|
            \() ->
                [ """module A exposing (..)

import B
import C
import Dict

foo = 1
"""
                , """module B exposing (..)

import C
import Set

bar = 1
"""
                , """module C exposing (..)

baz = 1
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectDataExtract
                        (Encode.object
                            [ ( "onlineGraph", Encode.string "https://dreampuf.github.io/GraphvizOnline/#digraph%20%7B%0A%20%20%22A%22%20-%3E%20%7B%22B%22%20%22C%22%7D%0A%20%20%22B%22%20-%3E%20%7B%22C%22%7D%0A%7D" )
                            , ( "graph", Encode.string """digraph {
  "A" -> {"B" "C"}
  "B" -> {"C"}
}""" )
                            ]
                            |> Encode.encode 0
                        )
        ]
