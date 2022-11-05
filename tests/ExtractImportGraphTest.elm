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
                    |> Review.Test.expectDataExtract ("""digraph {
  "A" -> {"B" "C"}
  "B" -> {"C"}
}""" |> Encode.string |> Encode.encode 0)
        ]
