module ExtractImportGraphTest exposing (all)

import ExtractImportGraph exposing (rule)
import Review.Project exposing (modules)
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
                    |> Review.Test.expectGlobalErrors
                        [ importGraph
                            [ ( [ "A" ]
                              , [ [ "B" ], [ "C" ] ]
                              )
                            , ( [ "B" ]
                              , [ [ "C" ] ]
                              )
                            ]
                        ]
        ]


importGraph : List ( List String, List (List String) ) -> { message : String, details : List String }
importGraph modules =
    { message = "Import Graph"
    , details =
        "digraph {"
            :: List.map
                (\( moduleName, imports ) ->
                    "\""
                        ++ String.join "." moduleName
                        ++ "\""
                        ++ " -> {"
                        ++ String.join " " (List.map (\s -> "\"" ++ String.join "." s ++ "\"") imports)
                        ++ "}"
                )
                modules
            ++ [ "}" ]
    }
