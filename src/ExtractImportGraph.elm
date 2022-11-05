module ExtractImportGraph exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Json.Encode as Encode
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


type alias ProjectContext =
    { imports : List ( { name : ModuleName, isSourceModule : Bool }, List { name : ModuleName, isSourceModule : Bool } )
    , dependencyModules : Set ModuleName
    , nonSourceModules : Set ModuleName
    }


type alias ModuleContext =
    { imports : List { name : ModuleName, isSourceModule : Bool }
    , moduleName : ModuleName
    , dependencyModules : Set ModuleName
    , nonSourceModules : Set ModuleName
    }


initContext : ProjectContext
initContext =
    { imports = [], dependencyModules = Set.empty, nonSourceModules = Set.empty }


{-| Reports... REPLACEME

    config =
        [ ExtractImportGraph.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-import-graph/example --rules ExtractImportGraph
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "ExtractImportGraph" initContext
        |> Rule.withDependenciesProjectVisitor dependencyVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withDataExtractor (dataExtractor >> Encode.string)
        |> Rule.fromProjectRuleSchema


dependencyVisitor : Dict String Dependency -> ProjectContext -> ( List never, ProjectContext )
dependencyVisitor ds context =
    Dict.values ds
        |> List.concatMap Dependency.modules
        |> List.map (String.split "." << .name)
        |> Set.fromList
        |> (\dModules -> ( [], { context | dependencyModules = dModules } ))


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\moduleName isSourceFile projectContext ->
            { imports = []
            , dependencyModules = projectContext.dependencyModules
            , moduleName = moduleName
            , nonSourceModules =
                if isSourceFile then
                    projectContext.nonSourceModules

                else
                    Set.insert moduleName projectContext.nonSourceModules
            }
        )
        |> Rule.withModuleName
        |> Rule.withIsInSourceDirectories


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\isInSourceDirectories moduleContext ->
            { imports =
                if isInSourceDirectories then
                    [ ( { name = moduleContext.moduleName, isSourceModule = True }
                      , List.reverse moduleContext.imports
                      )
                    ]

                else
                    []
            , dependencyModules = moduleContext.dependencyModules
            , nonSourceModules = moduleContext.nonSourceModules
            }
        )
        |> Rule.withIsInSourceDirectories


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { imports = previousContext.imports ++ newContext.imports
    , dependencyModules = Set.union newContext.dependencyModules previousContext.dependencyModules
    , nonSourceModules = Set.union newContext.nonSourceModules previousContext.nonSourceModules
    }


moduleVisitor :
    Rule.ModuleRuleSchema {} ModuleContext
    -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withImportVisitor importVisitor


dataExtractor : ProjectContext -> String
dataExtractor projectContext =
    let
        toNode : List String -> String
        toNode mN =
            "\"" ++ String.join "." mN ++ "\""

        nodes : List String
        nodes =
            List.filterMap
                (\( { name, isSourceModule }, imports ) ->
                    if List.isEmpty imports || not isSourceModule then
                        Nothing

                    else
                        "  "
                            ++ toNode name
                            ++ " -> {"
                            ++ String.join " " (List.map (toNode << .name) imports)
                            ++ "}"
                            |> Just
                )
                projectContext.imports
    in
    "digraph {\n" ++ String.join "\n" nodes ++ "\n}"


importVisitor : Node Import -> ModuleContext -> ( List never, ModuleContext )
importVisitor imp context =
    let
        moduleName : ModuleName
        moduleName =
            Node.value imp
                |> .moduleName
                |> Node.value
    in
    if Set.member moduleName context.dependencyModules then
        ( [], context )

    else
        ( []
        , { context
            | imports =
                { name = moduleName
                , isSourceModule = not <| Set.member moduleName context.nonSourceModules
                }
                    :: context.imports
          }
        )
