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


type alias ProjectContext =
    { imports : Dict ModuleName (List ModuleName)
    , dependencyModules : Set ModuleName
    }


type alias ModuleContext =
    { imports : List ModuleName
    , dependencyModules : Set ModuleName
    }


initContext : ProjectContext
initContext =
    { imports = Dict.empty
    , dependencyModules = Set.empty
    }


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
        (\projectContext ->
            { imports = []
            , dependencyModules = projectContext.dependencyModules
            }
        )


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\isInSourceDirectories moduleName moduleContext ->
            { imports =
                if isInSourceDirectories && not (List.isEmpty moduleContext.imports) then
                    Dict.singleton moduleName (List.reverse moduleContext.imports)

                else
                    Dict.empty
            , dependencyModules = moduleContext.dependencyModules
            }
        )
        |> Rule.withIsInSourceDirectories
        |> Rule.withModuleName


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { imports = Dict.union newContext.imports previousContext.imports
    , dependencyModules = previousContext.dependencyModules
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
            projectContext.imports
                |> Dict.toList
                |> List.map
                    (\( name, imports ) ->
                        "  "
                            ++ toNode name
                            ++ " -> {"
                            ++ String.join " " (List.map toNode imports)
                            ++ "}"
                    )
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
            | imports = moduleName :: context.imports
          }
        )
