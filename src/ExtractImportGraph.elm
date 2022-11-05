module ExtractImportGraph exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


type alias ProjectContext =
    { imports : List ( { name : ModuleName, isSourceModule : Bool }, List { name : ModuleName, isDependency : Bool, isSourceModule : Bool } )
    , dependencyModules : Set ModuleName
    , nonSourceModules : Set ModuleName
    }


type alias ModuleContext =
    { imports : List { name : ModuleName, isDependency : Bool, isSourceModule : Bool }
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
        |> Rule.withFinalProjectEvaluation finalEvaluationForProject
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
        (\moduleContext ->
            { imports =
                [ ( { name = moduleContext.moduleName, isSourceModule = not <| Set.member moduleContext.moduleName moduleContext.nonSourceModules }
                  , List.reverse moduleContext.imports
                  )
                ]
            , dependencyModules = moduleContext.dependencyModules
            , nonSourceModules = moduleContext.nonSourceModules
            }
        )


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


finalEvaluationForProject : ProjectContext -> List (Rule.Error { useErrorForModule : () })
finalEvaluationForProject projectContext =
    let
        toNode : List String -> String
        toNode mN =
            "\"" ++ String.join "." mN ++ "\""
    in
    [ Rule.globalError
        { message = "Import Graph"
        , details =
            List.filterMap
                (\( { name, isSourceModule }, imports ) ->
                    let
                        relevantImports =
                            List.filter (not << .isDependency) imports
                    in
                    if List.isEmpty relevantImports || not isSourceModule then
                        Nothing

                    else
                        toNode name
                            ++ " -> {"
                            ++ String.join " " (List.map (toNode << .name) relevantImports)
                            ++ "}"
                            |> Just
                )
                projectContext.imports
                |> (\es -> "digraph {" :: es ++ [ "}" ])
        }
    ]


importVisitor : Node Import -> ModuleContext -> ( List never, ModuleContext )
importVisitor imp context =
    Node.value imp
        |> .moduleName
        |> Node.value
        |> (\n ->
                ( []
                , { context
                    | imports =
                        { name = n
                        , isDependency = Set.member n context.dependencyModules
                        , isSourceModule = not <| Set.member n context.nonSourceModules
                        }
                            :: context.imports
                  }
                )
           )
