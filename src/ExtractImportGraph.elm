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


{-| Extract the import graph of your project's modules.

    config =
        [ ExtractImportGraph.rule
        ]

This will generate a description of a graph like the one below (example from [`rtfeldman/elm-spa-example`](https://github.com/rtfeldman/elm-spa-example)).

    digraph {
      "Api" -> {"Api.Endpoint" "Avatar" "Username"}
      "Api.Endpoint" -> {"Article.Slug" "CommentId" "Username"}
      "Article" -> {"Api" "Api.Endpoint" "Article.Body" "Article.Slug" "Article.Tag" "Author" "Profile" "Username" "Viewer"}
      "Article.Comment" -> {"Api" "Api.Endpoint" "Article" "Article.Slug" "Author" "CommentId" "Profile"}
      "Article.Feed" -> {"Api" "Article" "Article.Slug" "Article.Tag" "Author" "Avatar" "Page" "PaginatedList" "Profile" "Route" "Session" "Timestamp" "Username"}
      "Article.Tag" -> {"Api" "Api.Endpoint"}
      "Author" -> {"Api" "Api.Endpoint" "Profile" "Route" "Username" "Viewer"}
      "Avatar" -> {"Asset"}
      "Loading" -> {"Asset"}
      "Main" -> {"Api" "Article.Slug" "Avatar" "Page" "Page.Article" "Page.Article.Editor" "Page.Blank" "Page.Home" "Page.Login" "Page.NotFound" "Page.Profile" "Page.Register" "Page.Settings" "Route" "Session" "Username" "Viewer"}
      "Page" -> {"Api" "Avatar" "Profile" "Route" "Session" "Username" "Viewer"}
      "Page.Article" -> {"Api" "Api.Endpoint" "Article" "Article.Body" "Article.Comment" "Article.Slug" "Author" "Avatar" "CommentId" "Loading" "Log" "Page" "Profile" "Route" "Session" "Timestamp" "Username" "Viewer"}
      "Page.Article.Editor" -> {"Api" "Api.Endpoint" "Article" "Article.Body" "Article.Slug" "Loading" "Page" "Profile" "Route" "Session"}
      "Page.Home" -> {"Api" "Api.Endpoint" "Article" "Article.Feed" "Article.Tag" "Loading" "Log" "Page" "PaginatedList" "Session" "Username"}
      "Page.Login" -> {"Api" "Route" "Session" "Viewer"}
      "Page.NotFound" -> {"Asset"}
      "Page.Profile" -> {"Api" "Api.Endpoint" "Article" "Article.Feed" "Author" "Avatar" "Loading" "Log" "Page" "PaginatedList" "Profile" "Route" "Session" "Username" "Viewer"}
      "Page.Register" -> {"Api" "Route" "Session" "Viewer"}
      "Page.Settings" -> {"Api" "Api.Endpoint" "Avatar" "Email" "Loading" "Log" "Profile" "Route" "Session" "Username" "Viewer"}
      "Profile" -> {"Api" "Avatar" "Username"}
      "Route" -> {"Article.Slug" "Profile" "Username"}
      "Session" -> {"Api" "Avatar" "Profile" "Viewer"}
      "Viewer" -> {"Api" "Avatar" "Email" "Profile" "Username"}
    }

which you can then visualize using tools like [GraphvizOnline](https://dreampuf.github.io/GraphvizOnline/#digraph%20%7B%0A%20%20%22Api%22%20-%3E%20%7B%22Api.Endpoint%22%20%22Avatar%22%20%22Username%22%7D%0A%20%20%22Api.Endpoint%22%20-%3E%20%7B%22Article.Slug%22%20%22CommentId%22%20%22Username%22%7D%0A%20%20%22Article%22%20-%3E%20%7B%22Api%22%20%22Api.Endpoint%22%20%22Article.Body%22%20%22Article.Slug%22%20%22Article.Tag%22%20%22Author%22%20%22Profile%22%20%22Username%22%20%22Viewer%22%7D%0A%20%20%22Article.Comment%22%20-%3E%20%7B%22Api%22%20%22Api.Endpoint%22%20%22Article%22%20%22Article.Slug%22%20%22Author%22%20%22CommentId%22%20%22Profile%22%7D%0A%20%20%22Article.Feed%22%20-%3E%20%7B%22Api%22%20%22Article%22%20%22Article.Slug%22%20%22Article.Tag%22%20%22Author%22%20%22Avatar%22%20%22Page%22%20%22PaginatedList%22%20%22Profile%22%20%22Route%22%20%22Session%22%20%22Timestamp%22%20%22Username%22%7D%0A%20%20%22Article.Tag%22%20-%3E%20%7B%22Api%22%20%22Api.Endpoint%22%7D%0A%20%20%22Author%22%20-%3E%20%7B%22Api%22%20%22Api.Endpoint%22%20%22Profile%22%20%22Route%22%20%22Username%22%20%22Viewer%22%7D%0A%20%20%22Avatar%22%20-%3E%20%7B%22Asset%22%7D%0A%20%20%22Loading%22%20-%3E%20%7B%22Asset%22%7D%0A%20%20%22Main%22%20-%3E%20%7B%22Api%22%20%22Article.Slug%22%20%22Avatar%22%20%22Page%22%20%22Page.Article%22%20%22Page.Article.Editor%22%20%22Page.Blank%22%20%22Page.Home%22%20%22Page.Login%22%20%22Page.NotFound%22%20%22Page.Profile%22%20%22Page.Register%22%20%22Page.Settings%22%20%22Route%22%20%22Session%22%20%22Username%22%20%22Viewer%22%7D%0A%20%20%22Page%22%20-%3E%20%7B%22Api%22%20%22Avatar%22%20%22Profile%22%20%22Route%22%20%22Session%22%20%22Username%22%20%22Viewer%22%7D%0A%20%20%22Page.Article%22%20-%3E%20%7B%22Api%22%20%22Api.Endpoint%22%20%22Article%22%20%22Article.Body%22%20%22Article.Comment%22%20%22Article.Slug%22%20%22Author%22%20%22Avatar%22%20%22CommentId%22%20%22Loading%22%20%22Log%22%20%22Page%22%20%22Profile%22%20%22Route%22%20%22Session%22%20%22Timestamp%22%20%22Username%22%20%22Viewer%22%7D%0A%20%20%22Page.Article.Editor%22%20-%3E%20%7B%22Api%22%20%22Api.Endpoint%22%20%22Article%22%20%22Article.Body%22%20%22Article.Slug%22%20%22Loading%22%20%22Page%22%20%22Profile%22%20%22Route%22%20%22Session%22%7D%0A%20%20%22Page.Home%22%20-%3E%20%7B%22Api%22%20%22Api.Endpoint%22%20%22Article%22%20%22Article.Feed%22%20%22Article.Tag%22%20%22Loading%22%20%22Log%22%20%22Page%22%20%22PaginatedList%22%20%22Session%22%20%22Username%22%7D%0A%20%20%22Page.Login%22%20-%3E%20%7B%22Api%22%20%22Route%22%20%22Session%22%20%22Viewer%22%7D%0A%20%20%22Page.NotFound%22%20-%3E%20%7B%22Asset%22%7D%0A%20%20%22Page.Profile%22%20-%3E%20%7B%22Api%22%20%22Api.Endpoint%22%20%22Article%22%20%22Article.Feed%22%20%22Author%22%20%22Avatar%22%20%22Loading%22%20%22Log%22%20%22Page%22%20%22PaginatedList%22%20%22Profile%22%20%22Route%22%20%22Session%22%20%22Username%22%20%22Viewer%22%7D%0A%20%20%22Page.Register%22%20-%3E%20%7B%22Api%22%20%22Route%22%20%22Session%22%20%22Viewer%22%7D%0A%20%20%22Page.Settings%22%20-%3E%20%7B%22Api%22%20%22Api.Endpoint%22%20%22Avatar%22%20%22Email%22%20%22Loading%22%20%22Log%22%20%22Profile%22%20%22Route%22%20%22Session%22%20%22Username%22%20%22Viewer%22%7D%0A%20%20%22Profile%22%20-%3E%20%7B%22Api%22%20%22Avatar%22%20%22Username%22%7D%0A%20%20%22Route%22%20-%3E%20%7B%22Article.Slug%22%20%22Profile%22%20%22Username%22%7D%0A%20%20%22Session%22%20-%3E%20%7B%22Api%22%20%22Avatar%22%20%22Profile%22%20%22Viewer%22%7D%0A%20%20%22Viewer%22%20-%3E%20%7B%22Api%22%20%22Avatar%22%20%22Email%22%20%22Profile%22%20%22Username%22%7D%0A%7D).


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-import-graph/preview --extract --report=json --rules ExtractImportGraph
```

then access the data at `.extracts.ExtractImportGraph`. If you have [`jq`](https://stedolan.github.io/jq/) installed, you can run the following command:

```bash
elm-review --template SiriusStarr/elm-review-import-graph/preview --extract --report=json --rules ExtractImportGraph | jq -r '.extracts.ExtractImportGraph'
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
        |> Rule.withDataExtractor dataExtractor
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { imports : Dict ModuleName String
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
                    moduleContext.imports
                        |> List.reverse
                        |> List.map wrapNameInQuotes
                        |> String.join " "
                        |> Dict.singleton moduleName

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


dataExtractor : ProjectContext -> Encode.Value
dataExtractor projectContext =
    let
        nodes : List String
        nodes =
            projectContext.imports
                |> Dict.toList
                |> List.map
                    (\( name, imports ) ->
                        "  "
                            ++ wrapNameInQuotes name
                            ++ " -> {"
                            ++ imports
                            ++ "}"
                    )

        graph : String
        graph =
            "digraph {\n" ++ String.join "\n" nodes ++ "\n}"
    in
    Encode.object
        [ ( "onlineGraph", Encode.string ("https://dreampuf.github.io/GraphvizOnline/#" ++ escapeUrlCharacters graph) )
        , ( "graph", Encode.string graph )
        ]


escapeUrlCharacters : String -> String
escapeUrlCharacters graph =
    graph
        |> String.replace " " "%20"
        |> String.replace "\"" "%22"
        |> String.replace "{" "%7B"
        |> String.replace "}" "%7D"
        |> String.replace ">" "%3E"
        |> String.replace "\n" "%0A"


wrapNameInQuotes : ModuleName -> String
wrapNameInQuotes mN =
    "\"" ++ String.join "." mN ++ "\""


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
