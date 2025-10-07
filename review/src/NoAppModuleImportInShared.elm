module NoAppModuleImportInShared exposing (rule)

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoAppModuleImportInShared" ()
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.fromModuleRuleSchema


appModuleNames : List String
appModuleNames =
    [ "Registry", "Wizard" ]


importVisitor : Node Import -> List (Error {})
importVisitor node =
    let
        moduleName =
            Node.value (Node.value node).moduleName
    in
    case List.head moduleName of
        Just name ->
            if List.member name appModuleNames then
                [ Rule.error
                    { message = "Import from an app module into shared."
                    , details = [ "Do not import anything from app-specific modules into shared, as these are meant to be used across different apps." ]
                    }
                    (Node.range node)
                ]

            else
                []

        _ ->
            []
