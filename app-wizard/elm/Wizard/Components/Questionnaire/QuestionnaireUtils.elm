module Wizard.Components.Questionnaire.QuestionnaireUtils exposing
    ( isPathCollapsed
    , pathToFieldId
    , pathToString
    )

import Set exposing (Set)


pathToString : List String -> String
pathToString =
    String.join "."


{-| Turn a question path into an identifier usable as an HTML id or name. The path itself
starts with a UUID and contains dots, so it is prefixed and all characters that are not safe
in an identifier are replaced with underscores.
-}
pathToFieldId : String -> String
pathToFieldId path =
    let
        toSafeChar char =
            if Char.isAlphaNum char || char == '-' || char == '_' then
                char

            else
                '_'
    in
    "value_" ++ String.map toSafeChar path


isPathCollapsed : String -> { a | collapsedPaths : Set String } -> Bool
isPathCollapsed path model =
    Set.member path model.collapsedPaths
