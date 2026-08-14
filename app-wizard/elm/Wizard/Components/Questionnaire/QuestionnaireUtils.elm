module Wizard.Components.Questionnaire.QuestionnaireUtils exposing
    ( isPathCollapsed
    , pathToString
    )

import Set exposing (Set)


pathToString : List String -> String
pathToString =
    String.join "."


isPathCollapsed : String -> { a | collapsedPaths : Set String } -> Bool
isPathCollapsed path model =
    Set.member path model.collapsedPaths
