module Common.Components.Questionnaire.RightPanel exposing
    ( RightPanel(..)
    , commentsPath
    , decoder
    , encode
    , isCommentsFamily
    )

{-| Which panel is open beside the questionnaire, if any.

Kept in the model and in local storage (`project-{uuid}-right-panel`), never in
the URL; the comments deep link is the one way in from a URL.

-}

import Common.Components.Questionnaire.ContentMsg exposing (PluginQuestionActionData)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type RightPanel
    = None
    | Search
    | UnansweredQuestions
    | TODOs
    | VersionHistory
    | CommentsOverview
    | Comments String
    | Warnings
    | PluginQuestionAction PluginQuestionActionData


{-| The question whose comments are open.
-}
commentsPath : RightPanel -> Maybe String
commentsPath rightPanel =
    case rightPanel of
        Comments path ->
            Just path

        _ ->
            Nothing


{-| The overview or one question's threads: the toolbar's Comments button is
pressed for both.
-}
isCommentsFamily : RightPanel -> Bool
isCommentsFamily rightPanel =
    case rightPanel of
        CommentsOverview ->
            True

        Comments _ ->
            True

        _ ->
            False


decoder : Decoder RightPanel
decoder =
    D.field "type" D.string
        |> D.andThen
            (\panelType ->
                case panelType of
                    "None" ->
                        D.succeed None

                    "Search" ->
                        D.succeed Search

                    "UnansweredQuestions" ->
                        D.succeed UnansweredQuestions

                    "TODOs" ->
                        D.succeed TODOs

                    "VersionHistory" ->
                        D.succeed VersionHistory

                    "CommentsOverview" ->
                        D.succeed CommentsOverview

                    "Comments" ->
                        D.map Comments (D.field "value" D.string)

                    "Warnings" ->
                        D.succeed Warnings

                    _ ->
                        D.fail ("Unknown right panel: " ++ panelType)
            )


{-| A plugin's panel is not stored: it belongs to the question it was opened
from, in this visit.
-}
encode : RightPanel -> E.Value
encode rightPanel =
    let
        simple panelType =
            E.object [ ( "type", E.string panelType ) ]
    in
    case rightPanel of
        None ->
            simple "None"

        Search ->
            simple "Search"

        UnansweredQuestions ->
            simple "UnansweredQuestions"

        TODOs ->
            simple "TODOs"

        VersionHistory ->
            simple "VersionHistory"

        CommentsOverview ->
            simple "CommentsOverview"

        Comments path ->
            E.object [ ( "type", E.string "Comments" ), ( "value", E.string path ) ]

        Warnings ->
            simple "Warnings"

        PluginQuestionAction _ ->
            simple "None"
