module Wizard.Components.Questionnaire.RightPanel exposing
    ( PluginQuestionActionData
    , RightPanel(..)
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Wizard.Api.Models.KnowledgeModel.Question exposing (Question)
import Wizard.Plugins.Plugin exposing (Plugin, ProjectQuestionActionConnector)


type RightPanel
    = None
    | Search
    | TODOs
    | VersionHistory
    | CommentsOverview
    | Comments String
    | Warnings
    | PluginQuestionAction PluginQuestionActionData


type alias PluginQuestionActionData =
    { plugin : Plugin
    , connector : ProjectQuestionActionConnector
    , question : Question
    , questionPath : String
    }


decoder : Decoder RightPanel
decoder =
    D.field "type" D.string
        |> D.andThen
            (\str ->
                case str of
                    "None" ->
                        D.succeed None

                    "Search" ->
                        D.succeed Search

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
                        D.fail <| "Unknown RightPanel: " ++ str
            )


encode : RightPanel -> E.Value
encode rightPanel =
    case rightPanel of
        None ->
            E.object
                [ ( "type", E.string "None" )
                ]

        Search ->
            E.object
                [ ( "type", E.string "Search" )
                ]

        TODOs ->
            E.object
                [ ( "type", E.string "TODOs" )
                ]

        VersionHistory ->
            E.object
                [ ( "type", E.string "VersionHistory" )
                ]

        CommentsOverview ->
            E.object
                [ ( "type", E.string "CommentsOverview" )
                ]

        Comments value ->
            E.object
                [ ( "type", E.string "Comments" )
                , ( "value", E.string value )
                ]

        Warnings ->
            E.object
                [ ( "type", E.string "Warnings" )
                ]

        PluginQuestionAction _ ->
            E.object
                [ ( "type", E.string "None" )
                ]
