module Wizard.Common.Components.Questionnaire.RightPanel exposing
    ( RightPanel(..)
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type RightPanel
    = None
    | TODOs
    | VersionHistory
    | CommentsOverview
    | Comments String
    | Warnings


decoder : Decoder RightPanel
decoder =
    D.field "type" D.string
        |> D.andThen
            (\str ->
                case str of
                    "None" ->
                        D.succeed None

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
