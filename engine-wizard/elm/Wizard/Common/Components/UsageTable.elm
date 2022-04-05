module Wizard.Common.Components.UsageTable exposing (view)

import Html exposing (Html, div, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, style)
import Shared.Common.ByteUnits as ByteUnits
import Shared.Data.Usage exposing (Usage, UsageValue)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Components.UsageTable"


view : AppState -> Usage -> Html msg
view appState usage =
    table [ class "table usage-table table-hover" ]
        [ tbody []
            [ viewUsageRowSimple (l_ "metric.users" appState) usage.users
            , viewUsageRowSimple (l_ "metric.activeUsers" appState) usage.activeUsers
            , viewUsageRowSimple (l_ "metric.kmEditors" appState) usage.branches
            , viewUsageRowSimple (l_ "metric.knowledgeModels" appState) usage.knowledgeModels
            , viewUsageRowSimple (l_ "metric.templates" appState) usage.templates
            , viewUsageRowSimple (l_ "metric.projects" appState) usage.questionnaires
            , viewUsageRowSimple (l_ "metric.documents" appState) usage.documents
            , viewUsageRowBytes (l_ "metric.storage" appState) usage.storage
            ]
        ]


viewUsageRowSimple : String -> UsageValue -> Html msg
viewUsageRowSimple =
    viewUsageRow String.fromInt


viewUsageRowBytes : String -> UsageValue -> Html msg
viewUsageRowBytes =
    viewUsageRow ByteUnits.toReadable


viewUsageRow : (Int -> String) -> String -> UsageValue -> Html msg
viewUsageRow mapValue usageLabel usageValue =
    let
        ( visibleValue, progressBar ) =
            case usageValue.max of
                Just max ->
                    let
                        width =
                            String.fromFloat (toFloat usageValue.current * 100 / toFloat max) ++ "%"

                        barColorClass =
                            if usageValue.current == max then
                                "bg-danger"

                            else if (toFloat usageValue.current / toFloat max) >= 0.8 then
                                "bg-warning"

                            else
                                "bg-info"
                    in
                    ( mapValue usageValue.current ++ " / " ++ mapValue max
                    , div [ class "progress" ]
                        [ div [ class ("progress-bar " ++ barColorClass), style "width" width ] [] ]
                    )

                _ ->
                    ( mapValue usageValue.current
                    , emptyNode
                    )
    in
    tr []
        [ th [] [ text usageLabel ]
        , td [ class "text-right" ] [ text visibleValue ]
        , td [] [ progressBar ]
        ]
