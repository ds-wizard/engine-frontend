module Wizard.Common.Components.UsageTable exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, style)
import Shared.Common.ByteUnits as ByteUnits
import Shared.Data.Usage exposing (Usage, UsageValue)
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState exposing (AppState)


view : AppState -> Usage -> Html msg
view appState usage =
    table [ class "table table-usage table-hover" ]
        [ tbody []
            [ viewUsageRowSimple (gettext "Users" appState.locale) usage.users
            , viewUsageRowSimple (gettext "Active Users" appState.locale) usage.activeUsers
            , viewUsageRowSimple (gettext "Knowledge Model Editors" appState.locale) usage.branches
            , viewUsageRowSimple (gettext "Knowledge Models" appState.locale) usage.knowledgeModels
            , viewUsageRowSimple (gettext "Document Template Editors" appState.locale) usage.documentTemplateDrafts
            , viewUsageRowSimple (gettext "Document Templates" appState.locale) usage.documentTemplates
            , viewUsageRowSimple (gettext "Projects" appState.locale) usage.questionnaires
            , viewUsageRowSimple (gettext "Documents" appState.locale) usage.documents
            , viewUsageRowSimple (gettext "Locales" appState.locale) usage.locales
            , viewUsageRowBytes (gettext "Storage" appState.locale) usage.storage
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
        , td [ class "text-end" ] [ text visibleValue ]
        , td [] [ progressBar ]
        ]
