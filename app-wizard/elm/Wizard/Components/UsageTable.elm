module Wizard.Components.UsageTable exposing (view)

import Common.Components.Badge as Badge
import Common.Utils.ByteUnits as ByteUnits
import Gettext exposing (gettext)
import Html exposing (Html, div, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, style)
import Registry.Components.FontAwesome exposing (fas)
import Wizard.Api.Models.Usage exposing (Usage, UsageValue)
import Wizard.Data.AppState exposing (AppState)


view : AppState -> Bool -> Usage -> Html msg
view appState showSoftLimits usage =
    table [ class "table table-usage table-hover" ]
        [ tbody []
            [ viewUsageRowSimple appState showSoftLimits (gettext "Users" appState.locale) usage.users
            , viewUsageRowSimple appState showSoftLimits (gettext "Active Users" appState.locale) usage.activeUsers
            , viewUsageRowSimple appState showSoftLimits (gettext "Knowledge Model Editors" appState.locale) usage.branches
            , viewUsageRowSimple appState showSoftLimits (gettext "Knowledge Models" appState.locale) usage.knowledgeModels
            , viewUsageRowSimple appState showSoftLimits (gettext "Document Template Editors" appState.locale) usage.documentTemplateDrafts
            , viewUsageRowSimple appState showSoftLimits (gettext "Document Templates" appState.locale) usage.documentTemplates
            , viewUsageRowSimple appState showSoftLimits (gettext "Projects" appState.locale) usage.questionnaires
            , viewUsageRowSimple appState showSoftLimits (gettext "Documents" appState.locale) usage.documents
            , viewUsageRowSimple appState showSoftLimits (gettext "Locales" appState.locale) usage.locales
            , viewUsageRowBytes appState showSoftLimits (gettext "Storage" appState.locale) usage.storage
            ]
        ]


viewUsageRowSimple : AppState -> Bool -> String -> UsageValue -> Html msg
viewUsageRowSimple appState showSoftLimits =
    viewUsageRow appState showSoftLimits String.fromInt


viewUsageRowBytes : AppState -> Bool -> String -> UsageValue -> Html msg
viewUsageRowBytes appState showSoftLimits =
    viewUsageRow appState showSoftLimits ByteUnits.toReadable


viewUsageRow : AppState -> Bool -> (Int -> String) -> String -> UsageValue -> Html msg
viewUsageRow appState showSoftLimits mapValue usageLabel usageValue =
    let
        max =
            usageValue.max

        ( visibleValue, progressBar ) =
            if max > 0 then
                let
                    width =
                        String.fromFloat (toFloat usageValue.current * 100 / toFloat max) ++ "%"

                    barColorClass =
                        if usageValue.current >= max then
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

            else
                let
                    limit =
                        if showSoftLimits then
                            " / " ++ mapValue -max

                        else
                            ""
                in
                ( mapValue usageValue.current ++ limit
                , Badge.info []
                    [ fas "fa-infinity me-1"
                    , text (gettext "Unlimited" appState.locale)
                    ]
                )
    in
    tr []
        [ th [] [ text usageLabel ]
        , td [ class "text-end" ] [ text visibleValue ]
        , td [] [ progressBar ]
        ]
