module Wizard.Common.Components.TypeHintInput.TypeHintItem exposing (packageSuggestion, templateSuggestion)

import Html exposing (Html, div, span, strong, text)
import Html.Attributes exposing (class)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Data.TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Locale exposing (lg)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.ItemIcon as ItemIcon


packageSuggestion : PackageSuggestion -> Html msg
packageSuggestion pkg =
    complexItem
        [ div [] [ ItemIcon.view { text = pkg.name, image = Nothing } ]
        , div []
            [ div []
                [ strong [] [ text pkg.name ]
                , span [ class "badge badge-light" ] [ text <| Version.toString pkg.version ]
                ]
            , div [] [ text pkg.description ]
            ]
        ]


templateSuggestion : AppState -> TemplateSuggestion -> Html msg
templateSuggestion appState template =
    let
        visibleName =
            if appState.config.template.recommendedTemplateId == Just template.id then
                template.name ++ " (" ++ lg "questionnaire.template.recommended" appState ++ ")"

            else
                template.name
    in
    complexItem
        [ div [] [ ItemIcon.view { text = template.name, image = Nothing } ]
        , div []
            [ div []
                [ strong [] [ text visibleName ]
                , span [ class "badge badge-light" ] [ text <| Version.toString template.version ]
                ]
            , div [] [ text template.description ]
            ]
        ]


complexItem : List (Html msg) -> Html msg
complexItem =
    div [ class "TypeHintInput__TypeHints__ComplexItem" ]
