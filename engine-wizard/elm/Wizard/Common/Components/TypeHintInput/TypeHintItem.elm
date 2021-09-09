module Wizard.Common.Components.TypeHintInput.TypeHintItem exposing
    ( memberSuggestion
    , packageSuggestion
    , questionnaireSuggestion
    , templateSuggestion
    )

import Html exposing (Html, div, span, strong, text)
import Html.Attributes exposing (class)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Data.TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Data.User as User
import Shared.Data.UserSuggestion exposing (UserSuggestion)
import Shared.Locale exposing (lg)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.UserIcon as UserIcon


memberSuggestion : UserSuggestion -> Html msg
memberSuggestion user =
    complexItem
        [ div [] [ UserIcon.viewSmall user ]
        , div [] [ text <| User.fullName user ]
        ]


questionnaireSuggestion : { a | name : String, description : Maybe String } -> Html msg
questionnaireSuggestion questionnaire =
    complexItem
        [ div [] [ ItemIcon.view { text = questionnaire.name, image = Nothing } ]
        , div []
            [ div []
                [ strong [] [ text questionnaire.name ]
                ]
            , div [] [ text <| Maybe.withDefault "" questionnaire.description ]
            ]
        ]


packageSuggestion : PackageSuggestion -> Html msg
packageSuggestion pkg =
    complexItem
        [ div [] [ ItemIcon.view { text = pkg.name, image = Nothing } ]
        , div []
            [ div []
                [ strong [] [ text pkg.name ]
                , span [ class "badge badge-light", dataCy "typehint-item_package_version" ] [ text <| Version.toString pkg.version ]
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
