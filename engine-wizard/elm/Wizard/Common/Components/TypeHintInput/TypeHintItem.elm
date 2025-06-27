module Wizard.Common.Components.TypeHintInput.TypeHintItem exposing
    ( memberSuggestion
    , packageSuggestion
    , packageSuggestionWithVersion
    , questionnaireSuggestion
    , simple
    , templateSuggestion
    , userGroupSuggestion
    )

import Gettext exposing (gettext)
import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class)
import Shared.Components.Badge as Badge
import Shared.Html exposing (emptyNode)
import Version
import Wizard.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Api.Models.PackageSuggestion exposing (PackageSuggestion)
import Wizard.Api.Models.User as User
import Wizard.Api.Models.UserGroupSuggestion exposing (UserGroupSuggestion)
import Wizard.Api.Models.UserSuggestion exposing (UserSuggestion)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.UserIcon as UserIcon


simple : (a -> String) -> a -> Html msg
simple toName item =
    div [ class "TypeHintInput__TypeHints__SimpleItem" ] [ text (toName item) ]


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


packageSuggestionWithVersion : PackageSuggestion -> Html msg
packageSuggestionWithVersion =
    packageSuggestion True


packageSuggestion : Bool -> PackageSuggestion -> Html msg
packageSuggestion withVersion pkg =
    let
        version =
            if withVersion then
                Badge.light [ dataCy "typehint-item_package_version" ] [ text <| Version.toString pkg.version ]

            else
                emptyNode
    in
    complexItem
        [ div [] [ ItemIcon.view { text = pkg.name, image = Nothing } ]
        , div []
            [ div []
                [ strong [] [ text pkg.name ]
                , version
                ]
            , div [] [ text pkg.description ]
            ]
        ]


templateSuggestion : DocumentTemplateSuggestion -> Html msg
templateSuggestion template =
    complexItem
        [ div [] [ ItemIcon.view { text = template.name, image = Nothing } ]
        , div []
            [ div []
                [ strong [] [ text template.name ]
                , Badge.light [] [ text <| Version.toString template.version ]
                ]
            , div [] [ text template.description ]
            ]
        ]


userGroupSuggestion : AppState -> UserGroupSuggestion -> Html msg
userGroupSuggestion appState userGroup =
    let
        privateBadge =
            if userGroup.private then
                Badge.dark [] [ text (gettext "private" appState.locale) ]

            else
                emptyNode
    in
    complexItem
        [ div [] [ ItemIcon.view { text = userGroup.name, image = Nothing } ]
        , div []
            [ div []
                [ strong [] [ text userGroup.name ]
                , privateBadge
                ]
            , div [] [ text (Maybe.withDefault "-" userGroup.description) ]
            ]
        ]


complexItem : List (Html msg) -> Html msg
complexItem =
    div [ class "TypeHintInput__TypeHints__ComplexItem" ]
