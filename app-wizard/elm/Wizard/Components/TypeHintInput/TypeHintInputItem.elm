module Wizard.Components.TypeHintInput.TypeHintInputItem exposing
    ( memberSuggestion
    , packageSuggestion
    , packageSuggestionWithVersion
    , questionnaireSuggestion
    , templateSuggestion
    , userGroupSuggestion
    )

import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Components.Badge as Badge
import Common.Components.TypeHintInput.TypeHintInputItem as TypeHintInputItem
import Gettext exposing (gettext)
import Html exposing (Html, div, strong, text)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Extra as Html
import Version
import Wizard.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Api.Models.User as User
import Wizard.Api.Models.UserGroupSuggestion exposing (UserGroupSuggestion)
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Components.UserIcon as UserIcon
import Wizard.Data.AppState exposing (AppState)


memberSuggestion : UserSuggestion -> Html msg
memberSuggestion user =
    TypeHintInputItem.complex
        [ div [] [ UserIcon.viewSmall user ]
        , div [] [ text <| User.fullName user ]
        ]


questionnaireSuggestion : { a | name : String, description : Maybe String } -> Html msg
questionnaireSuggestion questionnaire =
    TypeHintInputItem.complex
        [ div [] [ ItemIcon.view { text = questionnaire.name, image = Nothing } ]
        , div []
            [ div []
                [ strong [] [ text questionnaire.name ]
                ]
            , div [] [ text <| Maybe.withDefault "" questionnaire.description ]
            ]
        ]


packageSuggestionWithVersion : KnowledgeModelPackageSuggestion -> Html msg
packageSuggestionWithVersion =
    packageSuggestion True


packageSuggestion : Bool -> KnowledgeModelPackageSuggestion -> Html msg
packageSuggestion withVersion pkg =
    let
        version =
            if withVersion then
                Badge.light [ dataCy "typehint-item_package_version" ] [ text <| Version.toString pkg.version ]

            else
                Html.nothing
    in
    TypeHintInputItem.complex
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
    TypeHintInputItem.complex
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
                Html.nothing
    in
    TypeHintInputItem.complex
        [ div [] [ ItemIcon.view { text = userGroup.name, image = Nothing } ]
        , div []
            [ div []
                [ strong [] [ text userGroup.name ]
                , privateBadge
                ]
            , div [] [ text (Maybe.withDefault "-" userGroup.description) ]
            ]
        ]
