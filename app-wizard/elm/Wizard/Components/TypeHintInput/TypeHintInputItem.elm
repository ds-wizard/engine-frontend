module Wizard.Components.TypeHintInput.TypeHintInputItem exposing
    ( memberSuggestion
    , packageSuggestion
    , packageSuggestionWithId
    , projectSuggestion
    , templateSuggestion
    , userGroupSuggestion
    )

import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Components.Badge as Badge
import Common.Components.TypeHintInput.TypeHintInputItem as TypeHintInputItem
import Common.Utils.DocumentTemplateUtils as DocumentTemplateUtils
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Gettext exposing (gettext)
import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Extra as Html
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


projectSuggestion : { a | name : String, description : Maybe String } -> Html msg
projectSuggestion project =
    TypeHintInputItem.complex
        [ div [] [ ItemIcon.view { text = project.name, image = Nothing } ]
        , div []
            [ div []
                [ strong [] [ text project.name ]
                ]
            , div [] [ text <| Maybe.withDefault "" project.description ]
            ]
        ]


packageSuggestionWithId : KnowledgeModelPackageSuggestion -> Html msg
packageSuggestionWithId =
    packageSuggestion True


packageSuggestion : Bool -> KnowledgeModelPackageSuggestion -> Html msg
packageSuggestion withPackageId pkg =
    let
        packageIdBadge =
            if withPackageId then
                div [] [ Badge.light [ class "ms-0 border", dataCy "typehint-item_package_version" ] [ text (KnowledgeModelUtils.getPackageId pkg) ] ]

            else
                Html.nothing
    in
    TypeHintInputItem.complex
        [ div [] [ ItemIcon.view { text = pkg.name, image = Nothing } ]
        , div []
            [ div [] [ strong [] [ text pkg.name ] ]
            , packageIdBadge
            , div [] [ text pkg.description ]
            ]
        ]


templateSuggestion : DocumentTemplateSuggestion -> Html msg
templateSuggestion template =
    TypeHintInputItem.complex
        [ div [] [ ItemIcon.view { text = template.name, image = Nothing } ]
        , div []
            [ div [] [ strong [] [ text template.name ] ]
            , div [] [ Badge.light [ class "ms-0 border" ] [ text (DocumentTemplateUtils.getId template) ] ]
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
