module Wizard.Projects.Create.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, li, text, ul)
import Html.Attributes exposing (class, classList)
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, detailClass)
import Wizard.Common.View.Page as Page
import Wizard.Projects.Create.CustomCreate.View as CustomCreateView
import Wizard.Projects.Create.Models exposing (CreateModel(..), Model)
import Wizard.Projects.Create.Msgs exposing (Msg(..))
import Wizard.Projects.Create.TemplateCreate.View as TemplateCreateView
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( templateActive, content ) =
            case model.createModel of
                TemplateCreateModel templateCreateModel ->
                    ( True
                    , Html.map TemplateCreateMsg <| TemplateCreateView.view appState templateCreateModel
                    )

                CustomCreateModel customCreateModel ->
                    ( False
                    , Html.map CustomCreateMsg <| CustomCreateView.view appState customCreateModel
                    )

        createFromTemplateAvailable =
            Feature.projectsCreateFromTemplate appState

        createCustomAvailable =
            Feature.projectsCreateCustom appState

        navbar =
            if createFromTemplateAvailable && createCustomAvailable then
                viewNavbar appState templateActive

            else
                emptyNode
    in
    div [ detailClass "Questionnaires__Create" ]
        [ Page.header (gettext "Create Project" appState.locale) []
        , navbar
        , content
        ]


viewNavbar : AppState -> Bool -> Html Msg
viewNavbar appState templateActive =
    ul [ class "nav nav-tabs" ]
        [ li [ class "nav-item" ]
            [ linkTo appState
                (Routes.projectsCreateTemplate Nothing)
                [ class "nav-link"
                , classList [ ( "active", templateActive ) ]
                , dataCy "project_create_nav_template"
                ]
                [ text (gettext "From Project Template" appState.locale)
                ]
            ]
        , li [ class "nav-item" ]
            [ linkTo appState
                (Routes.projectsCreateCustom Nothing)
                [ class "nav-link"
                , classList [ ( "active", not templateActive ) ]
                , dataCy "project_create_nav_custom"
                ]
                [ text (gettext "Custom" appState.locale)
                ]
            ]
        ]
