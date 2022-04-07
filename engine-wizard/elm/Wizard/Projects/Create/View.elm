module Wizard.Projects.Create.View exposing (view)

import Html exposing (Html, div, li, ul)
import Html.Attributes exposing (class, classList)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, detailClass)
import Wizard.Common.View.Page as Page
import Wizard.Projects.Create.CustomCreate.View as CustomCreateView
import Wizard.Projects.Create.Models exposing (CreateModel(..), Model)
import Wizard.Projects.Create.Msgs exposing (Msg(..))
import Wizard.Projects.Create.ProjectCreateRoute exposing (ProjectCreateRoute(..))
import Wizard.Projects.Create.TemplateCreate.View as TemplateCreateView
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Create.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Create.View"


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
        [ Page.header (l_ "header.title" appState) []
        , navbar
        , content
        ]


viewNavbar : AppState -> Bool -> Html Msg
viewNavbar appState templateActive =
    ul [ class "nav nav-tabs" ]
        [ li [ class "nav-item" ]
            [ linkTo appState
                (Routes.projectsCreateTemplate Nothing)
                [ class "nav-link link-with-icon"
                , classList [ ( "active", templateActive ) ]
                , dataCy "project_create_nav_template"
                ]
                [ lx_ "navbar.fromTemplate" appState
                ]
            ]
        , li [ class "nav-item" ]
            [ linkTo appState
                (Routes.projectsCreateCustom Nothing)
                [ class "nav-link link-with-icon"
                , classList [ ( "active", not templateActive ) ]
                , dataCy "project_create_nav_custom"
                ]
                [ lx_ "navbar.custom" appState
                ]
            ]
        ]
