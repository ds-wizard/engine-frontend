module Wizard.Templates.Import.View exposing (view)

import Html exposing (Html, a, div, li, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Shared.Data.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.Page as Page
import Wizard.Templates.Import.FileImport.View as FileImportView
import Wizard.Templates.Import.Models exposing (ImportModel(..), Model)
import Wizard.Templates.Import.Msgs exposing (Msg(..))
import Wizard.Templates.Import.RegistryImport.View as RegistryImportView


l_ : String -> AppState -> String
l_ =
    l "Wizard.Templates.Import.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Templates.Import.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( registryActive, content ) =
            case model.importModel of
                FileImportModel fileImportModel ->
                    ( False
                    , Html.map FileImportMsg <|
                        FileImportView.view appState fileImportModel
                    )

                RegistryImportModel registryImportModel ->
                    ( True
                    , Html.map RegistryImportMsg <|
                        RegistryImportView.view appState registryImportModel
                    )

        navbar =
            case appState.config.registry of
                RegistryEnabled _ ->
                    viewNavbar appState registryActive

                _ ->
                    emptyNode
    in
    div [ detailClass "KnowledgeModels__Import" ]
        [ Page.header (l_ "header" appState) []
        , navbar
        , content
        ]


viewNavbar : AppState -> Bool -> Html Msg
viewNavbar appState registryActive =
    ul [ class "nav nav-tabs" ]
        [ li [ class "nav-item" ]
            [ a
                [ onClick ShowRegistryImport
                , class "nav-link link-with-icon"
                , classList [ ( "active", registryActive ) ]
                ]
                [ faSet "kmImport.fromRegistry" appState
                , lx_ "navbar.fromRegistry" appState
                ]
            ]
        , li [ class "nav-item" ]
            [ a
                [ onClick ShowFileImport
                , class "nav-link link-with-icon"
                , classList [ ( "active", not registryActive ) ]
                ]
                [ faSet "kmImport.fromFile" appState
                , lx_ "navbar.fromFile" appState
                ]
            ]
        ]
