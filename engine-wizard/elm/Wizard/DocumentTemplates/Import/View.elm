module Wizard.DocumentTemplates.Import.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Shared.Data.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Shared.Html exposing (emptyNode, faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy, detailClass)
import Wizard.Common.View.Page as Page
import Wizard.DocumentTemplates.Import.FileImport.View as FileImportView
import Wizard.DocumentTemplates.Import.Models exposing (ImportModel(..), Model)
import Wizard.DocumentTemplates.Import.Msgs exposing (Msg(..))
import Wizard.DocumentTemplates.Import.RegistryImport.View as RegistryImportView


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
        [ Page.header (gettext "Import Document Template" appState.locale) []
        , navbar
        , content
        ]


viewNavbar : AppState -> Bool -> Html Msg
viewNavbar appState registryActive =
    ul [ class "nav nav-tabs" ]
        [ li [ class "nav-item" ]
            [ a
                [ onClick ShowRegistryImport
                , class "nav-link"
                , classList [ ( "active", registryActive ) ]
                , dataCy "template_import_nav_registry"
                ]
                [ faSet "kmImport.fromRegistry" appState
                , text (gettext "From DSW Registry" appState.locale)
                ]
            ]
        , li [ class "nav-item" ]
            [ a
                [ onClick ShowFileImport
                , class "nav-link"
                , classList [ ( "active", not registryActive ) ]
                , dataCy "template_import_nav_file"
                ]
                [ faSet "kmImport.fromFile" appState
                , text (gettext "From file" appState.locale)
                ]
            ]
        ]
