module Wizard.KnowledgeModels.Import.View exposing (view)

import Html exposing (Html, a, div, li, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Shared.Data.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
import Shared.Utils exposing (listInsertIf)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy, detailClass)
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.Import.FileImport.View as FileImportView
import Wizard.KnowledgeModels.Import.Models exposing (ImportModel(..), Model, isFileImportModel, isOwlImportModel, isRegistryImportModel)
import Wizard.KnowledgeModels.Import.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Import.OwlImport.View as OwlImportView
import Wizard.KnowledgeModels.Import.RegistryImport.View as RegistryImportView


l_ : String -> AppState -> String
l_ =
    l "Wizard.KnowledgeModels.Import.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KnowledgeModels.Import.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        owlNavItem =
            viewNavbarItem
                (lx_ "navbar.fromOwl" appState)
                (faSet "kmImport.fromOwl" appState)
                (isOwlImportModel model)
                ShowOwlImport
                "km_import_nav_owl"

        registryNavItem =
            viewNavbarItem
                (lx_ "navbar.fromRegistry" appState)
                (faSet "kmImport.fromRegistry" appState)
                (isRegistryImportModel model)
                ShowRegistryImport
                "km_import_nav_registry"

        fileNavItem =
            viewNavbarItem
                (lx_ "navbar.fromFile" appState)
                (faSet "kmImport.fromFile" appState)
                (isFileImportModel model)
                ShowFileImport
                "km_import_nav_file"

        registryEnabled =
            case appState.config.registry of
                RegistryEnabled _ ->
                    True

                _ ->
                    False

        navItems =
            []
                |> listInsertIf owlNavItem appState.config.owl.enabled
                |> listInsertIf registryNavItem registryEnabled
                |> listInsertIf fileNavItem True

        content =
            case model.importModel of
                FileImportModel fileImportModel ->
                    Html.map FileImportMsg <|
                        FileImportView.view appState fileImportModel

                RegistryImportModel registryImportModel ->
                    Html.map RegistryImportMsg <|
                        RegistryImportView.view appState registryImportModel

                OwlImportModel owlImportModel ->
                    Html.map OwlImportMsg <|
                        OwlImportView.view appState owlImportModel

        navbar =
            if List.length navItems > 1 then
                viewNavbar navItems

            else
                emptyNode
    in
    div [ detailClass "KnowledgeModels__Import" ]
        [ Page.header (l_ "header" appState) []
        , navbar
        , content
        ]


viewNavbarItem : Html msg -> Html msg -> Bool -> msg -> String -> Html msg
viewNavbarItem title icon isActive msg dataCyValue =
    li [ class "nav-item" ]
        [ a
            [ onClick msg
            , class "nav-link link-with-icon"
            , classList [ ( "active", isActive ) ]
            , dataCy dataCyValue
            ]
            [ icon
            , title
            ]
        ]


viewNavbar : List (Html Msg) -> Html Msg
viewNavbar items =
    ul [ class "nav nav-tabs" ] items
