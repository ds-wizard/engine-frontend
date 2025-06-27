module Wizard.KnowledgeModels.Import.View exposing (view)

import File exposing (File)
import File.Extra as File
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Utils exposing (listInsertIf)
import Wizard.Api.Models.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.FileImport as FileImport
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Common.Html.Attribute exposing (dataCy, detailClass)
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.Import.Models exposing (ImportModel(..), Model, isFileImportModel, isOwlImportModel, isRegistryImportModel)
import Wizard.KnowledgeModels.Import.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Import.OwlImport.View as OwlImportView
import Wizard.KnowledgeModels.Import.RegistryImport.View as RegistryImportView
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    let
        registryNavItem =
            viewNavbarItem
                (text (gettext "From registry" appState.locale))
                (faSet "kmImport.fromRegistry" appState)
                (isRegistryImportModel model)
                ShowRegistryImport
                "km_import_nav_registry"

        fileNavItem =
            viewNavbarItem
                (text (gettext "From file" appState.locale))
                (faSet "kmImport.fromFile" appState)
                (isFileImportModel model)
                ShowFileImport
                "km_import_nav_file"

        owlNavItem =
            viewNavbarItem
                (text (gettext "From OWL" appState.locale))
                (faSet "kmImport.fromOwl" appState)
                (isOwlImportModel model)
                ShowOwlImport
                "km_import_nav_owl"

        registryEnabled =
            case appState.config.registry of
                RegistryEnabled _ ->
                    True

                _ ->
                    False

        navItems =
            []
                |> listInsertIf registryNavItem registryEnabled
                |> listInsertIf fileNavItem True
                |> listInsertIf owlNavItem appState.config.owl.enabled

        content =
            case model.importModel of
                FileImportModel fileImportModel ->
                    Html.map FileImportMsg <|
                        FileImport.view appState
                            { validate = Just (validateKmFile appState)
                            , doneRoute = Routes.knowledgeModelsIndex
                            }
                            fileImportModel

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
        [ Page.headerWithGuideLink appState (gettext "Import Knowledge Model" appState.locale) GuideLinks.kmImport
        , navbar
        , content
        ]


viewNavbarItem : Html msg -> Html msg -> Bool -> msg -> String -> Html msg
viewNavbarItem title icon isActive msg dataCyValue =
    li [ class "nav-item" ]
        [ a
            [ onClick msg
            , class "nav-link"
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


validateKmFile : AppState -> File -> Maybe String
validateKmFile appState file =
    let
        ext =
            File.ext file
    in
    if ext == Just "km" || ext == Just "json" then
        Nothing

    else
        Just (gettext "This doesn't look like a knowledge model. Are you sure you picked the correct file?" appState.locale)
