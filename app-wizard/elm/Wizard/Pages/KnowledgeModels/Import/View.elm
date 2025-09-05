module Wizard.Pages.KnowledgeModels.Import.View exposing (view)

import Common.Components.FontAwesome exposing (faKmImportFromFile, faKmImportFromOwl, faKmImportFromRegistry)
import Common.Components.Page as Page
import File exposing (File)
import File.Extra as File
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import List.Utils as List
import Wizard.Api.Models.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Components.FileImport as FileImport
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.KnowledgeModels.Import.Models exposing (ImportModel(..), Model, isFileImportModel, isOwlImportModel, isRegistryImportModel)
import Wizard.Pages.KnowledgeModels.Import.Msgs exposing (Msg(..))
import Wizard.Pages.KnowledgeModels.Import.OwlImport.View as OwlImportView
import Wizard.Pages.KnowledgeModels.Import.RegistryImport.View as RegistryImportView
import Wizard.Routes as Routes
import Wizard.Utils.HtmlAttributesUtils exposing (detailClass)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    let
        registryNavItem =
            viewNavbarItem
                (text (gettext "From registry" appState.locale))
                faKmImportFromRegistry
                (isRegistryImportModel model)
                ShowRegistryImport
                "km_import_nav_registry"

        fileNavItem =
            viewNavbarItem
                (text (gettext "From file" appState.locale))
                faKmImportFromFile
                (isFileImportModel model)
                ShowFileImport
                "km_import_nav_file"

        owlNavItem =
            viewNavbarItem
                (text (gettext "From OWL" appState.locale))
                faKmImportFromOwl
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
                |> List.insertIf registryNavItem registryEnabled
                |> List.insertIf fileNavItem True
                |> List.insertIf owlNavItem appState.config.owl.enabled

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
                Html.nothing
    in
    div [ detailClass "KnowledgeModels__Import" ]
        [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.kmImport) (gettext "Import Knowledge Model" appState.locale)
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
