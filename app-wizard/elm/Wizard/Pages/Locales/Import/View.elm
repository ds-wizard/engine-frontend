module Wizard.Pages.Locales.Import.View exposing (view)

import Common.Components.FontAwesome exposing (faKmImportFromFile, faKmImportFromRegistry)
import Common.Components.Page as Page
import File exposing (File)
import File.Extra as File
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Wizard.Api.Models.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Components.FileImport as FileImport
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Locales.Import.Models exposing (ImportModel(..), Model)
import Wizard.Pages.Locales.Import.Msgs exposing (Msg(..))
import Wizard.Pages.Locales.Import.RegistryImport.View as RegistryImportView
import Wizard.Routes as Routes
import Wizard.Utils.HtmlAttributesUtils exposing (detailClass)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( registryActive, content ) =
            case model.importModel of
                FileImportModel fileImportModel ->
                    ( False
                    , Html.map FileImportMsg <|
                        FileImport.view appState
                            { validate = Just (validateLocaleFile appState)
                            , doneRoute = Routes.localesIndex
                            }
                            fileImportModel
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
                    Html.nothing
    in
    div [ detailClass "KnowledgeModels__Import" ]
        [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.localesImport) (gettext "Import Locale" appState.locale)
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
                , dataCy "locale_import_nav_registry"
                ]
                [ faKmImportFromRegistry
                , text (gettext "From registry" appState.locale)
                ]
            ]
        , li [ class "nav-item" ]
            [ a
                [ onClick ShowFileImport
                , class "nav-link"
                , classList [ ( "active", not registryActive ) ]
                , dataCy "locale_import_nav_file"
                ]
                [ faKmImportFromFile
                , text (gettext "From file" appState.locale)
                ]
            ]
        ]


validateLocaleFile : AppState -> File -> Maybe String
validateLocaleFile appState file =
    let
        ext =
            File.ext file

        mime =
            File.mime file
    in
    if ext == Just "zip" || mime == "application/zip" then
        Nothing

    else
        Just (gettext "This doesn't look like locales. Are you sure you picked the correct file?" appState.locale)
