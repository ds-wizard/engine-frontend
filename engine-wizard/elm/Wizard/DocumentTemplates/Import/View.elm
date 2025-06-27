module Wizard.DocumentTemplates.Import.View exposing (view)

import File exposing (File)
import File.Extra as File
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Shared.Html exposing (emptyNode, faSet)
import Wizard.Api.Models.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.FileImport as FileImport
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Common.Html.Attribute exposing (dataCy, detailClass)
import Wizard.Common.View.Page as Page
import Wizard.DocumentTemplates.Import.Models exposing (ImportModel(..), Model)
import Wizard.DocumentTemplates.Import.Msgs exposing (Msg(..))
import Wizard.DocumentTemplates.Import.RegistryImport.View as RegistryImportView
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( registryActive, content ) =
            case model.importModel of
                FileImportModel fileImportModel ->
                    ( False
                    , Html.map FileImportMsg <|
                        FileImport.view appState
                            { validate = Just (validateDocumentTemplateFile appState)
                            , doneRoute = Routes.documentTemplatesIndex
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
                    emptyNode
    in
    div [ detailClass "KnowledgeModels__Import" ]
        [ Page.headerWithGuideLink appState (gettext "Import Document Template" appState.locale) GuideLinks.documentTemplatesImport
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
                , text (gettext "From registry" appState.locale)
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


validateDocumentTemplateFile : AppState -> File -> Maybe String
validateDocumentTemplateFile appState file =
    let
        ext =
            File.ext file

        mime =
            File.mime file
    in
    if ext == Just "zip" || mime == "application/json" then
        Nothing

    else
        Just (gettext "This doesn't look like a document. Are you sure you picked the correct file?" appState.locale)
