module KnowledgeModels.Import.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Config exposing (Registry(..))
import Common.Html exposing (emptyNode, fa)
import Common.Html.Attribute exposing (detailClass)
import Common.View.Page as Page
import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KnowledgeModels.Import.FileImport.View as FileImportView
import KnowledgeModels.Import.Models exposing (ImportModel(..), Model)
import KnowledgeModels.Import.Msgs exposing (Msg(..))
import KnowledgeModels.Import.RegistryImport.View as RegistryImportView


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( registryActive, content ) =
            case model.importModel of
                FileImportModel fileImportModel ->
                    ( False
                    , Html.map FileImportMsg <|
                        FileImportView.view fileImportModel
                    )

                RegistryImportModel registryImportModel ->
                    ( True
                    , Html.map RegistryImportMsg <|
                        RegistryImportView.view appState registryImportModel
                    )

        navbar =
            case appState.config.registry of
                RegistryEnabled _ ->
                    viewNavbar registryActive

                _ ->
                    emptyNode
    in
    div [ detailClass "KnowledgeModels__Import" ]
        [ Page.header "Import Knowledge Model" []
        , navbar
        , content
        ]


viewNavbar : Bool -> Html Msg
viewNavbar registryActive =
    ul [ class "nav nav-tabs" ]
        [ li [ class "nav-item" ]
            [ a
                [ onClick ShowRegistryImport
                , class "nav-link link-with-icon"
                , classList [ ( "active", registryActive ) ]
                ]
                [ fa "cloud-download"
                , text "From Registry"
                ]
            ]
        , li [ class "nav-item" ]
            [ a
                [ onClick ShowFileImport
                , class "nav-link link-with-icon"
                , classList [ ( "active", not registryActive ) ]
                ]
                [ fa "upload"
                , text "From File"
                ]
            ]
        ]
