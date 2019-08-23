module KnowledgeModels.Import.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Config exposing (Registry(..))
import Common.Html exposing (emptyNode, fa, faSet)
import Common.Html.Attribute exposing (detailClass)
import Common.Locale exposing (l, lx)
import Common.View.Page as Page
import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KnowledgeModels.Import.FileImport.View as FileImportView
import KnowledgeModels.Import.Models exposing (ImportModel(..), Model)
import KnowledgeModels.Import.Msgs exposing (Msg(..))
import KnowledgeModels.Import.RegistryImport.View as RegistryImportView


l_ : String -> AppState -> String
l_ =
    l "KnowledgeModels.Import.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "KnowledgeModels.Import.View"


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
