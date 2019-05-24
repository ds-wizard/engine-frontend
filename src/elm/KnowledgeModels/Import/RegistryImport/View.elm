module KnowledgeModels.Import.RegistryImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Config exposing (Registry(..))
import Common.Html exposing (emptyNode, fa, linkTo)
import Common.View.ActionButton as ActionButton
import Common.View.FormResult as FormResult
import Html exposing (Html, a, code, div, h1, hr, input, p, text)
import Html.Attributes exposing (class, href, placeholder, target, type_, value)
import Html.Events exposing (onInput)
import KnowledgeModels.Import.RegistryImport.Models exposing (Model)
import KnowledgeModels.Import.RegistryImport.Msgs exposing (Msg(..))
import KnowledgeModels.Routing
import Routing


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.pulling of
                Success _ ->
                    viewImported model.packageId

                _ ->
                    viewForm appState model
    in
    div [ class "KnowledgeModels__Import__RegistryImport" ]
        [ content ]


viewForm : AppState -> Model -> Html Msg
viewForm appState model =
    div []
        [ FormResult.errorOnlyView model.pulling
        , div [ class "jumbotron" ]
            [ div [ class "input-group" ]
                [ input [ onInput ChangePackageId, type_ "text", value model.packageId, class "form-control", placeholder "Knowledge Model ID" ] []
                , div [ class "input-group-append" ]
                    [ ActionButton.button
                        { label = "Import"
                        , result = model.pulling
                        , msg = Submit
                        , dangerous = False
                        }
                    ]
                ]
            , hr [] []
            , viewRegistryText appState
            ]
        ]


viewRegistryText : AppState -> Html msg
viewRegistryText appState =
    case appState.config.registry of
        RegistryEnabled url ->
            p []
                [ text "You can find knowledge models in the "
                , a [ href url, target "_blank" ] [ text "registry" ]
                , text "."
                ]

        _ ->
            emptyNode


viewImported : String -> Html Msg
viewImported packageId =
    div [ class "jumbotron" ]
        [ h1 [] [ fa "check" ]
        , p [ class "lead" ]
            [ text "Knowledge model "
            , code [] [ text packageId ]
            , text " has been imported!"
            ]
        , p [ class "lead" ]
            [ linkTo (Routing.KnowledgeModels <| KnowledgeModels.Routing.Detail packageId)
                [ class "btn btn-primary" ]
                [ text "View detail" ]
            ]
        ]
