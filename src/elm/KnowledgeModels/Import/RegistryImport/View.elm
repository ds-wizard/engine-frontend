module KnowledgeModels.Import.RegistryImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (fa, linkTo)
import Common.View.ActionButton as ActionButton
import Common.View.FormResult as FormResult
import Html exposing (Html, br, code, div, h1, input, p, strong, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onInput)
import KnowledgeModels.Common.Package exposing (Package)
import KnowledgeModels.Import.RegistryImport.Models exposing (Model)
import KnowledgeModels.Import.RegistryImport.Msgs exposing (Msg(..))
import KnowledgeModels.Routing
import Routing


view : Model -> Html Msg
view model =
    let
        content =
            case model.pulling of
                Success _ ->
                    viewImported model.packageId

                _ ->
                    viewForm model
    in
    div []
        [ content ]


viewForm : Model -> Html Msg
viewForm model =
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
            ]
        ]


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
