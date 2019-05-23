module KnowledgeModels.Import.RegistryImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (fa, linkTo)
import Common.View.ActionButton as ActionButton
import Common.View.FormResult as FormResult
import Html exposing (Html, br, code, div, h1, input, p, strong, text)
import Html.Attributes exposing (class, placeholder, type_)
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
            case model.package of
                Success package ->
                    viewImported package

                _ ->
                    viewForm model
    in
    div []
        [ content ]


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ FormResult.errorOnlyView model.package
        , div [ class "jumbotron" ]
            [ div [ class "input-group" ]
                [ input [ onInput ChangePackageId, type_ "text", class "form-control", placeholder "Knowledge Model ID" ] []
                , div [ class "input-group-append" ]
                    [ ActionButton.button
                        { label = "Import"
                        , result = model.package
                        , msg = Submit
                        , dangerous = False
                        }
                    ]
                ]
            ]
        ]


viewImported : Package -> Html Msg
viewImported package =
    div [ class "jumbotron" ]
        [ h1 [] [ fa "check" ]
        , p [ class "lead" ]
            [ strong [] [ text package.name ]
            , text " "
            , code [] [ text package.id ]
            , text " has been imported!"
            ]
        , p [ class "lead" ]
            [ linkTo (Routing.KnowledgeModels <| KnowledgeModels.Routing.Detail package.id)
                [ class "btn btn-primary" ]
                [ text "View detail" ]
            ]
        ]
