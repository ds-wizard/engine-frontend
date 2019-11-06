module Wizard.KnowledgeModels.Import.RegistryImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, a, code, div, h1, hr, input, p, text)
import Html.Attributes exposing (class, href, placeholder, target, type_, value)
import Html.Events exposing (onInput)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Config exposing (Registry(..))
import Wizard.Common.Html exposing (emptyNode, faSet, linkTo)
import Wizard.Common.Locale exposing (l, lg, lh, lx)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.KnowledgeModels.Import.RegistryImport.Models exposing (Model)
import Wizard.KnowledgeModels.Import.RegistryImport.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Routes
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.KnowledgeModels.Import.RegistryImport.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.KnowledgeModels.Import.RegistryImport.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KnowledgeModels.Import.RegistryImport.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.pulling of
                Success _ ->
                    viewImported appState model.packageId

                _ ->
                    viewForm appState model
    in
    div [ class "KnowledgeModels__Import__RegistryImport" ]
        [ content ]


viewForm : AppState -> Model -> Html Msg
viewForm appState model =
    div []
        [ FormResult.errorOnlyView appState model.pulling
        , div [ class "jumbotron" ]
            [ div [ class "input-group" ]
                [ input
                    [ onInput ChangePackageId
                    , type_ "text"
                    , value model.packageId
                    , class "form-control"
                    , placeholder <| lg "package.knowledgeModelId" appState
                    ]
                    []
                , div [ class "input-group-append" ]
                    [ ActionButton.button appState
                        { label = l_ "form.import" appState
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
                (lh_ "registryLink"
                    [ a [ href url, target "_blank" ] [ lx_ "registry" appState ]
                    ]
                    appState
                )

        _ ->
            emptyNode


viewImported : AppState -> String -> Html Msg
viewImported appState packageId =
    div [ class "jumbotron" ]
        [ h1 [] [ faSet "_global.success" appState ]
        , p [ class "lead" ]
            (lh_ "imported.message" [ code [] [ text packageId ] ] appState)
        , p [ class "lead" ]
            [ linkTo appState
                (Routes.KnowledgeModelsRoute <| Wizard.KnowledgeModels.Routes.DetailRoute packageId)
                [ class "btn btn-primary" ]
                [ lx_ "imported.action" appState ]
            ]
        ]
