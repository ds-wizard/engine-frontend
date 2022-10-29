module Wizard.KnowledgeModels.Import.RegistryImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, a, code, div, h1, hr, input, p, text)
import Html.Attributes exposing (class, href, placeholder, target, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Shared.Data.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Shared.Html exposing (emptyNode, faSet)
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.KnowledgeModels.Import.RegistryImport.Models exposing (Model)
import Wizard.KnowledgeModels.Import.RegistryImport.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


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
    div [ class "KnowledgeModels__Import__RegistryImport", dataCy "km_import_registry" ]
        [ content ]


viewForm : AppState -> Model -> Html Msg
viewForm appState model =
    div []
        [ FormResult.errorOnlyView appState model.pulling
        , div [ class "px-4 py-5 bg-light rounded-3" ]
            [ Html.form [ onSubmit Submit, class "input-group" ]
                [ input
                    [ onInput ChangePackageId
                    , type_ "text"
                    , value model.packageId
                    , class "form-control"
                    , placeholder <| gettext "Knowledge Model ID" appState.locale
                    ]
                    []
                , ActionButton.submit appState
                    { label = gettext "Import" appState.locale
                    , result = model.pulling
                    }
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
                (String.formatHtml
                    (gettext "You can find Knowledge Models in %s." appState.locale)
                    [ a [ href url, target "_blank" ]
                        [ text (gettext "DSW Registry" appState.locale) ]
                    ]
                )

        _ ->
            emptyNode


viewImported : AppState -> String -> Html Msg
viewImported appState packageId =
    div [ class "px-4 py-5 bg-light rounded-3" ]
        [ h1 [] [ faSet "_global.success" appState ]
        , p [ class "lead" ]
            (String.formatHtml
                (gettext "Knowledge Model %s has been imported!" appState.locale)
                [ code [] [ text packageId ] ]
            )
        , p [ class "lead" ]
            [ linkTo appState
                (Routes.knowledgeModelsDetail packageId)
                [ class "btn btn-primary" ]
                [ text (gettext "View detail" appState.locale) ]
            ]
        ]
