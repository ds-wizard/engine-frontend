module Wizard.Pages.DocumentTemplates.Import.RegistryImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Components.ActionButton as ActionButton
import Common.Components.FontAwesome exposing (faSuccess)
import Common.Components.FormResult as FormResult
import Gettext exposing (gettext)
import Html exposing (Html, a, code, div, h1, hr, input, p, text)
import Html.Attributes exposing (class, href, placeholder, target, type_, value)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onInput, onSubmit)
import Html.Extra as Html
import String.Format as String
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Wizard.Api.Models.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplates.Import.RegistryImport.Models exposing (Model)
import Wizard.Pages.DocumentTemplates.Import.RegistryImport.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.pulling of
                Success _ ->
                    viewImported appState model.documentTemplateId

                _ ->
                    viewForm appState model
    in
    div [ class "KnowledgeModels__Import__RegistryImport", dataCy "template_import_registry" ]
        [ content ]


viewForm : AppState -> Model -> Html Msg
viewForm appState model =
    div []
        [ FormResult.errorOnlyView model.pulling
        , div [ class "px-4 py-5 bg-light rounded-3" ]
            [ Html.form [ onSubmit Submit, class "input-group" ]
                [ input
                    [ onInput ChangeTemplateId
                    , type_ "text"
                    , value model.documentTemplateId
                    , class "form-control"
                    , placeholder <| gettext "Document Template ID" appState.locale
                    ]
                    []
                , ActionButton.submit
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
                    (gettext "You can find document templates in %s." appState.locale)
                    [ a [ href (url ++ "/document-templates"), target "_blank" ] [ text LookAndFeelConfig.defaultRegistryName ]
                    ]
                )

        _ ->
            Html.nothing


viewImported : AppState -> String -> Html Msg
viewImported appState packageId =
    div [ class "px-4 py-5 bg-light rounded-3" ]
        [ h1 [] [ faSuccess ]
        , p [ class "lead" ]
            (String.formatHtml
                (gettext "Document template %s has been imported!" appState.locale)
                [ code [] [ text packageId ] ]
            )
        , p [ class "lead" ]
            [ linkTo (Routes.documentTemplatesDetail packageId)
                [ class "btn btn-primary" ]
                [ text (gettext "View detail" appState.locale) ]
            ]
        ]
