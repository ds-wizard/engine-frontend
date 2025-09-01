module Wizard.Locales.Import.RegistryImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, a, code, div, h1, hr, input, p, text)
import Html.Attributes exposing (class, href, placeholder, target, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Html.Extra as Html
import Shared.Components.FontAwesome exposing (faSuccess)
import String.Format as String
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Wizard.Api.Models.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Locales.Import.RegistryImport.Models exposing (Model)
import Wizard.Locales.Import.RegistryImport.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.pulling of
                Success _ ->
                    viewImported appState model.localeId

                _ ->
                    viewForm appState model
    in
    div [ class "KnowledgeModels__Import__RegistryImport", dataCy "locale_import_registry" ]
        [ content ]


viewForm : AppState -> Model -> Html Msg
viewForm appState model =
    div []
        [ FormResult.errorOnlyView model.pulling
        , div [ class "px-4 py-5 bg-light rounded-3" ]
            [ Html.form [ onSubmit Submit, class "input-group" ]
                [ input
                    [ onInput ChangeLocaleId
                    , type_ "text"
                    , value model.localeId
                    , class "form-control"
                    , placeholder <| gettext "Locale ID" appState.locale
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
                    (gettext "You can find locales in %s." appState.locale)
                    [ a [ href (url ++ "/locales"), target "_blank" ] [ text LookAndFeelConfig.defaultRegistryName ]
                    ]
                )

        _ ->
            Html.nothing


viewImported : AppState -> String -> Html Msg
viewImported appState localeId =
    div [ class "px-4 py-5 bg-light rounded-3" ]
        [ h1 [] [ faSuccess ]
        , p [ class "lead" ]
            (String.formatHtml
                (gettext "Locale %s has been imported!" appState.locale)
                [ code [] [ text localeId ] ]
            )
        , p [ class "lead" ]
            [ linkTo (Routes.localesDetail localeId)
                [ class "btn btn-primary" ]
                [ text (gettext "View detail" appState.locale) ]
            ]
        ]
