module Wizard.Pages.Public.Common.View exposing
    ( FormConfig
    , publicForm
    )

import ActionResult exposing (ActionResult)
import Common.Components.ActionButton as ActionButton
import Common.Components.FormResult as FormResult
import Common.Utils.ShortcutUtils as Shortcut
import Html exposing (Html, div, form, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Shortcut
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Routes as Routes


type alias FormConfig msg =
    { title : String
    , submitMsg : msg
    , actionResult : ActionResult String
    , submitLabel : String
    , formContent : Html msg
    , link : Maybe ( Routes.Route, String )
    }


publicForm : AppState -> FormConfig msg -> Html msg
publicForm appState formConfig =
    let
        shortcuts =
            if ActionResult.isLoading formConfig.actionResult then
                []

            else
                [ Shortcut.submitShortcut appState.navigator.isMac formConfig.submitMsg ]

        link =
            case formConfig.link of
                Just ( route, linkText ) ->
                    linkTo route [] [ text linkText ]

                _ ->
                    span [] []
    in
    Shortcut.shortcutElement shortcuts
        [ class "d-block align-self-center col-xs-10 col-sm-8 col-md-6 col-lg-4" ]
        [ form [ onSubmit formConfig.submitMsg, class "card bg-light" ]
            [ div [ class "card-header" ] [ text formConfig.title ]
            , div [ class "card-body" ]
                [ FormResult.view formConfig.actionResult
                , formConfig.formContent
                , div [ class "form-group d-flex align-items-baseline justify-content-between" ]
                    [ link
                    , ActionButton.submit <| ActionButton.SubmitConfig formConfig.submitLabel formConfig.actionResult
                    ]
                ]
            ]
        ]
