module Wizard.Settings.Generic.View exposing
    ( ViewProps
    , view
    )

import Form exposing (Form)
import Gettext
import Html exposing (Html, div, form)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (wideDetailClass)
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Settings.Generic.Model exposing (Model)


type alias ViewProps form msg =
    { locTitle : Gettext.Locale -> String
    , locSave : Gettext.Locale -> String
    , formView : AppState -> Form FormError form -> Html msg
    , wrapMsg : Form.Msg -> msg
    }


view : ViewProps form msg -> AppState -> Model form -> Html msg
view props appState model =
    Page.actionResultView appState (viewForm props appState model) model.config


viewForm : ViewProps form msg -> AppState -> Model form -> config -> Html msg
viewForm props appState model _ =
    let
        formActionsConfig =
            { text = Nothing
            , actionResult = model.savingConfig
            , form = model.form
            , wide = True
            }
    in
    div [ wideDetailClass "" ]
        [ Page.header (props.locTitle appState.locale) []
        , form [ onSubmit (props.wrapMsg Form.Submit), class "pb-6" ]
            [ FormResult.errorOnlyView appState model.savingConfig
            , props.formView appState model.form
            , FormActions.viewDynamic formActionsConfig appState
            ]
        ]
