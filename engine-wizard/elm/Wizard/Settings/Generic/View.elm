module Wizard.Settings.Generic.View exposing
    ( ViewProps
    , view
    )

import Form exposing (Form)
import Gettext
import Html exposing (Html, div, form)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Shared.Form as Form
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.GuideLinks exposing (GuideLinks)
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Settings.Generic.Model exposing (Model)


type alias ViewProps form msg =
    { locTitle : Gettext.Locale -> String
    , locSave : Gettext.Locale -> String
    , formView : AppState -> Form FormError form -> Html msg
    , guideLink : GuideLinks -> String
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
            , formChanged = model.formRemoved || Form.containsChanges model.form
            , wide = True
            }
    in
    div []
        [ Page.headerWithGuideLink appState (props.locTitle appState.locale) props.guideLink
        , form [ onSubmit (props.wrapMsg Form.Submit), class "pb-6" ]
            [ FormResult.errorOnlyView appState model.savingConfig
            , props.formView appState model.form
            , FormActions.viewDynamic formActionsConfig appState
            ]
        ]
