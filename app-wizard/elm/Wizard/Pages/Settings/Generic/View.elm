module Wizard.Pages.Settings.Generic.View exposing
    ( ViewProps
    , view
    )

import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Common.Utils.Form as Form
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.GuideLinks exposing (GuideLinks)
import Form exposing (Form)
import Gettext
import Html exposing (Html, div, form)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Wizard.Components.FormActions as FormActions
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Settings.Generic.Model exposing (Model)


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
        [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState props.guideLink) (props.locTitle appState.locale)
        , form [ onSubmit (props.wrapMsg Form.Submit), class "pb-6" ]
            [ FormResult.errorOnlyView model.savingConfig
            , props.formView appState model.form
            , FormActions.viewDynamic formActionsConfig appState
            ]
        ]
