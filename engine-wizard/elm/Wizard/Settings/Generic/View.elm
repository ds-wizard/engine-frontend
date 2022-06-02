module Wizard.Settings.Generic.View exposing
    ( ViewProps
    , view
    )

import Form exposing (Form)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy, wideDetailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Settings.Generic.Model exposing (Model)


type alias ViewProps form msg =
    { locTitle : AppState -> String
    , locSave : AppState -> String
    , formView : AppState -> Form FormError form -> Html msg
    , wrapMsg : Form.Msg -> msg
    }


view : ViewProps form msg -> AppState -> Model form -> Html msg
view props appState model =
    Page.actionResultView appState (viewForm props appState model) model.config


viewForm : ViewProps form msg -> AppState -> Model form -> config -> Html msg
viewForm props appState model _ =
    div [ wideDetailClass "" ]
        [ Page.header (props.locTitle appState) []
        , div []
            [ FormResult.errorOnlyView appState model.savingConfig
            , props.formView appState model.form
            , div [ class "mt-5" ]
                [ ActionButton.buttonWithAttrs appState
                    (ActionButton.ButtonWithAttrsConfig (props.locSave appState)
                        model.savingConfig
                        (props.wrapMsg Form.Submit)
                        False
                        [ dataCy "form_submit" ]
                    )
                ]
            ]
        ]
