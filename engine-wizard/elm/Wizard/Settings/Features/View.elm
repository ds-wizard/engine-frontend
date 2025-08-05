module Wizard.Settings.Features.View exposing (view)

import Form
import Gettext exposing (gettext)
import Html exposing (Html, div, form)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Shared.Form as Form
import Shared.Form.FormError exposing (FormError)
import Wizard.Api.Models.EditableConfig.EditableFeaturesConfig exposing (EditableFeaturesConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Settings.Features.Models exposing (Model)
import Wizard.Settings.Generic.Msgs as GenericMsgs exposing (Msg)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewForm appState model) model.config


viewForm : AppState -> Model -> config -> Html Msg
viewForm appState model _ =
    let
        formActionsConfig =
            { text = Nothing
            , actionResult = model.savingConfig
            , formChanged = model.formRemoved || Form.containsChanges model.form
            , wide = True
            }

        headerTitle =
            gettext "Features" appState.locale
    in
    div [ class "Features" ]
        [ Page.header headerTitle []
        , form [ onSubmit (GenericMsgs.FormMsg Form.Submit) ]
            [ FormResult.errorOnlyView model.savingConfig
            , formView appState model.form
            , FormActions.viewDynamic formActionsConfig appState
            ]
        ]


formView : AppState -> Form.Form FormError EditableFeaturesConfig -> Html Msg
formView appState form =
    let
        formWrap =
            Html.map GenericMsgs.FormMsg
    in
    div [ class "FeaturesForm" ]
        [ formWrap <| FormGroup.toggle form "toursEnabled" (gettext "Tours" appState.locale)
        , FormExtra.mdAfter (gettext "If enabled, Tours help users navigate the application when opening specific screens for the first time." appState.locale)
        ]
