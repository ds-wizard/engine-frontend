module Wizard.Settings.Features.View exposing (view)

import Form exposing (Form)
import Html exposing (Html, div)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Html.Attribute exposing (wideDetailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Settings.Common.EditableFeaturesConfig exposing (EditableFeaturesConfig)
import Wizard.Settings.Common.FeaturesConfigForm exposing (FeaturesConfigForm)
import Wizard.Settings.Features.Models exposing (Model)
import Wizard.Settings.Features.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Features.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Features.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewConfig appState model) model.config


viewConfig : AppState -> Model -> EditableFeaturesConfig -> Html Msg
viewConfig appState model _ =
    div [ wideDetailClass "" ]
        [ Page.header (l_ "title" appState) []
        , div []
            [ FormResult.view appState model.savingConfig
            , formView appState model.form
            , FormActions.viewActionOnly appState (ActionButton.ButtonConfig (l_ "save" appState) model.savingConfig (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Form CustomFormError FeaturesConfigForm -> Html Msg
formView appState form =
    let
        formHtml =
            div []
                [ FormGroup.toggle form "publicQuestionnaireEnabled" (l_ "form.publicQuestionnaire" appState)
                , FormExtra.mdAfter (l_ "form.publicQuestionnaire.desc" appState)
                , FormGroup.toggle form "questionnaireAccessibilityEnabled" (l_ "form.questionnaireAccessibility" appState)
                , FormExtra.mdAfter (l_ "form.questionnaireAccessibility.desc" appState)
                , FormGroup.toggle form "levelsEnabled" (l_ "form.phases" appState)
                , FormExtra.mdAfter (l_ "form.phases.desc" appState)
                , FormGroup.toggle form "registrationEnabled" (l_ "form.registration" appState)
                , FormExtra.mdAfter (l_ "form.registration.desc" appState)
                ]
    in
    formHtml |> Html.map FormMsg
