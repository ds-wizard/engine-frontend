module Wizard.Settings.Info.View exposing (view)

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
import Wizard.Settings.Common.EditableInfoConfig exposing (EditableInfoConfig)
import Wizard.Settings.Common.InfoConfigForm exposing (InfoConfigForm)
import Wizard.Settings.Info.Models exposing (Model)
import Wizard.Settings.Info.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Info.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Info.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewConfig appState model) model.config


viewConfig : AppState -> Model -> EditableInfoConfig -> Html Msg
viewConfig appState model _ =
    div [ wideDetailClass "" ]
        [ Page.header (l_ "title" appState) []
        , div []
            [ FormResult.view appState model.savingConfig
            , formView appState model.form
            , FormActions.viewActionOnly appState (ActionButton.ButtonConfig (l_ "save" appState) model.savingConfig (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Form CustomFormError InfoConfigForm -> Html Msg
formView appState form =
    let
        formHtml =
            div []
                [ FormGroup.markdownEditor appState form "welcomeInfo" (l_ "form.welcomeInfo" appState)
                , FormExtra.mdAfter (l_ "form.welcomeInfo.desc" appState)
                , FormGroup.markdownEditor appState form "welcomeWarning" (l_ "form.welcomeWarning" appState)
                , FormExtra.mdAfter (l_ "form.welcomeWarning.desc" appState)
                , FormGroup.markdownEditor appState form "loginInfo" (l_ "form.loginInfo" appState)
                , FormExtra.mdAfter (l_ "form.loginInfo.desc" appState)
                ]
    in
    formHtml |> Html.map FormMsg
