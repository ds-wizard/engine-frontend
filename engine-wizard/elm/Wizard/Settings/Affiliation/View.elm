module Wizard.Settings.Affiliation.View exposing (view)

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
import Wizard.Settings.Affiliation.Models exposing (Model)
import Wizard.Settings.Affiliation.Msgs exposing (Msg(..))
import Wizard.Settings.Common.AffiliationConfigForm exposing (AffiliationConfigForm)
import Wizard.Settings.Common.EditableAffiliationConfig exposing (EditableAffiliationConfig)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Affiliation.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Affiliation.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewConfig appState model) model.config


viewConfig : AppState -> Model -> EditableAffiliationConfig -> Html Msg
viewConfig appState model _ =
    div [ wideDetailClass "" ]
        [ Page.header (l_ "title" appState) []
        , div []
            [ FormResult.view appState model.savingConfig
            , formView appState model.form
            , FormActions.viewActionOnly appState (ActionButton.ButtonConfig (l_ "save" appState) model.savingConfig (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Form CustomFormError AffiliationConfigForm -> Html Msg
formView appState form =
    let
        formHtml =
            div []
                [ FormGroup.resizableTextarea appState form "affiliations" (l_ "form.affiliations" appState)
                , FormExtra.mdAfter (l_ "form.affiliations.desc" appState)
                ]
    in
    formHtml |> Html.map FormMsg
