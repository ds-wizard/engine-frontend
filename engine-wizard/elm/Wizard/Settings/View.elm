module Wizard.Settings.View exposing (..)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, button, div, h3, label, text)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick)
import Shared.Html exposing (faSet)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Config.ClientConfig as ClientConfig
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Html.Attribute exposing (wideDetailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Settings.Common.ConfigForm as ConfigForm exposing (ConfigForm)
import Wizard.Settings.Common.EditableConfig exposing (EditableConfig)
import Wizard.Settings.Models exposing (Model)
import Wizard.Settings.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewConfig appState model) model.config


viewConfig : AppState -> Model -> EditableConfig -> Html Msg
viewConfig appState model _ =
    div [ wideDetailClass "Configuration" ]
        [ Page.header (l_ "pageTitle" appState) []
        , div []
            [ FormResult.view appState model.savingConfig
            , formView appState model.form
            , FormActions.viewActionOnly appState (ActionButton.ButtonConfig (l_ "save" appState) model.savingConfig (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Form CustomFormError ConfigForm -> Html Msg
formView appState form =
    let
        dashboardOptions =
            [ ( ConfigForm.dashboardWelcome, l_ "dashboardOptions.welcome" appState )
            , ( ConfigForm.dashboardDmp, l_ "dashboardOptions.dmp" appState )
            ]

        formHtml =
            div []
                [ h3 [] [ lx_ "section.features" appState ]
                , FormGroup.toggle form "publicQuestionnaireEnabled" (l_ "form.publicQuestionnaire" appState)
                , FormExtra.mdAfter (l_ "form.publicQuestionnaire.desc" appState)
                , FormGroup.toggle form "questionnaireAccessibilityEnabled" (l_ "form.questionnaireAccessibility" appState)
                , FormExtra.mdAfter (l_ "form.questionnaireAccessibility.desc" appState)
                , FormGroup.toggle form "levelsEnabled" (l_ "form.phases" appState)
                , FormExtra.mdAfter (l_ "form.phases.desc" appState)
                , FormGroup.toggle form "registrationEnabled" (l_ "form.registration" appState)
                , FormExtra.mdAfter (l_ "form.registration.desc" appState)
                , h3 [] [ lx_ "section.client" appState ]
                , FormGroup.inputAttrs [ placeholder ClientConfig.defaultAppTitle ] appState form "appTitle" (l_ "form.appTitle" appState)
                , FormExtra.mdAfter (l_ "form.appTitle.desc" appState)
                , FormGroup.inputAttrs [ placeholder ClientConfig.defaultAppTitleShort ] appState form "appTitleShort" (l_ "form.appTitleShort" appState)
                , FormExtra.mdAfter (l_ "form.appTitleShort.desc" appState)
                , FormGroup.markdownEditor appState form "welcomeInfo" (l_ "form.welcomeInfo" appState)
                , FormExtra.mdAfter (l_ "form.welcomeInfo.desc" appState)
                , FormGroup.markdownEditor appState form "welcomeWarning" (l_ "form.welcomeWarning" appState)
                , FormExtra.mdAfter (l_ "form.welcomeWarning.desc" appState)
                , FormGroup.markdownEditor appState form "loginInfo" (l_ "form.loginInfo" appState)
                , FormExtra.mdAfter (l_ "form.loginInfo.desc" appState)
                , FormGroup.select appState dashboardOptions form "dashboard" (l_ "form.dashboardStyle" appState)
                , FormExtra.mdAfter (l_ "form.dashboardStyle.desc" appState)
                , FormGroup.inputAttrs [ placeholder ClientConfig.defaultPrivacyUrl ] appState form "privacyUrl" (l_ "form.privacyUrl" appState)
                , FormExtra.mdAfter (l_ "form.privacyUrl.desc" appState)
                , FormGroup.inputAttrs [ placeholder ClientConfig.defaultSupportEmail ] appState form "supportEmail" (l_ "form.supportEmail" appState)
                , FormExtra.mdAfter (l_ "form.supportEmail.desc" appState)
                , FormGroup.inputAttrs [ placeholder ClientConfig.defaultSupportRepositoryName ] appState form "supportRepositoryName" (l_ "form.supportRepositoryName" appState)
                , FormExtra.mdAfter (l_ "form.supportRepositoryName.desc" appState)
                , FormGroup.inputAttrs [ placeholder ClientConfig.defaultSupportRepositoryUrl ] appState form "supportRepositoryUrl" (l_ "form.supportRepositoryUrl" appState)
                , FormExtra.mdAfter (l_ "form.supportRepositoryUrl.desc" appState)
                , div [ class "custom-menu-links" ]
                    [ label [] [ lx_ "form.customMenuLinks" appState ]
                    , customMenuLinksHeader appState
                    , FormGroup.list appState (customMenuLinkItemView appState) form "customMenuLinks" ""
                    , FormExtra.mdAfter (l_ "form.customMenuLinks.desc" appState)
                    ]
                ]
    in
    formHtml |> Html.map FormMsg


customMenuLinksHeader : AppState -> Html msg
customMenuLinksHeader appState =
    div [ class "row custom-menu-links-header" ]
        [ div [ class "col-2" ]
            [ lx_ "form.customMenuLinks.icon" appState ]
        , div [ class "col-3" ]
            [ lx_ "form.customMenuLinks.title" appState ]
        , div [ class "col-4" ]
            [ lx_ "form.customMenuLinks.url" appState ]
        , div [ class "col-3" ]
            [ lx_ "form.customMenuLinks.newWindow" appState ]
        ]


customMenuLinkItemView : AppState -> Form CustomFormError ConfigForm -> Int -> Html Form.Msg
customMenuLinkItemView appState form i =
    let
        iconField =
            Form.getFieldAsString ("customMenuLinks." ++ String.fromInt i ++ ".icon") form

        titleField =
            Form.getFieldAsString ("customMenuLinks." ++ String.fromInt i ++ ".title") form

        urlField =
            Form.getFieldAsString ("customMenuLinks." ++ String.fromInt i ++ ".url") form

        newWindowField =
            Form.getFieldAsBool ("customMenuLinks." ++ String.fromInt i ++ ".newWindow") form

        ( iconError, iconErrorClass ) =
            FormGroup.getErrors appState iconField "Icon"

        ( titleError, titleErrorClass ) =
            FormGroup.getErrors appState titleField "Title"

        ( urlError, urlErrorClass ) =
            FormGroup.getErrors appState urlField "URL"
    in
    div [ class "row" ]
        [ div [ class "col-2" ]
            [ Input.textInput iconField [ class <| "form-control " ++ iconErrorClass ] ]
        , div [ class "col-3" ]
            [ Input.textInput titleField [ class <| "form-control " ++ titleErrorClass ] ]
        , div [ class "col-4" ]
            [ Input.textInput urlField [ class <| " form-control " ++ urlErrorClass ] ]
        , div [ class "col-2" ]
            [ Input.checkboxInput newWindowField [ class "col-1 form-control" ] ]
        , div [ class "col-1 text-right" ]
            [ button [ class "btn btn-outline-warning", onClick (Form.RemoveItem "customMenuLinks" i) ]
                [ faSet "_global.remove" appState ]
            ]
        , iconError
        , titleError
        , urlError
        ]
