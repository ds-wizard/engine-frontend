module Wizard.Settings.Client.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, button, div, h3, label, text)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick)
import Shared.Html exposing (emptyNode, faSet)
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
import Wizard.Settings.Client.Models exposing (Model)
import Wizard.Settings.Client.Msgs exposing (Msg(..))
import Wizard.Settings.Common.ClientConfigForm as ConfigForm exposing (ClientConfigForm)
import Wizard.Settings.Common.EditableClientConfig exposing (EditableClientConfig)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Client.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Client.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewConfig appState model) model.config


viewConfig : AppState -> Model -> EditableClientConfig -> Html Msg
viewConfig appState model _ =
    div [ wideDetailClass "" ]
        [ Page.header (l_ "title" appState) []
        , div []
            [ FormResult.view appState model.savingConfig
            , formView appState model.form
            , FormActions.viewActionOnly appState (ActionButton.ButtonConfig (l_ "save" appState) model.savingConfig (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Form CustomFormError ClientConfigForm -> Html Msg
formView appState form =
    let
        dashboardOptions =
            [ ( ConfigForm.dashboardWelcome, l_ "dashboardOptions.welcome" appState )
            , ( ConfigForm.dashboardDmp, l_ "dashboardOptions.dmp" appState )
            ]

        formHtml =
            div []
                [ h3 [] [ lx_ "section.general" appState ]
                , FormGroup.inputAttrs [ placeholder ClientConfig.defaultAppTitle ] appState form "appTitle" (l_ "form.appTitle" appState)
                , FormExtra.mdAfter (l_ "form.appTitle.desc" appState)
                , FormGroup.inputAttrs [ placeholder ClientConfig.defaultAppTitleShort ] appState form "appTitleShort" (l_ "form.appTitleShort" appState)
                , FormExtra.mdAfter (l_ "form.appTitleShort.desc" appState)
                , FormGroup.select appState dashboardOptions form "dashboard" (l_ "form.dashboardStyle" appState)
                , FormExtra.mdAfter (l_ "form.dashboardStyle.desc" appState)
                , div [ class "custom-menu-links" ]
                    [ label [] [ lx_ "form.customMenuLinks" appState ]
                    , customMenuLinksHeader appState form
                    , FormGroup.list appState (customMenuLinkItemView appState) form "customMenuLinks" ""
                    , FormExtra.mdAfter (l_ "form.customMenuLinks.desc" appState)
                    ]
                , h3 [] [ lx_ "section.privacy" appState ]
                , FormGroup.inputAttrs [ placeholder ClientConfig.defaultPrivacyUrl ] appState form "privacyUrl" (l_ "form.privacyUrl" appState)
                , FormExtra.mdAfter (l_ "form.privacyUrl.desc" appState)
                , h3 [] [ lx_ "section.support" appState ]
                , FormGroup.inputAttrs [ placeholder ClientConfig.defaultSupportEmail ] appState form "supportEmail" (l_ "form.supportEmail" appState)
                , FormExtra.mdAfter (l_ "form.supportEmail.desc" appState)
                , FormGroup.inputAttrs [ placeholder ClientConfig.defaultSupportRepositoryName ] appState form "supportRepositoryName" (l_ "form.supportRepositoryName" appState)
                , FormExtra.mdAfter (l_ "form.supportRepositoryName.desc" appState)
                , FormGroup.inputAttrs [ placeholder ClientConfig.defaultSupportRepositoryUrl ] appState form "supportRepositoryUrl" (l_ "form.supportRepositoryUrl" appState)
                , FormExtra.mdAfter (l_ "form.supportRepositoryUrl.desc" appState)
                ]
    in
    formHtml |> Html.map FormMsg


customMenuLinksHeader : AppState -> Form CustomFormError ClientConfigForm -> Html msg
customMenuLinksHeader appState form =
    let
        isEmpty =
            List.length (Form.getListIndexes "customMenuLinks" form) == 0
    in
    if isEmpty then
        emptyNode

    else
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


customMenuLinkItemView : AppState -> Form CustomFormError ClientConfigForm -> Int -> Html Form.Msg
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
            [ label [ class "checkbox-label" ] [ Input.checkboxInput newWindowField [ class "col-1 form-control" ] ] ]
        , div [ class "col-1 text-right" ]
            [ button [ class "btn btn-outline-warning", onClick (Form.RemoveItem "customMenuLinks" i) ]
                [ faSet "_global.remove" appState ]
            ]
        , iconError
        , titleError
        , urlError
        ]
