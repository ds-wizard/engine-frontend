module Wizard.Pages.Settings.Registry.View exposing (view)

import ActionResult
import Common.Components.ActionButton as ActionButton
import Common.Components.ActionResultBlock as ActionResultBlock
import Common.Components.Flash as Flash
import Common.Components.Form as Form
import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Components.FormResult as FormResult
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Common.Utils.Form as Form
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, button, div, h5, text)
import Html.Attributes exposing (class, disabled, readonly)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Wizard.Api.Models.EditableConfig.EditableRegistryConfig exposing (EditableRegistryConfig)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Settings.Generic.Msgs as GenericMsgs
import Wizard.Pages.Settings.Registry.Models exposing (Model)
import Wizard.Pages.Settings.Registry.Msgs exposing (Msg(..))
import Wizard.Utils.HtmlAttributesUtils exposing (wideDetailClass)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewForm appState model) model.genericModel.config


viewForm : AppState -> Model -> config -> Html Msg
viewForm appState model _ =
    let
        form =
            Form.initDynamic appState (GenericMsg (GenericMsgs.FormMsg Form.Submit)) model.genericModel.savingConfig
                |> Form.setFormView (formView appState model.genericModel.form)
                |> Form.setFormChanged (model.genericModel.formRemoved || Form.containsChanges model.genericModel.form)
                |> Form.setWide
                |> Form.viewDynamic
    in
    div [ wideDetailClass "" ]
        [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.settingsRegistry) (gettext "DSW Registry" appState.locale)
        , form
        , registrySignupModal appState model
        ]


formView : AppState -> Form FormError EditableRegistryConfig -> Html Msg
formView appState form =
    let
        formWrap =
            Html.map (GenericMsg << GenericMsgs.FormMsg)

        enabled =
            Maybe.withDefault False (Form.getFieldAsBool "enabled" form).value

        tokenInput =
            if enabled then
                let
                    hasToken =
                        (Form.getFieldAsString "token" form).value
                            |> Maybe.map (not << String.isEmpty)
                            |> Maybe.withDefault False

                    signupButton =
                        if hasToken then
                            Html.nothing

                        else
                            button [ class "btn btn-outline-primary", onClick <| ToggleRegistrySignup True ]
                                [ text (gettext "Sign Up" appState.locale) ]
                in
                div [ class "nested-group" ]
                    [ formWrap <| FormGroup.secret appState.locale form "token" (gettext "Token" appState.locale)
                    , FormExtra.mdAfter (gettext "Fill in your DSW Registry token. If you don't have one, you need sign up first." appState.locale)
                    , signupButton
                    ]

            else
                Html.nothing
    in
    div []
        [ formWrap <| FormGroup.toggle form "enabled" (gettext "Enabled" appState.locale)
        , FormExtra.mdAfter (gettext "If enabled, you can import knowledge models and document templates directly from [DSW Registry](https://registry.ds-wizard.org)." appState.locale)
        , tokenInput
        ]


registrySignupModal : AppState -> Model -> Html Msg
registrySignupModal appState model =
    let
        submitButton =
            if ActionResult.isSuccess model.registrySigningUp then
                button [ class "btn btn-primary", onClick <| ToggleRegistrySignup False ]
                    [ text (gettext "Done" appState.locale) ]

            else if List.length (Form.getErrors model.registrySignupForm) > 0 then
                button [ class "btn btn-primary", disabled True ]
                    [ text (gettext "Sign Up" appState.locale) ]

            else
                ActionButton.button
                    { label = gettext "Sign Up" appState.locale
                    , result = model.registrySigningUp
                    , msg = FormMsg Form.Submit
                    , dangerous = False
                    }

        cancelButton =
            button
                [ onClick <| ToggleRegistrySignup False
                , class "btn btn-secondary"
                , disabled <| ActionResult.isLoading model.registrySigningUp
                ]
                [ text (gettext "Cancel" appState.locale) ]

        resultBody resultText =
            Flash.success resultText

        body =
            if ActionResult.isSuccess model.registrySigningUp then
                ActionResultBlock.view
                    { viewContent = resultBody
                    , actionResult = model.registrySigningUp
                    , locale = appState.locale
                    }

            else
                let
                    form =
                        model.registrySignupForm
                in
                Html.map FormMsg <|
                    div []
                        [ FormResult.errorOnlyView model.registrySigningUp
                        , FormGroup.inputAttrs [ readonly True ] appState.locale form "organizationId" (gettext "Organization ID" appState.locale)
                        , FormGroup.inputAttrs [ readonly True ] appState.locale form "name" (gettext "Organization Name" appState.locale)
                        , FormGroup.textareaAttrs [ readonly True ] appState.locale form "description" (gettext "Organization Description" appState.locale)
                        , FormGroup.input appState.locale form "email" (gettext "Email" appState.locale)
                        , FormExtra.textAfter (gettext "The email will be used for a confirmation link and an eventual token recovery." appState.locale)
                        ]

        content =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text (gettext "Registry Sign Up" appState.locale) ]
                ]
            , div [ class "modal-body" ]
                [ body
                ]
            , div [ class "modal-footer" ]
                [ submitButton
                , cancelButton
                ]
            ]

        ( enterMsg, escMsg ) =
            if ActionResult.isLoading model.registrySigningUp then
                ( Nothing, Nothing )

            else
                ( Just (FormMsg Form.Submit), Just (ToggleRegistrySignup False) )

        modalConfig =
            { modalContent = content
            , visible = model.registrySignupOpen
            , enterMsg = enterMsg
            , escMsg = escMsg
            , dataCy = "registry-signup"
            }
    in
    Modal.simple modalConfig
