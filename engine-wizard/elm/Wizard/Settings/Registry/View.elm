module Wizard.Settings.Registry.View exposing (view)

import ActionResult
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, button, div, h5, text)
import Html.Attributes exposing (class, disabled, readonly)
import Html.Events exposing (onClick)
import Shared.Data.EditableConfig.EditableRegistryConfig exposing (EditableRegistryConfig)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy, wideDetailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.ActionResultBlock as ActionResultBlock
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Settings.Generic.Msgs as GenericMsgs
import Wizard.Settings.Registry.Models exposing (Model)
import Wizard.Settings.Registry.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewForm appState model) model.genericModel.config


viewForm : AppState -> Model -> config -> Html Msg
viewForm appState model _ =
    div [ wideDetailClass "" ]
        [ Page.header (gettext "DSW Registry" appState.locale) []
        , div []
            [ FormResult.errorOnlyView appState model.genericModel.savingConfig
            , formView appState model.genericModel.form
            , div [ class "mt-5" ]
                [ ActionButton.buttonWithAttrs appState
                    (ActionButton.ButtonWithAttrsConfig (gettext "Save" appState.locale)
                        model.genericModel.savingConfig
                        (GenericMsg <| GenericMsgs.FormMsg Form.Submit)
                        False
                        [ dataCy "form_submit" ]
                    )
                ]
            ]
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
                            emptyNode

                        else
                            button [ class "btn btn-outline-primary", onClick <| ToggleRegistrySignup True ]
                                [ text (gettext "Sign Up" appState.locale) ]
                in
                div [ class "nested-group" ]
                    [ formWrap <| FormGroup.textarea appState form "token" (gettext "Token" appState.locale)
                    , FormExtra.mdAfter (gettext "Fill in your DSW Registry token. If you don't have one, you need sign up first." appState.locale)
                    , signupButton
                    ]

            else
                emptyNode
    in
    div []
        [ formWrap <| FormGroup.toggle form "enabled" (gettext "Enabled" appState.locale)
        , FormExtra.mdAfter (gettext "If enabled, you can import Knowledge Models and Document Templates directly from [DSW Registry](https://registry.ds-wizard.org)." appState.locale)
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
                ActionButton.button appState
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
            Flash.success appState resultText

        body =
            if ActionResult.isSuccess model.registrySigningUp then
                ActionResultBlock.view appState resultBody model.registrySigningUp

            else
                let
                    form =
                        model.registrySignupForm
                in
                Html.map FormMsg <|
                    div []
                        [ FormResult.errorOnlyView appState model.registrySigningUp
                        , FormGroup.inputAttrs [ readonly True ] appState form "organizationId" (gettext "Organization ID" appState.locale)
                        , FormGroup.inputAttrs [ readonly True ] appState form "name" (gettext "Organization Name" appState.locale)
                        , FormGroup.textareaAttrs [ readonly True ] appState form "description" (gettext "Organization Description" appState.locale)
                        , FormGroup.input appState form "email" (gettext "Email" appState.locale)
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

        modalConfig =
            { modalContent = content
            , visible = model.registrySignupOpen
            , dataCy = "registry-signup"
            }
    in
    Modal.simple modalConfig
