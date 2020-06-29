module Wizard.Settings.Registry.View exposing (view)

import ActionResult
import Form exposing (Form)
import Html exposing (Html, button, div, h5)
import Html.Attributes exposing (class, disabled, readonly)
import Html.Events exposing (onClick)
import Shared.Data.EditableConfig.EditableRegistryConfig exposing (EditableRegistryConfig)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (wideDetailClass)
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


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Registry.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Registry.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewForm appState model) model.genericModel.config


viewForm : AppState -> Model -> config -> Html Msg
viewForm appState model _ =
    div [ wideDetailClass "" ]
        [ Page.header (l_ "title" appState) []
        , div []
            [ FormResult.errorOnlyView appState model.genericModel.savingConfig
            , formView appState model.genericModel.form
            , div [ class "mt-5" ]
                [ ActionButton.button appState
                    (ActionButton.ButtonConfig (l_ "save" appState)
                        model.genericModel.savingConfig
                        (GenericMsg <| GenericMsgs.FormMsg Form.Submit)
                        False
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

        hasToken =
            (Form.getFieldAsString "token" form).value
                |> Maybe.map (not << String.isEmpty)
                |> Maybe.withDefault False

        signupButton =
            if hasToken then
                emptyNode

            else
                button [ class "btn btn-outline-primary", onClick <| ToggleRegistrySignup True ]
                    [ lx_ "form.signUp" appState ]

        tokenInput =
            if enabled then
                div [ class "nested-group" ]
                    [ formWrap <| FormGroup.textarea appState form "token" (l_ "form.token" appState)
                    , FormExtra.mdAfter (l_ "form.token.desc" appState)
                    , signupButton
                    ]

            else
                emptyNode
    in
    div []
        [ formWrap <| FormGroup.toggle form "enabled" (l_ "form.enabled" appState)
        , FormExtra.mdAfter (l_ "form.enabled.desc" appState)
        , tokenInput
        ]


registrySignupModal : AppState -> Model -> Html Msg
registrySignupModal appState model =
    let
        submitButton =
            if ActionResult.isSuccess model.registrySigningUp then
                button [ class "btn btn-primary", onClick <| ToggleRegistrySignup False ]
                    [ lx_ "registryModal.button.done" appState ]

            else if List.length (Form.getErrors model.registrySignupForm) > 0 then
                button [ class "btn btn-primary", disabled True ]
                    [ lx_ "registryModal.button.signUp" appState ]

            else
                ActionButton.button appState
                    { label = l_ "registryModal.button.signUp" appState
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
                [ lx_ "registryModal.button.cancel" appState ]

        form =
            model.registrySignupForm

        formBody =
            Html.map FormMsg <|
                div []
                    [ FormResult.errorOnlyView appState model.registrySigningUp
                    , FormGroup.inputAttrs [ readonly True ] appState form "organizationId" (l_ "registryModal.form.organizationId" appState)
                    , FormGroup.inputAttrs [ readonly True ] appState form "name" (l_ "registryModal.form.organizationName" appState)
                    , FormGroup.textareaAttrs [ readonly True ] appState form "description" (l_ "registryModal.form.organizationDescription" appState)
                    , FormGroup.input appState form "email" (l_ "registryModal.form.email" appState)
                    , FormExtra.textAfter (l_ "registryModal.form.email.desc" appState)
                    ]

        resultBody resultText =
            Flash.success appState resultText

        body =
            if ActionResult.isSuccess model.registrySigningUp then
                ActionResultBlock.view appState resultBody model.registrySigningUp

            else
                formBody

        content =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ lx_ "registryModal.title" appState ]
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
            }
    in
    Modal.simple modalConfig
