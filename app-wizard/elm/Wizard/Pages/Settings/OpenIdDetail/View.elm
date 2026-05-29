module Wizard.Pages.Settings.OpenIdDetail.View exposing (view)

import Common.Components.FontAwesome exposing (faDelete, fas)
import Common.Components.Form as Form
import Common.Components.FormExtra as FromExtra
import Common.Components.FormGroup as FormGroup
import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Common.Utils.Form as Form
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, div, hr, label, li, strong, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Uuid
import Wizard.Api.Models.OpenIdClientDetail exposing (OpenIdClientDetail)
import Wizard.Components.CopyableCodeBlock as CopyableCodeBlock
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Settings.Common.AuthButtonFormGroup as AuthButtonFormGroup
import Wizard.Pages.Settings.Common.Forms.OpenIdClientForm as OpenIdClientForm exposing (OpenIdClientForm)
import Wizard.Pages.Settings.OpenIdDetail.Models exposing (Model)
import Wizard.Pages.Settings.OpenIdDetail.Msgs exposing (Msg(..))
import Wizard.Utils.WizardGuideLinks as GuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewOpenId appState model) model.openIdClient


viewOpenId : AppState -> Model -> OpenIdClientDetail -> Html Msg
viewOpenId appState model openId =
    let
        isMicrosoftMode =
            OpenIdClientForm.isMicrosoftMode model.form

        callbackUrlLabel =
            if isMicrosoftMode then
                gettext "Redirect URI" appState.locale

            else
                gettext "Callback URL" appState.locale

        callbackUrl =
            appState.clientUrl ++ "/open-id/" ++ Uuid.toString openId.uuid ++ "/callback"

        logoutUrlLabel =
            if isMicrosoftMode then
                gettext "Front-channel logout URL" appState.locale

            else
                gettext "Logout URL" appState.locale

        logoutUrl =
            appState.apiUrl ++ "/open-id-clients/" ++ Uuid.toString openId.uuid ++ "/logout"

        clientIdLabel =
            if isMicrosoftMode then
                gettext "Application (client) ID" appState.locale

            else
                gettext "Client ID" appState.locale

        clientSecretLabel =
            if isMicrosoftMode then
                gettext "Client Secret Value" appState.locale

            else
                gettext "Client Secret" appState.locale

        advancedConfigurationIcon =
            if model.advancedConfigExpanded then
                "fa-chevron-down"

            else
                "fa-chevron-right"

        formContent =
            div []
                [ FormResult.errorOnlyView model.savingOpenId
                , mapFormMsg <| FormGroup.input appState.locale model.form "name" (gettext "Name" appState.locale)
                , div [ class "card bg-light mb-3" ]
                    [ div [ class "card-body" ]
                        [ FormGroup.plainGroup
                            (Html.map CallbackUrlCodeBlockMsg <| CopyableCodeBlock.view appState model.callbackUrlCodeBlockState callbackUrl)
                            callbackUrlLabel
                        , FormGroup.plainGroup
                            (Html.map LogoutUrlCodeBlockMsg <| CopyableCodeBlock.view appState model.callbackUrlCodeBlockState logoutUrl)
                            logoutUrlLabel
                        ]
                    ]
                , ul [ class "nav nav-tabs mb-3 mt-4" ]
                    [ li [ class "nav-item" ]
                        [ a
                            [ class "nav-link"
                            , classList [ ( "active", isMicrosoftMode ) ]
                            , onClick (FormMsg OpenIdClientForm.setFormModeMicrosoftMsg)
                            ]
                            [ text (gettext "Microsoft" appState.locale) ]
                        ]
                    , li [ class "nav-item" ]
                        [ a
                            [ class "nav-link"
                            , classList [ ( "active", not isMicrosoftMode ) ]
                            , onClick (FormMsg OpenIdClientForm.setFormModeCustomMsg)
                            ]
                            [ text (gettext "Custom" appState.locale) ]
                        ]
                    ]
                , div [ class "row mb-3" ]
                    [ div [ class "col" ] [ mapFormMsg <| FormGroup.input appState.locale model.form "clientId" clientIdLabel ]
                    , div [ class "col" ] [ mapFormMsg <| FormGroup.secret appState.locale model.form "clientSecret" clientSecretLabel ]
                    ]
                , Html.viewIf isMicrosoftMode <|
                    div [ class "row mb-3" ]
                        [ div [ class "col-6" ]
                            [ mapFormMsg <| FormGroup.input appState.locale model.form "directoryId" (gettext "Directory (tenant) ID" appState.locale) ]
                        ]
                , Html.viewIf (not isMicrosoftMode) <| mapFormMsg <| FormGroup.input appState.locale model.form "url" (gettext "URL" appState.locale)
                , Html.viewIf (not isMicrosoftMode) <|
                    div [ class "input-table", dataCy "settings_authentication_service_parameters" ]
                        [ label [] [ text (gettext "Parameters" appState.locale) ]
                        , serviceParametersHeader appState "parameters" model.form
                        , mapFormMsg <| FormGroup.list appState.locale (serviceParameterView appState "parameters") model.form "parameters" "" (gettext "Add parameter" appState.locale)
                        ]
                , div [ class "row mt-4 mb-1" ]
                    [ div [ class "col" ]
                        [ a [ class "fw-bold", onClick (SetAdvancedConfigExpanded (not model.advancedConfigExpanded)) ]
                            [ fas (advancedConfigurationIcon ++ " fa-fw me-1")
                            , text (gettext "Advanced configuration" appState.locale)
                            ]
                        ]
                    ]
                , Html.viewIf model.advancedConfigExpanded <|
                    div [ class "border-start border-5 ps-4 pt-3" ]
                        [ div [ class "row mb-3" ]
                            [ div [ class "col" ]
                                [ mapFormMsg <| FormGroup.toggle model.form "registrationEnabled" (gettext "Registration enabled" appState.locale)
                                , FromExtra.mdAfter (gettext "Allow users to register using this OpenID provider." appState.locale)
                                ]
                            ]
                        , div [ class "row mb-2" ]
                            [ div [ class "col" ]
                                [ strong [] [ text "Scopes" ]
                                ]
                            ]
                        , div [ class "row" ]
                            [ div [ class "col" ]
                                [ mapFormMsg <| FormGroup.toggle model.form "scopeEmail" (gettext "Email" appState.locale)
                                , FromExtra.mdAfter (gettext "Allows the application to access your email address and whether it has been verified." appState.locale)
                                , mapFormMsg <| FormGroup.toggle model.form "scopeProfile" (gettext "Profile" appState.locale)
                                , FromExtra.mdAfter (gettext "Allows the application to access the basic profile information (name, username, profile picture, and other personal details)." appState.locale)
                                ]
                            ]
                        ]
                , hr [] []
                , mapFormMsg <| AuthButtonFormGroup.formGroup appState model.form
                ]

        form =
            Form.initDynamic appState (FormMsg Form.Submit) model.savingOpenId
                |> Form.setFormView formContent
                |> Form.setFormChanged (model.formRemoved || Form.containsChanges model.form)
                |> Form.setWide
                |> Form.viewDynamic
    in
    div []
        [ Page.headerWithGuideLink
            (AppState.toGuideLinkConfig appState GuideLinks.settingsOpenId)
            (gettext "Edit OpenID Config" appState.locale)
        , form
        ]


serviceParametersHeader : AppState -> String -> Form FormError OpenIdClientForm -> Html msg
serviceParametersHeader appState field form =
    let
        isEmpty =
            List.isEmpty (Form.getListIndexes field form)
    in
    if isEmpty then
        Html.nothing

    else
        div [ class "row input-table-header" ]
            [ div [ class "col-5" ] [ text (gettext "Name" appState.locale) ]
            , div [ class "col-6" ] [ text (gettext "Value" appState.locale) ]
            ]


serviceParameterView : AppState -> String -> Form FormError OpenIdClientForm -> Int -> Html Form.Msg
serviceParameterView appState prefix form i =
    let
        name =
            prefix ++ "." ++ String.fromInt i ++ ".name"

        value =
            prefix ++ "." ++ String.fromInt i ++ ".value"

        nameField =
            Form.getFieldAsString name form

        valueField =
            Form.getFieldAsString value form

        ( nameError, nameErrorClass ) =
            FormGroup.getErrors appState.locale nameField (gettext "Name" appState.locale)

        ( valueError, valueErrorClass ) =
            FormGroup.getErrors appState.locale valueField (gettext "Value" appState.locale)
    in
    div [ class "row mb-2" ]
        [ div [ class "col-5" ]
            [ Input.textInput nameField [ class <| "form-control " ++ nameErrorClass ]
            , nameError
            ]
        , div [ class "col-6" ]
            [ Input.textInput valueField [ class <| "form-control " ++ valueErrorClass ]
            , valueError
            ]
        , div [ class "col-1 text-end" ]
            [ a [ class "btn btn-link link-danger", onClick (Form.RemoveItem prefix i) ] [ faDelete ] ]
        ]


mapFormMsg : Html Form.Msg -> Html Msg
mapFormMsg =
    Html.map FormMsg
