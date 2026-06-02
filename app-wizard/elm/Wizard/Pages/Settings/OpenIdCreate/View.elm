module Wizard.Pages.Settings.OpenIdCreate.View exposing (view)

import Common.Components.FontAwesome exposing (faDelete, fas)
import Common.Components.Form as Form
import Common.Components.FormExtra as FromExtra
import Common.Components.FormGroup as FormGroup
import Common.Components.Page as Page
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, div, hr, label, li, strong, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Wizard.Api.Models.OpenIdClientDetail exposing (OpenIdClientDetail)
import Wizard.Components.ExternalLoginButton as ExternalLoginButton
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Settings.Common.AuthButtonFormGroup as AuthButtonFormGroup
import Wizard.Pages.Settings.Common.Forms.OpenIdClientForm as OpenIdClientForm exposing (OpenIdClientForm)
import Wizard.Pages.Settings.OpenIdCreate.Models exposing (Model)
import Wizard.Pages.Settings.OpenIdCreate.Msgs exposing (Msg(..))
import Wizard.Utils.WizardGuideLinks as GuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState model) model.openIdPrefabs


viewContent : AppState -> Model -> List OpenIdClientDetail -> Html Msg
viewContent appState model openIdPrefabs =
    let
        prefabsView =
            if not (List.isEmpty openIdPrefabs) && OpenIdClientForm.isEmpty model.form then
                let
                    viewPrefabButton openID =
                        ExternalLoginButton.render [ onClick (FillOpenIDServiceConfig openID) ]
                            openID.name
                            openID.style.icon
                            openID.style.color
                            openID.style.background
                in
                div [ class "prefab-selection prefab-selection-openid" ]
                    [ strong [] [ text (gettext "Quick setup" appState.locale) ]
                    , div [] (List.map viewPrefabButton <| List.sortBy .name openIdPrefabs)
                    ]

            else
                Html.nothing

        isMicrosoftMode =
            OpenIdClientForm.isMicrosoftMode model.form

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
                [ prefabsView
                , mapFormMsg <| FormGroup.input appState.locale model.form "name" (gettext "Name" appState.locale)
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
    in
    div [ class "pb-6" ]
        [ Page.headerWithGuideLink
            (AppState.toGuideLinkConfig appState GuideLinks.settingsOpenId)
            (gettext "Create OpenID Config" appState.locale)
        , Form.viewSimple
            { formMsg = FormMsg
            , formResult = model.savingForm
            , formView = formContent
            , submitLabel = gettext "Save" appState.locale
            , cancelMsg = Just Cancel
            , locale = appState.locale
            , isMac = appState.navigator.isMac
            }
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
            [ Input.textInput nameField [ class <| "form-control " ++ nameErrorClass, dataCy "settings_authentication_service_parameter-name" ]
            , nameError
            ]
        , div [ class "col-6" ]
            [ Input.textInput valueField [ class <| "form-control " ++ valueErrorClass, dataCy "settings_authentication_service_parameter-value" ]
            , valueError
            ]
        , div [ class "col-1 text-end" ]
            [ a [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix i) ] [ faDelete ] ]
        ]


mapFormMsg : Html Form.Msg -> Html Msg
mapFormMsg =
    Html.map FormMsg
