module Wizard.Settings.Authentication.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, div, h3, hr, label, strong, text)
import Html.Attributes exposing (attribute, class, placeholder)
import Html.Events exposing (onClick)
import Shared.Auth.Role as Role
import Shared.Data.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import String.Extra as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ExternalLoginButton as ExternalLoginButton
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Page as Page
import Wizard.Settings.Authentication.Models exposing (Model)
import Wizard.Settings.Authentication.Msgs exposing (Msg(..))
import Wizard.Settings.Common.Forms.AuthenticationConfigForm as AuthenticationConfigForm exposing (AuthenticationConfigForm)
import Wizard.Settings.Generic.Msgs as GenericMsgs
import Wizard.Settings.Generic.View as GenericView


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState model) model.openIDPrefabs


viewContent : AppState -> Model -> List EditableOpenIDServiceConfig -> Html Msg
viewContent appState model openIDPrefabs =
    GenericView.view (viewProps openIDPrefabs) appState model.genericModel


viewProps : List EditableOpenIDServiceConfig -> GenericView.ViewProps AuthenticationConfigForm Msg
viewProps openIDPrefabs =
    { locTitle = gettext "Authentication"
    , locSave = gettext "Save"
    , formView = formView openIDPrefabs
    , wrapMsg = formMsg
    }


formView : List EditableOpenIDServiceConfig -> AppState -> Form FormError AuthenticationConfigForm -> Html Msg
formView openIDPrefabs appState form =
    let
        internalAuthentication =
            let
                twoFactorAuthEnabled =
                    Maybe.withDefault False (Form.getFieldAsBool "twoFactorAuthEnabled" form).value

                twoFactorInputs =
                    if twoFactorAuthEnabled then
                        let
                            formWrap =
                                Html.map (GenericMsg << GenericMsgs.FormMsg)
                        in
                        div [ class "nested-group" ]
                            [ formWrap <| FormGroup.input appState form "twoFactorAuthCodeLength" (gettext "Code Length" appState.locale)
                            , formWrap <| FormGroup.input appState form "twoFactorAuthExpiration" (gettext "Expiration" appState.locale)
                            , FormExtra.mdAfter (gettext "Expiration time of the authentication code in **seconds**." appState.locale)
                            ]

                    else
                        emptyNode
            in
            [ h3 [] [ text (gettext "Internal" appState.locale) ]
            , mapFormMsg <| FormGroup.toggle form "registrationEnabled" (gettext "Registration" appState.locale)
            , FormExtra.mdAfter (gettext "If enabled, users can create new internal accounts directly in the instance." appState.locale)
            , mapFormMsg <| FormGroup.toggle form "twoFactorAuthEnabled" (gettext "Two-Factor Authentication" appState.locale)
            , FormExtra.mdAfter (gettext "If enabled, users first enter a username and password at login, and then they receive a one-time code to confirm the login on their email." appState.locale)
            , twoFactorInputs
            ]

        externalAuthentication =
            [ h3 [] [ text (gettext "External" appState.locale) ]
            , FormGroup.listWithCustomMsg appState formMsg (serviceFormView appState openIDPrefabs) form "services" (gettext "OpenID Services" appState.locale) (gettext "Add service" appState.locale)
            ]
    in
    div [ class "Authentication" ]
        ([ mapFormMsg <| FormGroup.select appState (Role.options appState) form "defaultRole" (gettext "Default role" appState.locale)
         , FormExtra.mdAfter (gettext "Define the role that is assigned to new users." appState.locale)
         ]
            ++ internalAuthentication
            ++ externalAuthentication
        )


serviceFormView : AppState -> List EditableOpenIDServiceConfig -> Form FormError AuthenticationConfigForm -> Int -> Html Msg
serviceFormView appState openIDPrefabs form i =
    let
        idField =
            "services." ++ String.fromInt i ++ ".id"

        nameField =
            "services." ++ String.fromInt i ++ ".name"

        urlField =
            "services." ++ String.fromInt i ++ ".url"

        clientIdField =
            "services." ++ String.fromInt i ++ ".clientId"

        clientSecretField =
            "services." ++ String.fromInt i ++ ".clientSecret"

        parametersField =
            "services." ++ String.fromInt i ++ ".parameters"

        styleBackgroundField =
            "services." ++ String.fromInt i ++ ".styleBackground"

        styleColorField =
            "services." ++ String.fromInt i ++ ".styleColor"

        styleIconField =
            "services." ++ String.fromInt i ++ ".styleIcon"

        callbackUrl =
            (Form.getFieldAsString idField form).value
                |> Maybe.map (\id -> appState.clientUrl ++ "/auth/" ++ id ++ "/callback")
                |> Maybe.withDefault "-"

        logoutUrl =
            (Form.getFieldAsString idField form).value
                |> Maybe.map (\id -> appState.apiUrl ++ "/auth/" ++ id ++ "/logout")
                |> Maybe.withDefault "-"

        buttonName =
            Maybe.withDefault "" <| (Form.getFieldAsString nameField form).value

        buttonIcon =
            (Form.getFieldAsString styleIconField form).value
                |> Maybe.andThen String.toMaybe

        buttonColor =
            (Form.getFieldAsString styleColorField form).value
                |> Maybe.andThen String.toMaybe

        buttonBackground =
            (Form.getFieldAsString styleBackgroundField form).value
                |> Maybe.andThen String.toMaybe

        prefabsView =
            if (not << List.isEmpty) openIDPrefabs && AuthenticationConfigForm.isOpenIDServiceEmpty i form then
                let
                    viewPrefabButton openID =
                        ExternalLoginButton.render [ onClick (FillOpenIDServiceConfig i openID) ]
                            appState
                            openID.name
                            openID.style.icon
                            openID.style.color
                            openID.style.background
                in
                div [ class "prefab-selection" ]
                    [ strong [] [ text (gettext "Quick setup" appState.locale) ]
                    , div [] (List.map viewPrefabButton <| List.sortBy .name openIDPrefabs)
                    ]

            else
                emptyNode
    in
    div [ class "card bg-light mb-4" ]
        [ div [ class "card-body" ]
            [ prefabsView
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ mapFormMsg <| FormGroup.input appState form idField (gettext "ID" appState.locale) ]
                , div [ class "col text-end" ]
                    [ mapFormMsg <|
                        a
                            [ class "btn btn-danger with-icon"
                            , onClick (Form.RemoveItem "services" i)
                            , dataCy "settings_authentication_service_remove-button"
                            ]
                            [ faSet "_global.delete" appState
                            , text (gettext "Remove" appState.locale)
                            ]
                    ]
                ]
            , FormGroup.textView "callback-url" callbackUrl (gettext "Callback URL" appState.locale)
            , FormGroup.textView "logout-url" logoutUrl (gettext "Logout URL" appState.locale)
            , div [ class "row" ]
                [ div [ class "col" ] [ mapFormMsg <| FormGroup.input appState form clientIdField (gettext "Client ID" appState.locale) ]
                , div [ class "col" ] [ mapFormMsg <| FormGroup.secret appState form clientSecretField (gettext "Client Secret" appState.locale) ]
                ]
            , mapFormMsg <| FormGroup.input appState form urlField (gettext "URL" appState.locale)
            , div [ class "input-table", dataCy "settings_authentication_service_parameters" ]
                [ label [] [ text (gettext "Parameters" appState.locale) ]
                , serviceParametersHeader appState parametersField form
                , mapFormMsg <| FormGroup.list appState (serviceParameterView appState parametersField) form parametersField "" (gettext "Add parameter" appState.locale)
                ]
            , hr [] []
            , div [ class "row" ]
                [ div [ class "col-7" ]
                    [ div [ class "row" ]
                        [ div [ class "col" ]
                            [ mapFormMsg <| FormGroup.inputAttrs [ placeholder <| ExternalLoginButton.defaultIcon appState ] appState form styleIconField (gettext "Icon" appState.locale)
                            ]
                        , div [ class "col" ]
                            [ mapFormMsg <| FormGroup.input appState form nameField (gettext "Name" appState.locale)
                            ]
                        ]
                    , div [ class "row" ]
                        [ div [ class "col" ]
                            [ mapFormMsg <| FormGroup.inputAttrs [ placeholder ExternalLoginButton.defaultBackground ] appState form styleBackgroundField (gettext "Background Color" appState.locale)
                            ]
                        , div [ class "col" ]
                            [ mapFormMsg <| FormGroup.inputAttrs [ placeholder ExternalLoginButton.defaultColor ] appState form styleColorField (gettext "Text Color" appState.locale)
                            ]
                        ]
                    ]
                , div [ class "col-4 offset-1" ]
                    [ div [ class "form-group" ]
                        [ label [] [ text (gettext "Button Preview" appState.locale) ]
                        , div [ class "mt-4" ]
                            [ ExternalLoginButton.render [] appState buttonName buttonIcon buttonColor buttonBackground
                            ]
                        ]
                    ]
                ]
            ]
        ]


serviceParametersHeader : AppState -> String -> Form FormError AuthenticationConfigForm -> Html msg
serviceParametersHeader appState field form =
    let
        isEmpty =
            List.isEmpty (Form.getListIndexes field form)
    in
    if isEmpty then
        emptyNode

    else
        div [ class "row input-table-header" ]
            [ div [ class "col-5" ] [ text (gettext "Name" appState.locale) ]
            , div [ class "col-6" ] [ text (gettext "Value" appState.locale) ]
            ]


serviceParameterView : AppState -> String -> Form FormError AuthenticationConfigForm -> Int -> Html Form.Msg
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
            FormGroup.getErrors appState nameField (gettext "Name" appState.locale)

        ( valueError, valueErrorClass ) =
            FormGroup.getErrors appState valueField (gettext "Value" appState.locale)
    in
    div [ class "row mb-2" ]
        [ div [ class "col-5" ]
            [ Input.textInput nameField [ class <| "form-control " ++ nameErrorClass, attribute "data-cy" "settings_authentication_service_parameter-name" ]
            , nameError
            ]
        , div [ class "col-6" ]
            [ Input.textInput valueField [ class <| "form-control " ++ valueErrorClass, attribute "data-cy" "settings_authentication_service_parameter-value" ]
            , valueError
            ]
        , div [ class "col-1 text-end" ]
            [ a [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix i) ] [ faSet "_global.delete" appState ] ]
        ]


mapFormMsg : Html Form.Msg -> Html Msg
mapFormMsg =
    Html.map formMsg


formMsg : Form.Msg -> Msg
formMsg =
    GenericMsg << GenericMsgs.FormMsg
