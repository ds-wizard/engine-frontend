module Wizard.Settings.Authentication.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, a, div, h3, hr, label)
import Html.Attributes exposing (attribute, class, placeholder)
import Html.Events exposing (onClick)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
import String.Extra as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.View.ExternalLoginButton as ExternalLoginButton
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Authentication.Models exposing (Model)
import Wizard.Settings.Common.Forms.AuthenticationConfigForm exposing (AuthenticationConfigForm)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.View as GenericView
import Wizard.Users.Common.Role as Role


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Authentication.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Authentication.View"


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps AuthenticationConfigForm
viewProps =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView
    }


formView : AppState -> Form CustomFormError AuthenticationConfigForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.select appState (Role.options appState) form "defaultRole" (l_ "form.defaultRole" appState)
        , FormExtra.mdAfter (l_ "form.defaultRole.desc" appState)
        , h3 [] [ lx_ "section.internal" appState ]
        , FormGroup.toggle form "registrationEnabled" (l_ "form.registration" appState)
        , FormExtra.mdAfter (l_ "form.registration.desc" appState)
        , h3 [] [ lx_ "section.external" appState ]
        , FormGroup.list appState (serviceFormView appState) form "services" (l_ "form.services" appState)
        ]


serviceFormView : AppState -> Form CustomFormError AuthenticationConfigForm -> Int -> Html Form.Msg
serviceFormView appState form i =
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
    in
    div [ class "card bg-light mb-4" ]
        [ div [ class "card-body" ]
            [ div [ class "row" ]
                [ div [ class "col" ]
                    [ FormGroup.input appState form idField (l_ "form.service.id" appState) ]
                , div [ class "col text-right" ]
                    [ a [ class "btn btn-danger link-with-icon", onClick (Form.RemoveItem "services" i) ]
                        [ faSet "_global.delete" appState
                        , lx_ "form.service.remove" appState
                        ]
                    ]
                ]
            , FormGroup.textView callbackUrl (l_ "form.service.callbackUrl" appState)
            , div [ class "row" ]
                [ div [ class "col" ] [ FormGroup.input appState form clientIdField (l_ "form.service.clientId" appState) ]
                , div [ class "col" ] [ FormGroup.input appState form clientSecretField (l_ "form.service.clientSecret" appState) ]
                ]
            , FormGroup.input appState form urlField (l_ "form.service.url" appState)
            , div [ class "input-table" ]
                [ label [] [ lx_ "form.service.parameters" appState ]
                , serviceParametersHeader appState parametersField form
                , FormGroup.list appState (serviceParameterView appState parametersField) form parametersField ""
                ]
            , hr [] []
            , div [ class "row" ]
                [ div [ class "col-7" ]
                    [ div [ class "row" ]
                        [ div [ class "col" ]
                            [ FormGroup.inputAttrs [ placeholder <| ExternalLoginButton.defaultIcon appState ] appState form styleIconField (l_ "form.service.icon" appState)
                            ]
                        , div [ class "col" ]
                            [ FormGroup.input appState form nameField (l_ "form.service.name" appState)
                            ]
                        ]
                    , div [ class "row" ]
                        [ div [ class "col" ]
                            [ FormGroup.inputAttrs [ placeholder ExternalLoginButton.defaultBackground ] appState form styleBackgroundField (l_ "form.service.background" appState)
                            ]
                        , div [ class "col" ]
                            [ FormGroup.inputAttrs [ placeholder ExternalLoginButton.defaultColor ] appState form styleColorField (l_ "form.service.color" appState)
                            ]
                        ]
                    ]
                , div [ class "col-4 offset-1" ]
                    [ div [ class "form-group" ]
                        [ label [] [ lx_ "form.service.buttonPreview" appState ]
                        , div [ class "mt-4" ]
                            [ ExternalLoginButton.preview appState buttonName buttonIcon buttonColor buttonBackground
                            ]
                        ]
                    ]
                ]
            ]
        ]


serviceParametersHeader : AppState -> String -> Form CustomFormError AuthenticationConfigForm -> Html msg
serviceParametersHeader appState field form =
    let
        isEmpty =
            List.length (Form.getListIndexes field form) == 0
    in
    if isEmpty then
        emptyNode

    else
        div [ class "row input-table-header" ]
            [ div [ class "col-5" ] [ lx_ "form.service.parameter.name" appState ]
            , div [ class "col-6" ] [ lx_ "form.service.parameter.value" appState ]
            ]


serviceParameterView : AppState -> String -> Form CustomFormError AuthenticationConfigForm -> Int -> Html Form.Msg
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
            FormGroup.getErrors appState nameField (l_ "form.service.parameter.name" appState)

        ( valueError, valueErrorClass ) =
            FormGroup.getErrors appState valueField (l_ "form.service.parameter.value" appState)
    in
    div [ class "row mb-2" ]
        [ div [ class "col-5" ]
            [ Input.textInput nameField [ class <| "form-control " ++ nameErrorClass, attribute "data-cy" "input-name" ]
            , nameError
            ]
        , div [ class "col-6" ]
            [ Input.textInput valueField [ class <| "form-control " ++ valueErrorClass, attribute "data-cy" "input-value" ]
            , valueError
            ]
        , div [ class "col-1 text-right" ]
            [ a [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix i) ] [ faSet "_global.delete" appState ] ]
        ]
