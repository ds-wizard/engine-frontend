module Wizard.Settings.Authentication.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, a, div, h3, hr, label, strong, text)
import Html.Attributes exposing (attribute, class, placeholder)
import Html.Events exposing (onClick)
import Shared.Auth.Role as Role
import Shared.Data.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
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


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Authentication.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Authentication.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState model) model.openIDPrefabs


viewContent : AppState -> Model -> List EditableOpenIDServiceConfig -> Html Msg
viewContent appState model openIDPrefabs =
    GenericView.view (viewProps openIDPrefabs) appState model.genericModel


viewProps : List EditableOpenIDServiceConfig -> GenericView.ViewProps AuthenticationConfigForm Msg
viewProps openIDPrefabs =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView openIDPrefabs
    , wrapMsg = formMsg
    }


formView : List EditableOpenIDServiceConfig -> AppState -> Form FormError AuthenticationConfigForm -> Html Msg
formView openIDPrefabs appState form =
    div [ class "Authentication" ]
        [ mapFormMsg <| FormGroup.select appState (Role.options appState) form "defaultRole" (l_ "form.defaultRole" appState)
        , FormExtra.mdAfter (l_ "form.defaultRole.desc" appState)
        , h3 [] [ lx_ "section.internal" appState ]
        , mapFormMsg <| FormGroup.toggle form "registrationEnabled" (l_ "form.registration" appState)
        , FormExtra.mdAfter (l_ "form.registration.desc" appState)
        , h3 [] [ lx_ "section.external" appState ]
        , FormGroup.listWithCustomMsg appState formMsg (serviceFormView appState openIDPrefabs) form "services" (l_ "form.services" appState)
        ]


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
                    [ strong [] [ text "Quick setup" ]
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
                    [ mapFormMsg <| FormGroup.input appState form idField (l_ "form.service.id" appState) ]
                , div [ class "col text-end" ]
                    [ mapFormMsg <|
                        a
                            [ class "btn btn-danger with-icon"
                            , onClick (Form.RemoveItem "services" i)
                            , dataCy "settings_authentication_service_remove-button"
                            ]
                            [ faSet "_global.delete" appState
                            , lx_ "form.service.remove" appState
                            ]
                    ]
                ]
            , FormGroup.textView "callback-url" callbackUrl (l_ "form.service.callbackUrl" appState)
            , div [ class "row" ]
                [ div [ class "col" ] [ mapFormMsg <| FormGroup.input appState form clientIdField (l_ "form.service.clientId" appState) ]
                , div [ class "col" ] [ mapFormMsg <| FormGroup.input appState form clientSecretField (l_ "form.service.clientSecret" appState) ]
                ]
            , mapFormMsg <| FormGroup.input appState form urlField (l_ "form.service.url" appState)
            , div [ class "input-table", dataCy "settings_authentication_service_parameters" ]
                [ label [] [ lx_ "form.service.parameters" appState ]
                , serviceParametersHeader appState parametersField form
                , mapFormMsg <| FormGroup.list appState (serviceParameterView appState parametersField) form parametersField ""
                ]
            , hr [] []
            , div [ class "row" ]
                [ div [ class "col-7" ]
                    [ div [ class "row" ]
                        [ div [ class "col" ]
                            [ mapFormMsg <| FormGroup.inputAttrs [ placeholder <| ExternalLoginButton.defaultIcon appState ] appState form styleIconField (l_ "form.service.icon" appState)
                            ]
                        , div [ class "col" ]
                            [ mapFormMsg <| FormGroup.input appState form nameField (l_ "form.service.name" appState)
                            ]
                        ]
                    , div [ class "row" ]
                        [ div [ class "col" ]
                            [ mapFormMsg <| FormGroup.inputAttrs [ placeholder ExternalLoginButton.defaultBackground ] appState form styleBackgroundField (l_ "form.service.background" appState)
                            ]
                        , div [ class "col" ]
                            [ mapFormMsg <| FormGroup.inputAttrs [ placeholder ExternalLoginButton.defaultColor ] appState form styleColorField (l_ "form.service.color" appState)
                            ]
                        ]
                    ]
                , div [ class "col-4 offset-1" ]
                    [ div [ class "form-group" ]
                        [ label [] [ lx_ "form.service.buttonPreview" appState ]
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
            List.length (Form.getListIndexes field form) == 0
    in
    if isEmpty then
        emptyNode

    else
        div [ class "row input-table-header" ]
            [ div [ class "col-5" ] [ lx_ "form.service.parameter.name" appState ]
            , div [ class "col-6" ] [ lx_ "form.service.parameter.value" appState ]
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
            FormGroup.getErrors appState nameField (l_ "form.service.parameter.name" appState)

        ( valueError, valueErrorClass ) =
            FormGroup.getErrors appState valueField (l_ "form.service.parameter.value" appState)
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
