module Wizard.Settings.Auth.View exposing (view)

import Form exposing (Form)
import Html exposing (Html, a, div, h3, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Auth.Models exposing (Model)
import Wizard.Settings.Auth.Msgs exposing (Msg)
import Wizard.Settings.Common.AuthConfigForm exposing (AuthConfigForm)
import Wizard.Settings.Generic.View as GenericView


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Auth.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Auth.View"


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps AuthConfigForm
viewProps =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView
    }


formView : AppState -> Form CustomFormError AuthConfigForm -> Html Form.Msg
formView appState form =
    div []
        [ h3 [] [ text "Internal" ]
        , FormGroup.toggle form "registrationEnabled" (l_ "form.registration" appState)
        , FormExtra.mdAfter (l_ "form.registration.desc" appState)
        , h3 [] [ text "External" ]
        , FormGroup.list appState (serviceFormView appState) form "services" "Services"
        ]


serviceFormView : AppState -> Form CustomFormError AuthConfigForm -> Int -> Html Form.Msg
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
    in
    div [ class "card" ]
        [ div [ class "card-body" ]
            [ a [ class "btn btn-danger", onClick (Form.RemoveItem "services" i) ] [ text "Remove" ]
            , FormGroup.input appState form idField "ID"
            , FormGroup.textView callbackUrl "Callback URL"
            , FormGroup.input appState form nameField "Name"
            , FormGroup.input appState form urlField "URL"
            , FormGroup.input appState form clientIdField "Client ID"
            , FormGroup.input appState form clientSecretField "Client Secret"
            , FormGroup.input appState form styleBackgroundField "Background"
            , FormGroup.input appState form styleColorField "Color"
            , FormGroup.input appState form styleIconField "Icon"
            ]
        ]
