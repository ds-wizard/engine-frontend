module Wizard.Settings.LookAndFeel.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, a, div, img, label)
import Html.Attributes exposing (attribute, class, placeholder, src)
import Html.Events exposing (onClick)
import Markdown
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.LookAndFeel.Models exposing (Model)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.LookAndFeel.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.LookAndFeel.View"


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps LookAndFeelConfig
viewProps =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView
    }


formView : AppState -> Form FormError LookAndFeelConfig -> Html Form.Msg
formView appState form =
    div []
        [ div [ class "row" ]
            [ div [ class "col-8" ]
                [ FormGroup.inputAttrs [ placeholder LookAndFeelConfig.defaultAppTitle ] appState form "appTitle" (l_ "form.appTitle" appState)
                , FormExtra.mdAfter (l_ "form.appTitle.desc" appState)
                ]
            , div
                [ class "col-4" ]
                [ img [ class "settings-img", src "/img/settings/app-title.png" ] []
                ]
            ]
        , div [ class "row mt-5" ]
            [ div [ class "col-8" ]
                [ FormGroup.inputAttrs [ placeholder LookAndFeelConfig.defaultAppTitleShort ] appState form "appTitleShort" (l_ "form.appTitleShort" appState)
                , FormExtra.mdAfter (l_ "form.appTitleShort.desc" appState)
                ]
            , div [ class "col-4" ]
                [ img [ class "settings-img", src "/img/settings/app-title-short.png" ] []
                ]
            ]
        , div [ class "input-table mt-5" ]
            [ div [ class "row" ]
                [ div [ class "col-8" ]
                    [ label [] [ lx_ "form.customMenuLinks" appState ]
                    , Markdown.toHtml [ class "form-text text-muted" ] (l_ "form.customMenuLinks.desc" appState)
                    ]
                , div [ class "col-4" ]
                    [ img [ class "settings-img", src "/img/settings/custom-menu-links.png" ] [] ]
                ]
            , div [ class "row mt-3" ]
                [ div [ class "col" ]
                    [ customMenuLinksHeader appState form
                    , FormGroup.list appState (customMenuLinkItemView appState) form "customMenuLinks" ""
                    ]
                ]
            ]
        , div [ class "row mt-5" ]
            [ div [ class "col-12" ]
                [ label [] [ lx_ "form.loginInfo" appState ] ]
            , div [ class "col-8" ]
                [ FormGroup.markdownEditor appState form "loginInfo" ""
                , FormExtra.mdAfter (l_ "form.loginInfo.desc" appState)
                ]
            , div [ class "col-4" ]
                [ img [ class "settings-img", src "/img/settings/login-info-text.png" ] []
                ]
            ]
        ]


customMenuLinksHeader : AppState -> Form FormError LookAndFeelConfig -> Html msg
customMenuLinksHeader appState form =
    let
        isEmpty =
            List.length (Form.getListIndexes "customMenuLinks" form) == 0
    in
    if isEmpty then
        emptyNode

    else
        div [ class "row input-table-header" ]
            [ div [ class "col-2" ]
                [ lx_ "form.customMenuLinks.icon" appState ]
            , div [ class "col-3" ]
                [ lx_ "form.customMenuLinks.title" appState ]
            , div [ class "col-4" ]
                [ lx_ "form.customMenuLinks.url" appState ]
            , div [ class "col-3" ]
                [ lx_ "form.customMenuLinks.newWindow" appState ]
            ]


customMenuLinkItemView : AppState -> Form FormError LookAndFeelConfig -> Int -> Html Form.Msg
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
            [ Input.textInput iconField [ class <| "form-control " ++ iconErrorClass, attribute "data-cy" "input-icon" ] ]
        , div [ class "col-3" ]
            [ Input.textInput titleField [ class <| "form-control " ++ titleErrorClass, attribute "data-cy" "input-title" ] ]
        , div [ class "col-4" ]
            [ Input.textInput urlField [ class <| " form-control " ++ urlErrorClass, attribute "data-cy" "input-url" ] ]
        , div [ class "col-2" ]
            [ label [ class "checkbox-label" ] [ Input.checkboxInput newWindowField [ attribute "data-cy" "input-new-window" ] ]
            ]
        , div [ class "col-1 text-right" ]
            [ a [ class "btn btn-link text-danger", onClick (Form.RemoveItem "customMenuLinks" i), attribute "data-cy" "button-remove" ]
                [ faSet "_global.delete" appState ]
            ]
        , iconError
        , titleError
        , urlError
        ]
