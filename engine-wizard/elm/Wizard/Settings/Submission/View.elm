module Wizard.Settings.Submission.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, a, button, div, label, p, strong)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick)
import List.Extra as List
import Markdown
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Page as Page
import Wizard.Documents.Common.Template exposing (Template)
import Wizard.Settings.Common.EditableSubmissionConfig exposing (EditableSubmissionConfig)
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.Submission.Models exposing (Model)
import Wizard.Settings.Submission.Msgs exposing (Msg(..))
import Wizard.Utils exposing (httpMethodOptions)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Submission.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Submission.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewConfig appState model) model.templates


viewConfig : AppState -> Model -> List Template -> Html Msg
viewConfig appState model templates =
    Html.map GenericMsg <|
        GenericView.view (viewProps templates) appState model.genericModel


viewProps : List Template -> GenericView.ViewProps EditableSubmissionConfig
viewProps templates =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView templates
    }


formView : List Template -> AppState -> Form CustomFormError EditableSubmissionConfig -> Html Form.Msg
formView templates appState form =
    let
        enabled =
            Maybe.withDefault False (Form.getFieldAsBool "enabled" form).value

        servicesInput =
            if enabled then
                FormGroup.list appState (serviceFormView appState templates) form "services" (l_ "form.services" appState)

            else
                emptyNode
    in
    div []
        [ FormGroup.toggle form "enabled" (l_ "form.enabled" appState)
        , FormExtra.mdAfter (l_ "form.enabled.desc" appState)
        , servicesInput
        ]


serviceFormView : AppState -> List Template -> Form CustomFormError EditableSubmissionConfig -> Int -> Html Form.Msg
serviceFormView appState templates form i =
    let
        field name =
            "services." ++ String.fromInt i ++ "." ++ name

        idField =
            field "id"

        nameField =
            field "name"

        descriptionField =
            field "description"

        supportedFormatsField =
            field "supportedFormats"

        propsField =
            field "props"

        requestField =
            field "request"
    in
    div [ class "card bg-light mb-4" ]
        [ div [ class "card-body" ]
            [ div [ class "row" ]
                [ div [ class "col" ]
                    [ FormGroup.input appState form idField (l_ "form.id" appState) ]
                , div [ class "col text-right" ]
                    [ a [ class "btn btn-danger link-with-icon", onClick (Form.RemoveItem "services" i) ]
                        [ faSet "_global.delete" appState
                        , lx_ "form.service.remove" appState
                        ]
                    ]
                ]
            , FormGroup.input appState form nameField (l_ "form.name" appState)
            , FormGroup.markdownEditor appState form descriptionField (l_ "form.description" appState)
            , div [ class "input-table" ]
                [ label [] [ lx_ "form.supportedFormats" appState ]
                , p [ class "text-muted" ] [ lx_ "form.supportedFormats.desc" appState ]
                , FormGroup.list appState (supportedFormatFormView appState templates supportedFormatsField) form supportedFormatsField ""
                ]
            , div [ class "input-table" ]
                [ label [] [ lx_ "form.props" appState ]
                , Markdown.toHtml [ class "text-muted text-justify" ] (l_ "form.props.desc" appState)
                , FormGroup.list appState (propFormView appState propsField) form propsField ""
                ]
            , requestFormView appState form requestField
            ]
        ]


supportedFormatFormView : AppState -> List Template -> String -> Form CustomFormError EditableSubmissionConfig -> Int -> Html Form.Msg
supportedFormatFormView appState templates prefix form index =
    let
        field name =
            prefix ++ "." ++ String.fromInt index ++ "." ++ name

        templateUuidField =
            Form.getFieldAsString (field "templateUuid") form

        formatUuidField =
            Form.getFieldAsString (field "formatUuid") form

        ( templateUuidError, templateUuidErrorClass ) =
            FormGroup.getErrors appState templateUuidField (l_ "form.template" appState)

        ( formatUuidError, formatUuidErrorClass ) =
            FormGroup.getErrors appState formatUuidField (l_ "form.format" appState)

        defaultOption =
            ( "", "--" )

        templateOptions =
            defaultOption :: List.map (\t -> ( t.uuid, t.name )) templates

        formatOptions =
            templateUuidField.value
                |> Maybe.andThen (\uuid -> List.find (.uuid >> (==) uuid) templates)
                |> Maybe.map (.formats >> List.map (\f -> ( f.uuid, f.name )) >> (::) defaultOption)
                |> Maybe.withDefault []
    in
    div [ class "input-group mb-2" ]
        [ Input.selectInput templateOptions templateUuidField [ class "form-control", class templateUuidErrorClass ]
        , Input.selectInput formatOptions formatUuidField [ class "form-control", class formatUuidErrorClass ]
        , div [ class "input-group-append" ]
            [ button [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix index) ]
                [ faSet "_global.delete" appState ]
            ]
        , templateUuidError
        , formatUuidError
        ]


propFormView : AppState -> String -> Form CustomFormError EditableSubmissionConfig -> Int -> Html Form.Msg
propFormView appState prefix form index =
    let
        field =
            Form.getFieldAsString (prefix ++ "." ++ String.fromInt index) form
    in
    div [ class "input-group mb-2" ]
        [ Input.textInput field [ class "form-control" ]
        , div [ class "input-group-append" ]
            [ button [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix index) ]
                [ faSet "_global.delete" appState ]
            ]
        ]


requestFormView : AppState -> Form CustomFormError EditableSubmissionConfig -> String -> Html Form.Msg
requestFormView appState form prefix =
    let
        field name =
            prefix ++ "." ++ name

        urlField =
            field "url"

        methodField =
            field "method"

        headersField =
            field "headers"

        multipartEnabledField =
            field "multipart.enabled"

        multipartFileName =
            field "multipart.fileName"

        multipartEnabled =
            Maybe.withDefault False (Form.getFieldAsBool multipartEnabledField form).value

        multipartFileNameInput =
            if multipartEnabled then
                div [ class "nested-group" ]
                    [ FormGroup.input appState form multipartFileName (l_ "form.multipartFileName" appState)
                    ]

            else
                emptyNode
    in
    div []
        [ strong [] [ lx_ "form.request" appState ]
        , div [ class "nested-group mt-2" ]
            [ FormGroup.select appState httpMethodOptions form methodField (l_ "form.method" appState)
            , FormGroup.input appState form urlField (l_ "form.url" appState)
            , FormGroup.list appState (headerFormView appState headersField) form headersField (l_ "form.headers" appState)
            , FormGroup.toggle form multipartEnabledField (l_ "form.multipart" appState)
            , FormExtra.mdAfter (l_ "form.multipart.desc" appState)
            , multipartFileNameInput
            ]
        ]


headerFormView : AppState -> String -> Form CustomFormError EditableSubmissionConfig -> Int -> Html Form.Msg
headerFormView appState prefix form index =
    let
        field name =
            prefix ++ "." ++ String.fromInt index ++ "." ++ name

        headerField =
            Form.getFieldAsString (field "key") form

        valueField =
            Form.getFieldAsString (field "value") form

        ( headerError, headerErrorClass ) =
            FormGroup.getErrors appState headerField (l_ "form.header" appState)

        ( valueError, valueErrorClass ) =
            FormGroup.getErrors appState valueField (l_ "form.value" appState)
    in
    div [ class "input-group mb-2" ]
        [ Input.textInput headerField [ placeholder (l_ "form.header" appState), class "form-control", class headerErrorClass ]
        , Input.textInput valueField [ placeholder (l_ "form.value" appState), class "form-control", class valueErrorClass ]
        , div [ class "input-group-append" ]
            [ button [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix index) ]
                [ faSet "_global.delete" appState ]
            ]
        , headerError
        , valueError
        ]
