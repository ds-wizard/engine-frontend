module Wizard.Settings.Submission.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, a, button, div, label, p, strong)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick)
import List.Extra as List
import Shared.Data.EditableConfig.EditableSubmissionConfig exposing (EditableSubmissionConfig)
import Shared.Data.TemplateSuggestion as TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
import Shared.Markdown as Markdown
import Shared.Utils exposing (getOrganizationAndItemId, httpMethodOptions)
import Uuid
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Page as Page
import Wizard.Settings.Generic.Msgs as GenericMsgs
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.Submission.Models exposing (Model)
import Wizard.Settings.Submission.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Submission.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Submission.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewConfig appState model) model.templates


viewConfig : AppState -> Model -> List TemplateSuggestion -> Html Msg
viewConfig appState model templates =
    GenericView.view (viewProps templates) appState model.genericModel


viewProps : List TemplateSuggestion -> GenericView.ViewProps EditableSubmissionConfig Msg
viewProps templates =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView templates
    , wrapMsg = GenericMsg << GenericMsgs.FormMsg
    }


formView : List TemplateSuggestion -> AppState -> Form FormError EditableSubmissionConfig -> Html Msg
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
    Html.map (GenericMsg << GenericMsgs.FormMsg) <|
        div []
            [ FormGroup.toggle form "enabled" (l_ "form.enabled" appState)
            , FormExtra.mdAfter (l_ "form.enabled.desc" appState)
            , servicesInput
            ]


serviceFormView : AppState -> List TemplateSuggestion -> Form FormError EditableSubmissionConfig -> Int -> Html Form.Msg
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
                , div [ class "col text-end" ]
                    [ a [ class "btn btn-danger with-icon", onClick (Form.RemoveItem "services" i) ]
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


supportedFormatFormView : AppState -> List TemplateSuggestion -> String -> Form FormError EditableSubmissionConfig -> Int -> Html Form.Msg
supportedFormatFormView appState templates prefix form index =
    let
        field name =
            prefix ++ "." ++ String.fromInt index ++ "." ++ name

        templateField =
            Form.getFieldAsString (field "template") form

        templateIdField =
            Form.getFieldAsString (field "templateId") form

        formatUuidField =
            Form.getFieldAsString (field "formatUuid") form

        ( templateIdError, templateIdErrorClass ) =
            FormGroup.getErrors appState templateIdField (l_ "form.template" appState)

        ( formatUuidError, formatUuidErrorClass ) =
            FormGroup.getErrors appState formatUuidField (l_ "form.format" appState)

        defaultOption =
            ( "", "--" )

        templateOptions =
            TemplateSuggestion.createOptions templates

        templateToTemplateVersionOptions template =
            templates
                |> List.filter (.id >> getOrganizationAndItemId >> (==) template)
                |> List.sortWith (\a b -> Version.compare b.version a.version)
                |> List.map (\t -> ( t.id, Version.toString t.version ))
                |> (::) defaultOption

        templateVersionOptions =
            templateField.value
                |> Maybe.map templateToTemplateVersionOptions
                |> Maybe.withDefault []

        formatOptions =
            templateIdField.value
                |> Maybe.andThen (\uuid -> List.find (.id >> (==) uuid) templates)
                |> Maybe.map (.formats >> List.map (\f -> ( Uuid.toString f.uuid, f.name )) >> (::) defaultOption)
                |> Maybe.withDefault []
    in
    div [ class "input-group mb-2" ]
        [ Input.selectInput templateOptions templateField [ class "form-select", class templateIdErrorClass ]
        , Input.selectInput templateVersionOptions templateIdField [ class "form-select", class templateIdErrorClass ]
        , Input.selectInput formatOptions formatUuidField [ class "form-select", class formatUuidErrorClass ]
        , button [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix index) ]
            [ faSet "_global.delete" appState ]
        , templateIdError
        , formatUuidError
        ]


propFormView : AppState -> String -> Form FormError EditableSubmissionConfig -> Int -> Html Form.Msg
propFormView appState prefix form index =
    let
        field =
            Form.getFieldAsString (prefix ++ "." ++ String.fromInt index) form
    in
    div [ class "input-group mb-2" ]
        [ Input.textInput field [ class "form-control" ]
        , button [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix index) ]
            [ faSet "_global.delete" appState ]
        ]


requestFormView : AppState -> Form FormError EditableSubmissionConfig -> String -> Html Form.Msg
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

        multipartEnabled =
            Maybe.withDefault False (Form.getFieldAsBool multipartEnabledField form).value

        multipartFileNameInput =
            if multipartEnabled then
                let
                    multipartFileName =
                        field "multipart.fileName"
                in
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


headerFormView : AppState -> String -> Form FormError EditableSubmissionConfig -> Int -> Html Form.Msg
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
        , button [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix index) ]
            [ faSet "_global.delete" appState ]
        , headerError
        , valueError
        ]
