module Wizard.KMEditor.Editor.Components.KMEditor.Input exposing
    ( AnnotationsInputConfig
    , CheckboxInputConfig
    , ColorInputConfig
    , HeadersInputConfig
    , InputConfig
    , InputFileConfig
    , MarkdownInputConfig
    , MetricsInputConfig
    , PropsInputConfig
    , QuestionValidationsInputConfig
    , ReorderableInputConfig
    , SelectInputConfig
    , SelectWithGroupsInputConfig
    , TagsInputConfig
    , annotations
    , checkbox
    , color
    , fileSize
    , headers
    , markdown
    , metrics
    , props
    , questionValidations
    , reorderable
    , select
    , selectWithGroups
    , string
    , tags
    , textarea
    )

import Gettext exposing (gettext)
import Html exposing (Html, a, div, input, label, li, optgroup, option, span, text, ul)
import Html.Attributes as Attribute exposing (attribute, checked, class, classList, for, href, id, name, placeholder, rows, selected, step, style, target, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Events.Extra exposing (onChange)
import Html.Keyed
import List.Extra as List
import Maybe.Extra as Maybe
import Reorderable
import Shared.Common.ByteUnits as ByteUnits
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Integration.RequestHeader as RequestHeader exposing (RequestHeader)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.MetricMeasure as MetricMeasure exposing (MetricMeasure)
import Shared.Data.KnowledgeModel.Question.QuestionValidation as QuestionValidation exposing (QuestionValidation)
import Shared.Data.KnowledgeModel.Question.QuestionValueType as QuestionValueType exposing (QuestionValueType)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Markdown as Markdown
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.DatePicker as DatePicker
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, grammarlyAttribute, tooltipLeft)
import Wizard.Common.View.Tag as Tag
import Wizard.Routes



-- Basic Inputs


type alias InputConfig msg =
    { name : String
    , label : String
    , value : String
    , onInput : String -> msg
    }


string : InputConfig msg -> Html msg
string config =
    div [ class "form-group" ]
        [ label [ for config.name ] [ text config.label ]
        , input
            [ type_ "text"
            , class "form-control"
            , id config.name
            , name config.name
            , value config.value
            , onInput config.onInput
            ]
            []
        ]


type alias InputFileConfig msg =
    { name : String
    , label : String
    , value : String
    , onInput : String -> msg
    , maxFileSize : Int
    }


fileSize : InputFileConfig msg -> Html msg
fileSize config =
    let
        preProcess value =
            case String.toInt value of
                Just size ->
                    if size > 0 then
                        if size <= config.maxFileSize then
                            value

                        else
                            String.fromInt config.maxFileSize

                    else
                        ""

                Nothing ->
                    ""

        readableValue =
            case String.toInt config.value of
                Just size ->
                    if size > 0 then
                        span [ class "input-group-text" ]
                            [ text "≈ "
                            , text (ByteUnits.toReadable size)
                            ]

                    else
                        emptyNode

                Nothing ->
                    emptyNode
    in
    div [ class "form-group" ]
        [ label [ for config.name ] [ text config.label ]
        , div [ class "input-group" ]
            [ input
                [ type_ "number"
                , step "1"
                , class "form-control"
                , id config.name
                , name config.name
                , value config.value
                , onInput (config.onInput << preProcess)
                , Attribute.max (String.fromInt config.maxFileSize)
                ]
                []
            , readableValue
            ]
        ]


textarea : InputConfig msg -> Html msg
textarea config =
    div [ class "form-group" ]
        [ label [ for config.name ] [ text config.label ]
        , Html.textarea
            [ class "form-control"
            , id config.name
            , name config.name
            , value config.value
            , onInput config.onInput
            , rows <| List.length <| String.split "\n" config.value
            ]
            []
        ]



-- Checkbox Input


type alias CheckboxInputConfig msg =
    { name : String
    , label : String
    , value : Bool
    , onInput : Bool -> msg
    }


checkbox : CheckboxInputConfig msg -> Html msg
checkbox config =
    div [ class "form-group" ]
        [ div [ class "form-check" ]
            [ label [ class "form-check-label form-check-toggle" ]
                [ input
                    [ type_ "checkbox"
                    , class "form-check-input"
                    , name config.name
                    , id config.name
                    , checked config.value
                    , onCheck config.onInput
                    ]
                    []
                , span [] [ text config.label ]
                ]
            ]
        ]



-- Select Input


type alias SelectInputConfig msg =
    { name : String
    , label : String
    , value : String
    , options : List ( String, String )
    , onChange : String -> msg
    , extra : Maybe (Html msg)
    }


select : SelectInputConfig msg -> Html msg
select config =
    let
        viewOption ( optionValue, optionLabel ) =
            option [ value optionValue, selected (optionValue == config.value) ]
                [ text optionLabel ]
    in
    div [ class "form-group" ]
        [ label [ for config.name ] [ text config.label ]
        , Html.select
            [ class "form-control"
            , id config.name
            , name config.name
            , onInput config.onChange
            ]
            (List.map viewOption config.options)
        , Maybe.withDefault emptyNode config.extra
        ]


type alias SelectWithGroupsInputConfig msg =
    { name : String
    , label : String
    , value : String
    , defaultOption : ( String, String )
    , options : List ( String, List ( String, String ) )
    , onChange : String -> msg
    }


selectWithGroups : SelectWithGroupsInputConfig msg -> Html msg
selectWithGroups config =
    let
        viewGroup ( groupTitle, groupOptions ) =
            optgroup [ attribute "label" groupTitle ]
                (List.map viewOption groupOptions)

        viewOption ( optionValue, optionLabel ) =
            option [ value optionValue, selected (optionValue == config.value) ]
                [ text optionLabel ]
    in
    div [ class "form-group" ]
        [ label [ for config.name ] [ text config.label ]
        , Html.select
            [ class "form-control"
            , id config.name
            , name config.name
            , onInput config.onChange
            ]
            (viewOption config.defaultOption :: List.map viewGroup config.options)
        ]



-- Markdown Input


type alias MarkdownInputConfig msg =
    { name : String
    , label : String
    , value : String
    , onInput : String -> msg
    , previewMsg : Bool -> String -> msg
    , entityUuid : String
    , markdownPreviews : List String
    }


markdown : AppState -> MarkdownInputConfig msg -> Html msg
markdown appState config =
    let
        fieldIdentifier =
            createFieldId config.entityUuid config.name

        previewActive =
            List.member fieldIdentifier config.markdownPreviews

        content =
            if previewActive then
                Markdown.toHtml [] config.value

            else
                Html.textarea
                    [ class "form-control"
                    , id config.name
                    , name config.name
                    , onInput config.onInput
                    , value config.value
                    , rows <| List.length <| String.lines config.value
                    , grammarlyAttribute
                    ]
                    []
    in
    div [ class "form-group form-group-markup-editor" ]
        [ label [ for config.name ] [ text config.label ]
        , div [ class "card" ]
            [ div [ class "card-header" ]
                [ ul [ class "nav nav-tabs card-header-tabs" ]
                    [ li [ class "nav-item" ]
                        [ a
                            [ onClick (config.previewMsg False fieldIdentifier)
                            , class "nav-link"
                            , classList [ ( "active", not previewActive ) ]
                            ]
                            [ text (gettext "Editor" appState.locale) ]
                        ]
                    , li [ class "nav-item" ]
                        [ a
                            [ onClick (config.previewMsg True fieldIdentifier)
                            , class "nav-link"
                            , classList [ ( "active", previewActive ) ]
                            ]
                            [ text (gettext "Preview" appState.locale) ]
                        ]
                    ]
                ]
            , div [ class "card-body" ] [ content ]
            , div [ class "card-footer text-muted" ]
                (String.formatHtml (gettext "You can use %s and see the result in the preview tab." appState.locale)
                    [ a [ href (GuideLinks.markdownCheatsheet appState.guideLinks), target "_blank" ] [ text "Markdown" ] ]
                )
            ]
        ]



-- Reorderable Input


type alias ReorderableInputConfig msg =
    { name : String
    , label : String
    , items : List String
    , entityUuid : String
    , getReorderableState : String -> Maybe Reorderable.State
    , toMsg : String -> Reorderable.Msg -> msg
    , updateList : List String -> msg
    , getRoute : String -> Wizard.Routes.Route
    , getName : String -> String
    , untitledLabel : String
    , addChildMsg : msg
    , addChildLabel : String
    , addChildDataCy : String
    }


reorderable : AppState -> ReorderableInputConfig msg -> Html msg
reorderable appState config =
    let
        fieldIdentifier =
            createFieldId config.entityUuid config.name

        reorderableState =
            config.getReorderableState fieldIdentifier
                |> Maybe.withDefault Reorderable.initialState

        inputChild ignoreDrag item =
            let
                itemName =
                    config.getName item

                ( untitled, visibleName ) =
                    if String.isEmpty itemName then
                        ( True, config.untitledLabel )

                    else
                        ( False, itemName )
            in
            div [ classList [ ( "untitled", untitled ) ] ]
                [ ignoreDrag (linkTo appState (config.getRoute item))
                    []
                    [ text visibleName ]
                ]

        placeholderView =
            div [] [ text "-" ]

        addChild =
            a
                [ onClick config.addChildMsg
                , class "link-add-child with-icon"
                , dataCy ("km-editor_input-children_" ++ config.addChildDataCy ++ "_add-button")
                ]
                [ faSet "_global.add" appState
                , text config.addChildLabel
                ]
    in
    div [ class "form-group" ]
        [ label [] [ text config.label ]
        , Reorderable.view
            { toId = identity
            , toMsg = config.toMsg fieldIdentifier
            , updateList = config.updateList
            , itemView = inputChild
            , placeholderView = placeholderView
            , listClass = "input-children"
            , itemClass = "input-child"
            , placeholderClass = "input-child input-child-placeholder"
            }
            reorderableState
            config.items
        , addChild
        ]



-- Annotations Input


type alias AnnotationsInputConfig msg =
    { annotations : List Annotation
    , onEdit : Maybe String -> List Annotation -> msg
    }


annotations : AppState -> AnnotationsInputConfig msg -> Html msg
annotations appState config =
    let
        updateKeyAt index newKey =
            let
                update_ annotation =
                    { key = newKey, value = annotation.value }
            in
            List.updateAt index update_ config.annotations

        updateValAt index newVal =
            let
                update_ annotation =
                    { key = annotation.key, value = newVal }
            in
            List.updateAt index update_ config.annotations

        removeAt index =
            List.removeAt index config.annotations

        viewAnnotation i annotation =
            let
                lines =
                    annotation.value
                        |> String.split "\n"
                        |> List.length
                        |> max 2
            in
            ( "annotation." ++ String.fromInt i
            , div
                [ class "annotations-editor-item"
                , dataCy "annotations_item"
                ]
                [ div [ class "annotations-editor-item-inputs" ]
                    [ input
                        [ type_ "text"
                        , value annotation.key
                        , onInput (config.onEdit Nothing << updateKeyAt i)
                        , class "form-control"
                        , placeholder (gettext "Key" appState.locale)
                        , dataCy "annotation_key"
                        ]
                        []
                    , Html.textarea
                        [ value annotation.value
                        , onInput (config.onEdit Nothing << updateValAt i)
                        , class "form-control"
                        , placeholder (gettext "Value" appState.locale)
                        , dataCy "annotation_value"
                        , grammarlyAttribute
                        , rows lines
                        ]
                        []
                    ]
                , div []
                    [ a
                        [ class "btn btn-link text-danger"
                        , onClick (config.onEdit Nothing <| removeAt i)
                        , dataCy "annotation_remove-button"
                        ]
                        [ faSet "_global.delete" appState ]
                    ]
                ]
            )

        addAnnotation =
            a
                [ class "with-icon"
                , onClick (config.onEdit (Just ".annotations-editor-item:last-child .annotations-editor-item-inputs input") (config.annotations ++ [ Annotation.new ]))
                ]
                [ faSet "_global.add" appState
                , text (gettext "Add annotation" appState.locale)
                ]
    in
    div [ class "form-group" ]
        [ label [] [ text (gettext "Annotations" appState.locale) ]
        , Html.Keyed.node "div" [] (List.indexedMap viewAnnotation config.annotations)
        , addAnnotation
        ]



-- Tags Input


type alias TagsInputConfig msg =
    { label : String
    , tags : List Tag
    , selected : List String
    , onChange : List String -> msg
    }


tags : AppState -> TagsInputConfig msg -> Html msg
tags appState config =
    let
        tagListConfig =
            { selected = config.selected
            , addMsg = \value -> config.onChange <| value :: config.selected
            , removeMsg = \value -> config.onChange <| List.filter ((/=) value) config.selected
            , showDescription = False
            }
    in
    div [ class "form-group" ]
        [ label [] [ text config.label ]
        , Tag.list appState tagListConfig config.tags
        ]



-- Question Validations Input


type alias QuestionValidationsInputConfig msg =
    { label : String
    , valueType : QuestionValueType
    , validations : List QuestionValidation
    , onChange : List QuestionValidation -> msg
    }


questionValidations : AppState -> QuestionValidationsInputConfig msg -> Html msg
questionValidations appState config =
    let
        filteredValidationOptions =
            List.filter (List.member config.valueType << .questionTypes) (validationOptions appState)

        mbDefaultValidation =
            case config.valueType of
                QuestionValueType.StringQuestionValueType ->
                    Just QuestionValidation.minLength

                QuestionValueType.NumberQuestionValueType ->
                    Just QuestionValidation.minNumber

                QuestionValueType.DateQuestionValueType ->
                    Just QuestionValidation.fromDate

                QuestionValueType.DateTimeQuestionValueType ->
                    Just QuestionValidation.fromDateTime

                QuestionValueType.TimeQuestionValueType ->
                    Just QuestionValidation.fromTime

                QuestionValueType.TextQuestionValueType ->
                    Just QuestionValidation.minLength

                QuestionValueType.EmailQuestionValueType ->
                    Just QuestionValidation.domain

                QuestionValueType.UrlQuestionValueType ->
                    Just QuestionValidation.regex

                QuestionValueType.ColorQuestionValueType ->
                    Nothing

        filteredValidations =
            List.filter (\v -> List.member (QuestionValidation.toOptionString v) (List.map .value filteredValidationOptions)) config.validations

        removeValidationMsg i =
            config.onChange <| List.removeAt i config.validations

        changeValidationMsg i value =
            case value of
                "MinLength" ->
                    config.onChange (List.setAt i QuestionValidation.minLength config.validations)

                "MaxLength" ->
                    config.onChange (List.setAt i QuestionValidation.maxLength config.validations)

                "Regex" ->
                    config.onChange (List.setAt i QuestionValidation.regex config.validations)

                "Orcid" ->
                    config.onChange (List.setAt i QuestionValidation.orcid config.validations)

                "Doi" ->
                    config.onChange (List.setAt i QuestionValidation.doi config.validations)

                "MinNumber" ->
                    config.onChange (List.setAt i QuestionValidation.minNumber config.validations)

                "MaxNumber" ->
                    config.onChange (List.setAt i QuestionValidation.maxNumber config.validations)

                "FromDate" ->
                    config.onChange (List.setAt i QuestionValidation.fromDate config.validations)

                "ToDate" ->
                    config.onChange (List.setAt i QuestionValidation.toDate config.validations)

                "FromDateTime" ->
                    config.onChange (List.setAt i QuestionValidation.fromDateTime config.validations)

                "ToDateTime" ->
                    config.onChange (List.setAt i QuestionValidation.toDateTime config.validations)

                "FromTime" ->
                    config.onChange (List.setAt i QuestionValidation.fromTime config.validations)

                "ToTime" ->
                    config.onChange (List.setAt i QuestionValidation.toTime config.validations)

                "Domain" ->
                    config.onChange (List.setAt i QuestionValidation.domain config.validations)

                _ ->
                    config.onChange config.validations

        inputName i =
            "validation-" ++ String.fromInt i ++ "-value"

        viewValidationTypeOption validation validationOption =
            option
                [ value validationOption.value
                , selected (QuestionValidation.toOptionString validation == validationOption.value)
                ]
                [ text validationOption.label ]

        intInput i createValidation data =
            div [ class "mt-3" ]
                [ label [] [ text (gettext "Value" appState.locale) ]
                , input
                    [ type_ "number"
                    , name (inputName i)
                    , id (inputName i)
                    , class "form-control"
                    , value (String.fromInt data.value)
                    , onInput (\newValue -> config.onChange (List.setAt i (createValidation { value = Maybe.withDefault 0 <| String.toInt newValue }) config.validations))
                    ]
                    []
                ]

        floatInput i createValidation data =
            div [ class "mt-3" ]
                [ label [] [ text (gettext "Value" appState.locale) ]
                , input
                    [ type_ "number"
                    , name (inputName i)
                    , id (inputName i)
                    , class "form-control"
                    , value (String.fromFloat data.value)
                    , onInput (\newValue -> config.onChange (List.setAt i (createValidation { value = Maybe.withDefault 0 <| String.toFloat newValue }) config.validations))
                    ]
                    []
                ]

        stringInput i createValidation data =
            div [ class "mt-3" ]
                [ label [] [ text (gettext "Value" appState.locale) ]
                , input
                    [ type_ "text"
                    , name (inputName i)
                    , id (inputName i)
                    , class "form-control"
                    , value data.value
                    , onInput (\newValue -> config.onChange (List.setAt i (createValidation { value = newValue }) config.validations))
                    ]
                    []
                ]

        dateInput i createValidation data =
            div [ class "mt-3" ]
                [ label [] [ text (gettext "Value" appState.locale) ]
                , DatePicker.datePicker
                    [ DatePicker.value data.value
                    , DatePicker.onChange (\newValue -> config.onChange (List.setAt i (createValidation { value = newValue }) config.validations))
                    ]
                ]

        dateTimeInput i createValidation data =
            div [ class "mt-3" ]
                [ label [] [ text (gettext "Value" appState.locale) ]
                , DatePicker.dateTimePicker
                    [ DatePicker.value data.value
                    , DatePicker.onChange (\newValue -> config.onChange (List.setAt i (createValidation { value = newValue }) config.validations))
                    ]
                ]

        timeInput i createValidation data =
            div [ class "mt-3" ]
                [ label [] [ text (gettext "Value" appState.locale) ]
                , DatePicker.timePicker
                    [ DatePicker.value data.value
                    , DatePicker.onChange (\newValue -> config.onChange (List.setAt i (createValidation { value = newValue }) config.validations))
                    ]
                ]

        validationInput i validation =
            case validation of
                QuestionValidation.MinLength data ->
                    intInput i QuestionValidation.MinLength data

                QuestionValidation.MaxLength data ->
                    intInput i QuestionValidation.MaxLength data

                QuestionValidation.Regex data ->
                    stringInput i QuestionValidation.Regex data

                QuestionValidation.Orcid ->
                    emptyNode

                QuestionValidation.Doi ->
                    emptyNode

                QuestionValidation.MinNumber data ->
                    floatInput i QuestionValidation.MinNumber data

                QuestionValidation.MaxNumber data ->
                    floatInput i QuestionValidation.MaxNumber data

                QuestionValidation.FromDate data ->
                    dateInput i QuestionValidation.FromDate data

                QuestionValidation.ToDate data ->
                    dateInput i QuestionValidation.ToDate data

                QuestionValidation.FromDateTime data ->
                    dateTimeInput i QuestionValidation.FromDateTime data

                QuestionValidation.ToDateTime data ->
                    dateTimeInput i QuestionValidation.ToDateTime data

                QuestionValidation.FromTime data ->
                    timeInput i QuestionValidation.FromTime data

                QuestionValidation.ToTime data ->
                    timeInput i QuestionValidation.ToTime data

                QuestionValidation.Domain data ->
                    stringInput i QuestionValidation.Domain data

        viewValidation i validation =
            div [ class "card bg-light" ]
                [ div [ class "card-body" ]
                    [ a
                        (class "text-danger delete"
                            :: onClick (removeValidationMsg i)
                            :: tooltipLeft (gettext "Remove validation" appState.locale)
                        )
                        [ faSet "_global.delete" appState ]
                    , div []
                        [ label [] [ text (gettext "Validation Type" appState.locale) ]
                        , Html.select
                            [ class "form-control"
                            , name ("validation-" ++ String.fromInt i ++ "-type")
                            , id ("validation-" ++ String.fromInt i ++ "-type")
                            , onChange (changeValidationMsg i)
                            ]
                            (List.map (viewValidationTypeOption validation) filteredValidationOptions)
                        ]
                    , validationInput i validation
                    ]
                ]

        addQuestionValidationMsg validation =
            config.onChange <| List.append filteredValidations [ validation ]
    in
    case mbDefaultValidation of
        Just defaultValidation ->
            div [ class "form-group question-validations" ]
                [ label [] [ text config.label ]
                , div [] (List.indexedMap viewValidation filteredValidations)
                , div []
                    [ a
                        [ onClick (addQuestionValidationMsg defaultValidation)
                        , class "link-add-child with-icon"
                        , dataCy "km-editor_question-validations_add-button"
                        ]
                        [ faSet "_global.add" appState
                        , text (gettext "Add validation" appState.locale)
                        ]
                    ]
                ]

        Nothing ->
            emptyNode


type alias ValidationOption =
    { value : String
    , label : String
    , questionTypes : List QuestionValueType
    }


validationOptions : AppState -> List ValidationOption
validationOptions appState =
    [ { value = "MinLength"
      , label = gettext "Min Length" appState.locale
      , questionTypes =
            [ QuestionValueType.StringQuestionValueType
            , QuestionValueType.TextQuestionValueType
            ]
      }
    , { value = "MaxLength"
      , label = gettext "Max Length" appState.locale
      , questionTypes =
            [ QuestionValueType.StringQuestionValueType
            , QuestionValueType.TextQuestionValueType
            ]
      }
    , { value = "Regex"
      , label = gettext "Regex" appState.locale
      , questionTypes =
            [ QuestionValueType.StringQuestionValueType
            , QuestionValueType.TextQuestionValueType
            , QuestionValueType.EmailQuestionValueType
            , QuestionValueType.UrlQuestionValueType
            ]
      }
    , { value = "Orcid"
      , label = gettext "ORCID" appState.locale
      , questionTypes =
            [ QuestionValueType.StringQuestionValueType
            ]
      }
    , { value = "Doi"
      , label = gettext "DOI" appState.locale
      , questionTypes =
            [ QuestionValueType.StringQuestionValueType
            ]
      }
    , { value = "MinNumber"
      , label = gettext "Min Number" appState.locale
      , questionTypes =
            [ QuestionValueType.NumberQuestionValueType
            ]
      }
    , { value = "MaxNumber"
      , label = gettext "Max Number" appState.locale
      , questionTypes =
            [ QuestionValueType.NumberQuestionValueType
            ]
      }
    , { value = "FromDate"
      , label = gettext "From Date" appState.locale
      , questionTypes =
            [ QuestionValueType.DateQuestionValueType
            ]
      }
    , { value = "ToDate"
      , label = gettext "To Date" appState.locale
      , questionTypes =
            [ QuestionValueType.DateQuestionValueType
            ]
      }
    , { value = "FromDateTime"
      , label = gettext "From Date Time" appState.locale
      , questionTypes =
            [ QuestionValueType.DateTimeQuestionValueType
            ]
      }
    , { value = "ToDateTime"
      , label = gettext "To Date Time" appState.locale
      , questionTypes =
            [ QuestionValueType.DateTimeQuestionValueType
            ]
      }
    , { value = "FromTime"
      , label = gettext "From Time" appState.locale
      , questionTypes =
            [ QuestionValueType.TimeQuestionValueType
            ]
      }
    , { value = "ToTime"
      , label = gettext "To Time" appState.locale
      , questionTypes =
            [ QuestionValueType.TimeQuestionValueType
            ]
      }
    , { value = "Domain"
      , label = gettext "Domain" appState.locale
      , questionTypes =
            [ QuestionValueType.EmailQuestionValueType
            ]
      }
    ]



-- Metrics Input


type alias MetricsInputConfig msg =
    { metrics : List Metric
    , metricMeasures : List MetricMeasure
    , onChange : List MetricMeasure -> msg
    }


metrics : AppState -> MetricsInputConfig msg -> Html msg
metrics appState config =
    let
        onMetricCheck metricUuid isChecked =
            config.onChange <|
                if isChecked then
                    MetricMeasure.init metricUuid :: config.metricMeasures

                else
                    List.filter (.metricUuid >> (/=) metricUuid) config.metricMeasures

        onValueChange metricUuid setter value =
            let
                update metricMeasure =
                    if metricMeasure.metricUuid == metricUuid then
                        setter (clamp 0 1 <| Maybe.withDefault 0 <| String.toFloat value) metricMeasure

                    else
                        metricMeasure
            in
            config.onChange <| List.map update config.metricMeasures

        onWeightChange metricUuid =
            onValueChange metricUuid MetricMeasure.setWeight

        onMeasureChange metricUuid =
            onValueChange metricUuid MetricMeasure.setMeasure

        metricView metric =
            let
                toggleFieldName =
                    "metricMeasure-" ++ metric.uuid ++ "-enabled"

                metricMeasure =
                    List.find (.metricUuid >> (==) metric.uuid) config.metricMeasures

                enabled =
                    Maybe.isJust metricMeasure

                valueField fieldLabel fieldNameSuffix onValueChange_ valueGetter =
                    let
                        fieldName =
                            "metricMeasure-" ++ metric.uuid ++ "-" ++ fieldNameSuffix
                    in
                    div [ class "form-group" ]
                        [ label [] [ text fieldLabel ]
                        , input
                            [ type_ "number"
                            , class "form-control"
                            , name fieldName
                            , id fieldName
                            , onInput (onValueChange_ metric.uuid)
                            , value (String.fromFloat <| Maybe.unwrap 1 valueGetter metricMeasure)
                            , step "0.1"
                            ]
                            []
                        ]
            in
            div [ class "metric-view" ]
                [ div [ class "form-check" ]
                    [ label [ class "form-check-label form-check-toggle" ]
                        [ input
                            [ type_ "checkbox"
                            , class "form-check-input"
                            , name toggleFieldName
                            , id toggleFieldName
                            , checked enabled
                            , onCheck (onMetricCheck metric.uuid)
                            ]
                            []
                        , span [] [ text metric.title ]
                        ]
                    ]
                , div [ class "metric-view-inputs", classList [ ( "metric-view-inputs-enabled", enabled ) ] ]
                    [ valueField (gettext "Weight" appState.locale) "weight" onWeightChange .weight
                    , valueField (gettext "Measure" appState.locale) "measure" onMeasureChange .measure
                    ]
                ]
    in
    div [ class "form-group" ]
        [ label [] [ text (gettext "Metrics" appState.locale) ]
        , div [] (List.map metricView config.metrics)
        ]



-- Color Input


type alias ColorInputConfig msg =
    { name : String
    , label : String
    , value : String
    , onInput : String -> msg
    }


color : ColorInputConfig msg -> Html msg
color config =
    let
        colorButton colorHex =
            a
                [ onClick (config.onInput colorHex)
                , style "background" colorHex
                , dataCy "form-group_color_color-button"
                ]
                []
    in
    div [ class "form-group form-group-color-picker" ]
        [ label [ for config.name ] [ text config.label ]
        , div [ class "input-wrapper" ]
            [ div [ class "color-preview", style "background-color" config.value ] []
            , input
                [ type_ "text"
                , class "form-control"
                , id config.name
                , name config.name
                , value config.value
                , onInput config.onInput
                ]
                []
            ]
        , div [ class "color-buttons" ] (List.map colorButton colorOptions)
        ]


colorOptions : List String
colorOptions =
    [ "#1ABC9C"
    , "#2ECC71"
    , "#3498DB"
    , "#9B59B6"
    , "#34495E"
    , "#16A085"
    , "#27AE60"
    , "#2980B9"
    , "#8E44AD"
    , "#2C3E50"
    , "#F1C40F"
    , "#E67E22"
    , "#E74C3C"
    , "#ECF0F1"
    , "#95A5A6"
    , "#F39C12"
    , "#D35400"
    , "#C0392B"
    , "#BDC3C7"
    , "#7F8C8D"
    ]



-- Props Input


type alias PropsInputConfig msg =
    { label : String
    , values : List String
    , onChange : Maybe String -> List String -> msg
    }


props : AppState -> PropsInputConfig msg -> Html msg
props appState config =
    let
        updateAt index newValue =
            List.updateAt index (always newValue) config.values

        removeAt index =
            List.removeAt index config.values

        viewProp i prop =
            ( "prop." ++ String.fromInt i
            , div [ class "d-flex", dataCy "props-input_input-wrapper" ]
                [ input
                    [ type_ "text"
                    , value prop
                    , onInput (config.onChange Nothing << updateAt i)
                    , class "form-control mb-2"
                    , dataCy "props-input_input"
                    , name <| "prop." ++ String.fromInt i
                    ]
                    []
                , a
                    [ class "btn btn-link text-danger"
                    , onClick <| config.onChange Nothing <| removeAt i
                    , attribute "data-cy" "prop-remove"
                    ]
                    [ faSet "_global.delete" appState ]
                ]
            )

        addProp =
            a
                [ onClick (config.onChange (Just "[data-cy=props-input_input-wrapper]:last-child input") (config.values ++ [ "" ]))
                , dataCy "props-input_add-button"
                , class "with-icon"
                ]
                [ faSet "_global.add" appState
                , text (gettext "Add" appState.locale)
                ]
    in
    div [ class "form-group" ]
        [ label [] [ text config.label ]
        , Html.Keyed.node "div" [] (List.indexedMap viewProp config.values)
        , addProp
        ]



-- Headers Input


type alias HeadersInputConfig msg =
    { label : String
    , headers : List RequestHeader
    , onEdit : Maybe String -> List RequestHeader -> msg
    }


headers : AppState -> HeadersInputConfig msg -> Html msg
headers appState config =
    let
        updateKeyAt index newKey =
            let
                update_ header =
                    { key = newKey, value = header.value }
            in
            List.updateAt index update_ config.headers

        updateValAt index newVal =
            let
                update_ annotation =
                    { key = annotation.key, value = newVal }
            in
            List.updateAt index update_ config.headers

        removeAt index =
            List.removeAt index config.headers

        viewHeader i header =
            ( "header." ++ String.fromInt i
            , div
                [ class "input-group mb-2"
                , dataCy "integration-input_item"
                ]
                [ input
                    [ type_ "text"
                    , value header.key
                    , onInput (config.onEdit Nothing << updateKeyAt i)
                    , placeholder (gettext "Header Name" appState.locale)
                    , class "form-control"
                    , dataCy "integration-input_name"
                    ]
                    []
                , input
                    [ type_ "text"
                    , value header.value
                    , onInput (config.onEdit Nothing << updateValAt i)
                    , placeholder (gettext "Header Value" appState.locale)
                    , class "form-control"
                    , dataCy "integration-input_value"
                    ]
                    []
                , a
                    [ class "btn btn-link text-danger"
                    , onClick (config.onEdit Nothing <| removeAt i)
                    , attribute "data-cy" "prop-remove"
                    ]
                    [ faSet "_global.delete" appState ]
                ]
            )

        addHeader =
            a
                [ class "with-icon"
                , onClick (config.onEdit (Just "[data-cy=integration-input_item]:last-child input:first-child") (config.headers ++ [ RequestHeader.new ]))
                , dataCy "integration-input_add-button"
                ]
                [ faSet "_global.add" appState
                , text (gettext "Add header" appState.locale)
                ]
    in
    div [ class "form-group" ]
        [ label [] [ text config.label ]
        , Html.Keyed.node "div" [] (List.indexedMap viewHeader config.headers)
        , addHeader
        ]



-- Utils


createFieldId : String -> String -> String
createFieldId entityUuid fieldName =
    entityUuid ++ ":" ++ fieldName
