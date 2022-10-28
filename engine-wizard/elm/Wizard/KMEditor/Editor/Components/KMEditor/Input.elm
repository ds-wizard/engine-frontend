module Wizard.KMEditor.Editor.Components.KMEditor.Input exposing
    ( AnnotationsInputConfig
    , CheckboxInputConfig
    , ColorInputConfig
    , HeadersInputConfig
    , InputConfig
    , MarkdownInputConfig
    , MetricsInputConfig
    , PropsInputConfig
    , ReorderableInputConfig
    , SelectInputConfig
    , TagsInputConfig
    , annotations
    , checkbox
    , color
    , headers
    , markdown
    , metrics
    , props
    , reorderable
    , select
    , string
    , tags
    , textarea
    )

import Gettext exposing (gettext)
import Html exposing (Html, a, div, input, label, li, option, span, text, ul)
import Html.Attributes exposing (attribute, checked, class, classList, for, id, name, placeholder, rows, selected, step, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Keyed
import List.Extra as List
import Maybe.Extra as Maybe
import Reorderable
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Integration.RequestHeader as RequestHeader exposing (RequestHeader)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.MetricMeasure as MetricMeasure exposing (MetricMeasure)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Html exposing (faSet)
import Shared.Markdown as Markdown
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, grammarlyAttribute)
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
    div [ class "form-group form-group-markdown" ]
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
                [ text (gettext "You can use markdown and see the result in the preview tab." appState.locale) ]
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
    , onEdit : List Annotation -> msg
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
                        , onInput (config.onEdit << updateKeyAt i)
                        , class "form-control"
                        , placeholder (gettext "Key" appState.locale)
                        , dataCy "annotation_key"
                        ]
                        []
                    , Html.textarea
                        [ value annotation.value
                        , onInput (config.onEdit << updateValAt i)
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
                        , onClick (config.onEdit <| removeAt i)
                        , dataCy "annotation_remove-button"
                        ]
                        [ faSet "_global.delete" appState ]
                    ]
                ]
            )

        addAnnotation =
            a
                [ class "with-icon"
                , onClick (config.onEdit (config.annotations ++ [ Annotation.new ]))
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
            }
    in
    div [ class "form-group" ]
        [ label [] [ text config.label ]
        , Tag.list appState tagListConfig config.tags
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
    , onChange : List String -> msg
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
                    , onInput (config.onChange << updateAt i)
                    , class "form-control mb-2"
                    , dataCy "props-input_input"
                    , name <| "prop." ++ String.fromInt i
                    ]
                    []
                , a
                    [ class "btn btn-link text-danger"
                    , onClick <| config.onChange <| removeAt i
                    , attribute "data-cy" "prop-remove"
                    ]
                    [ faSet "_global.delete" appState ]
                ]
            )

        addProp =
            a
                [ onClick (config.onChange (config.values ++ [ "" ]))
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
    , onEdit : List RequestHeader -> msg
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
                    , onInput (config.onEdit << updateKeyAt i)
                    , placeholder (gettext "Header Name" appState.locale)
                    , class "form-control"
                    , dataCy "integration-input_name"
                    ]
                    []
                , input
                    [ type_ "text"
                    , value header.value
                    , onInput (config.onEdit << updateValAt i)
                    , placeholder (gettext "Header Value" appState.locale)
                    , class "form-control"
                    , dataCy "integration-input_value"
                    ]
                    []
                , a
                    [ class "btn btn-link text-danger"
                    , onClick (config.onEdit <| removeAt i)
                    , attribute "data-cy" "prop-remove"
                    ]
                    [ faSet "_global.delete" appState ]
                ]
            )

        addHeader =
            a
                [ class "with-icon"
                , onClick (config.onEdit (config.headers ++ [ RequestHeader.new ]))
                , dataCy "integration-input_add-button"
                ]
                [ faSet "_global.add" appState
                , text (gettext "Add" appState.locale)
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
