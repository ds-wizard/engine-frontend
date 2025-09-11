module Wizard.Pages.Dev.Operations.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Api.Models.DevOperation exposing (DevOperation)
import Common.Api.Models.DevOperation.DevOperationParameter exposing (DevOperationParameter)
import Common.Api.Models.DevOperation.DevOperationParameterType as DevOperationParameterType
import Common.Api.Models.DevOperationSection exposing (DevOperationSection)
import Common.Components.ActionButton as ActionButton
import Common.Components.Flash as Flash
import Common.Components.Page as Page
import Common.Utils.Markdown as Markdown
import Dict
import Html exposing (Html, a, div, h2, h3, input, label, span, strong, text)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Uuid
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dev.Operations.Models exposing (Model, fieldPath, operationPath)
import Wizard.Pages.Dev.Operations.Msgs exposing (Msg(..))
import Wizard.Utils.HtmlAttributesUtils exposing (settingsClass)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent model) model.adminOperationSections


viewContent : Model -> List DevOperationSection -> Html Msg
viewContent model adminOperationSections =
    let
        section =
            adminOperationSections
                |> List.find (.name >> Just >> (==) model.openedSection)
                |> Maybe.unwrap Html.nothing (viewSection model)
    in
    div [ settingsClass "Settings" ]
        [ div [ class "Settings__navigation" ] [ navigation model adminOperationSections ]
        , div [ class "Settings__content" ] [ section ]
        ]


navigation : Model -> List DevOperationSection -> Html Msg
navigation model sections =
    let
        title =
            strong [] [ text "Dev Operations" ]

        sectionLinks =
            List.map (navigationSectionLink model) sections
    in
    div [ class "nav nav-pills flex-column" ]
        (title :: sectionLinks)


navigationSectionLink : Model -> DevOperationSection -> Html Msg
navigationSectionLink model section =
    a
        [ class "nav-link"
        , classList [ ( "active", model.openedSection == Just section.name ) ]
        , onClick (OpenSection section.name)
        ]
        [ text section.name ]


viewSection : Model -> DevOperationSection -> Html Msg
viewSection model section =
    let
        operations =
            List.map (viewOperation model section.name) section.operations

        description =
            Maybe.unwrap Html.nothing (Markdown.toHtml []) section.description
    in
    div []
        [ h2 [] [ text section.name ]
        , description
        , div [] operations
        ]


viewOperation : Model -> String -> DevOperation -> Html Msg
viewOperation model sectionName operation =
    let
        description =
            Maybe.unwrap Html.nothing (Markdown.toHtml []) operation.description

        parameters =
            List.map (viewParameter model sectionName operation.name) operation.parameters

        actionResult =
            Maybe.withDefault Unset <| Dict.get (operationPath sectionName operation.name) model.operationResults

        resultView =
            case actionResult of
                Success result ->
                    Flash.success result.output

                Error error ->
                    Flash.error error

                _ ->
                    Html.nothing

        buttonConfig =
            { label = "Execute"
            , result = actionResult
            , msg = ExecuteOperation sectionName operation.name
            , dangerous = False
            }

        executeButton =
            div [ class "form-group mb-0" ]
                [ ActionButton.button buttonConfig
                ]
    in
    div [ class "mt-5" ]
        [ h3 [] [ text operation.name ]
        , description
        , div [ class "card bg-light" ]
            [ div [ class "card-body" ]
                (resultView :: parameters ++ [ executeButton ])
            ]
        ]


viewParameter : Model -> String -> String -> DevOperationParameter -> Html Msg
viewParameter model sectionName operationName parameter =
    let
        ( parameterTypeLabel, parameterTypePlaceholder ) =
            case parameter.type_ of
                DevOperationParameterType.String ->
                    ( "string", "abc" )

                DevOperationParameterType.Int ->
                    ( "int", "1" )

                DevOperationParameterType.Double ->
                    ( "double", "2.3" )

                DevOperationParameterType.Bool ->
                    ( "bool", "True / False" )

                DevOperationParameterType.Json ->
                    ( "Json", " { \"prop\": 1 }" )

                DevOperationParameterType.Tenant ->
                    ( "tenant", Uuid.toString Uuid.nil )

        path =
            fieldPath sectionName operationName parameter.name

        fieldValue =
            Maybe.withDefault "" <| Dict.get path model.fieldValues
    in
    div [ class "form-group" ]
        [ label [] [ text parameter.name ]
        , div [ class "input-group" ]
            [ span [ class "input-group-text" ] [ text parameterTypeLabel ]
            , input
                [ type_ "text"
                , class "form-control"
                , placeholder parameterTypePlaceholder
                , onInput (FieldInput path)
                , value fieldValue
                ]
                []
            ]
        ]
