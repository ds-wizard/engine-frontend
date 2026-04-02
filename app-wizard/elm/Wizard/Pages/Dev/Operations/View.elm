module Wizard.Pages.Dev.Operations.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Api.Models.DevOperation exposing (DevOperation)
import Common.Api.Models.DevOperation.DevOperationParameter exposing (DevOperationParameter)
import Common.Api.Models.DevOperation.DevOperationParameterType as DevOperationParameterType
import Common.Api.Models.DevOperationSection exposing (DevOperationSection)
import Common.Components.ActionButton as ActionButton
import Common.Components.Flash as Flash
import Common.Components.Page as Page
import Common.Components.TypeHintInput as TypeHintInput
import Common.Utils.Markdown as Markdown
import Dict
import Html exposing (Html, a, div, h2, h3, input, label, span, strong, text)
import Html.Attributes exposing (checked, class, classList, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dev.Operations.Models exposing (Model, fieldPath, getTypeHintInputModel, operationPath)
import Wizard.Pages.Dev.Operations.Msgs exposing (Msg(..))
import Wizard.Utils.HtmlAttributesUtils exposing (settingsClass)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState model) model.adminOperationSections


viewContent : AppState -> Model -> List DevOperationSection -> Html Msg
viewContent appState model adminOperationSections =
    let
        section =
            adminOperationSections
                |> List.find (.name >> Just >> (==) model.openedSection)
                |> Maybe.unwrap Html.nothing (viewSection appState model)
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


viewSection : AppState -> Model -> DevOperationSection -> Html Msg
viewSection appState model section =
    let
        operations =
            List.map (viewOperation appState model section.name) section.operations

        description =
            Maybe.unwrap Html.nothing (Markdown.toHtml []) section.description
    in
    div []
        [ h2 [] [ text section.name ]
        , description
        , div [] operations
        ]


viewOperation : AppState -> Model -> String -> DevOperation -> Html Msg
viewOperation appState model sectionName operation =
    let
        description =
            Maybe.unwrap Html.nothing (Markdown.toHtml []) operation.description

        parameters =
            List.map (viewParameter appState model sectionName operation.name) operation.parameters

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


viewParameter : AppState -> Model -> String -> String -> DevOperationParameter -> Html Msg
viewParameter appState model sectionName operationName parameter =
    let
        path =
            fieldPath sectionName operationName parameter.name

        fieldValue =
            Maybe.withDefault "" <| Dict.get path model.fieldValues

        viewSimpleParameterFormGroup_ =
            viewSimpleParameterFormGroup parameter path fieldValue
    in
    case parameter.type_ of
        DevOperationParameterType.String ->
            viewSimpleParameterFormGroup_ "string" "abc"

        DevOperationParameterType.Int ->
            viewSimpleParameterFormGroup_ "int" "1"

        DevOperationParameterType.Double ->
            viewSimpleParameterFormGroup_ "double" "2.3"

        DevOperationParameterType.Bool ->
            viewBoolParameterFormGroup parameter path fieldValue

        DevOperationParameterType.Json ->
            viewSimpleParameterFormGroup_ "Json" "{ \"prop\": 1 }"

        DevOperationParameterType.Tenant ->
            let
                item tenant =
                    div [ class "typehints-complex-item" ]
                        [ ItemIcon.tenantIcon LookAndFeelConfig.defaultLogoUrl tenant
                        , div []
                            [ strong [ class "d-block" ] [ text tenant.name ]
                            , div [] [ text tenant.clientUrl ]
                            ]
                        ]

                viewConfig =
                    { viewItem = item
                    , wrapMsg = UpdateTypeHintInput path
                    , nothingSelectedItem = span [ class "text-muted" ] [ text <| "Select tenant" ]
                    , clearEnabled = True
                    , locale = appState.locale
                    }
            in
            div [ class "form-group" ]
                [ label [] [ text parameter.name ]
                , TypeHintInput.view viewConfig (getTypeHintInputModel path model) False
                ]


viewSimpleParameterFormGroup : DevOperationParameter -> String -> String -> String -> String -> Html Msg
viewSimpleParameterFormGroup parameter path fieldValue parameterTypeLabel parameterTypePlaceholder =
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


viewBoolParameterFormGroup : DevOperationParameter -> String -> String -> Html Msg
viewBoolParameterFormGroup parameter path fieldValue =
    div [ class "form-group" ]
        [ label [] [ text parameter.name ]
        , div [ class "form-check py-0 my-0" ]
            [ label [ class "form-check-label form-check-toggle" ]
                [ input
                    [ class "form-check-input"
                    , onCheck (FieldInputBool path)
                    , type_ "checkbox"
                    , checked (fieldValue == "True")
                    ]
                    []
                , span [] []
                ]
            ]
        ]
