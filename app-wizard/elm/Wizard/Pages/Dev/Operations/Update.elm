module Wizard.Pages.Dev.Operations.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.Models.DevOperationSection as AdminOperationSection
import Common.Components.TypeHintInput as TypeHintInput
import Common.Utils.RequestHelpers as RequestHelpers
import Dict
import Uuid
import Wizard.Api.DevOperations as DevOperationsApi
import Wizard.Api.Tenants as TenantsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Dev.Operations.Models exposing (Model, fieldPath, getSection, getTypeHintInputModel, operationPath)
import Wizard.Pages.Dev.Operations.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    DevOperationsApi.getOperations appState GetDevOperationsComplete


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetDevOperationsComplete result ->
            let
                ( newModel, cmd ) =
                    RequestHelpers.applyResult
                        { setResult = \s m -> { m | adminOperationSections = s }
                        , defaultError = "Unable to get dev operations."
                        , model = model
                        , result = result
                        , logoutMsg = Wizard.Msgs.logoutMsg
                        , locale = appState.locale
                        }

                openedSection =
                    newModel.adminOperationSections
                        |> ActionResult.toMaybe
                        |> Maybe.andThen List.head
                        |> Maybe.map .name
            in
            ( { newModel | openedSection = openedSection }, cmd )

        OpenSection sectionName ->
            ( { model | openedSection = Just sectionName }, Cmd.none )

        FieldInput path value ->
            ( { model | fieldValues = Dict.insert path value model.fieldValues }, Cmd.none )

        FieldInputBool path value ->
            let
                stringValue =
                    if value then
                        "True"

                    else
                        "False"
            in
            ( { model | fieldValues = Dict.insert path stringValue model.fieldValues }, Cmd.none )

        UpdateTypeHintInput path typeHintInputMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << UpdateTypeHintInput path
                    , getTypeHints = TenantsApi.getTenantSuggestions appState
                    , getError = "Unable to get tenants"
                    , setReply = wrapMsg << FieldInput path << Uuid.toString << .uuid
                    , clearReply = Just (wrapMsg <| FieldInput path "")
                    , filterResults = Nothing
                    }

                ( typeHintInputModel, cmd ) =
                    TypeHintInput.update updateConfig typeHintInputMsg (getTypeHintInputModel path model)
            in
            ( { model | typeHintInputModels = Dict.insert path typeHintInputModel model.typeHintInputModels }, cmd )

        ExecuteOperation sectionName operationName ->
            let
                mbOperation =
                    Maybe.andThen (AdminOperationSection.getOperation operationName) (getSection sectionName model)
            in
            case mbOperation of
                Just operation ->
                    let
                        getParameterValue parameter =
                            Maybe.withDefault "" <| Dict.get (fieldPath sectionName operationName parameter.name) model.fieldValues

                        execution =
                            { sectionName = sectionName
                            , operationName = operationName
                            , parameters = List.map getParameterValue operation.parameters
                            }

                        cmd =
                            Cmd.map wrapMsg <|
                                DevOperationsApi.executeOperation appState execution (ExecuteOperationComplete sectionName operationName)

                        operationResults =
                            Dict.insert (operationPath sectionName operationName) Loading model.operationResults
                    in
                    ( { model | operationResults = operationResults }, cmd )

                Nothing ->
                    ( model, Cmd.none )

        ExecuteOperationComplete sectionName operationName result ->
            RequestHelpers.applyResult
                { setResult = \r m -> { m | operationResults = Dict.insert (operationPath sectionName operationName) r m.operationResults }
                , defaultError = "Execution failed."
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }
