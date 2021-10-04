module Wizard.Admin.Operations.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Dict
import Shared.Api.Admin as AdminApi
import Shared.Data.AdminOperationSection as AdminOperationSection
import Wizard.Admin.Operations.Models exposing (Model, fieldPath, getSection, operationPath)
import Wizard.Admin.Operations.Msgs exposing (Msg(..))
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs


fetchData : AppState -> Cmd Msg
fetchData appState =
    AdminApi.getOperations appState GetAdminOperationsComplete


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetAdminOperationsComplete result ->
            let
                ( newModel, cmd ) =
                    applyResult appState
                        { setResult = \s m -> { m | adminOperationSections = s }
                        , defaultError = "Unable to get admin operations."
                        , model = model
                        , result = result
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
                                AdminApi.executeOperation execution appState (ExecuteOperationComplete sectionName operationName)

                        operationResults =
                            Dict.insert (operationPath sectionName operationName) Loading model.operationResults
                    in
                    ( { model | operationResults = operationResults }, cmd )

                Nothing ->
                    ( model, Cmd.none )

        ExecuteOperationComplete sectionName operationName result ->
            applyResult appState
                { setResult = \r m -> { m | operationResults = Dict.insert (operationPath sectionName operationName) r m.operationResults }
                , defaultError = "Execution failed."
                , model = model
                , result = result
                }
