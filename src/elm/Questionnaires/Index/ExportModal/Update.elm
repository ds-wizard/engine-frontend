module Questionnaires.Index.ExportModal.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Templates as TemplatesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Msgs
import Questionnaires.Common.Template exposing (Template)
import Questionnaires.Index.ExportModal.Models exposing (Model, initialModel)
import Questionnaires.Index.ExportModal.Msgs exposing (Msg(..))
import Utils exposing (withNoCmd)


fetchData : AppState -> Cmd Msg
fetchData appState =
    TemplatesApi.getTemplates appState GetTemplatesCompleted


update : Msg -> Model -> ( Model, Cmd Msgs.Msg )
update msg model =
    case msg of
        GetTemplatesCompleted result ->
            handleGetTemplatesCompleted model result

        Close ->
            handleClose

        SelectFormat format ->
            handleSelectFormat model format

        SelectTemplate template ->
            handleSelectTemplate model template



-- Handlers


handleGetTemplatesCompleted : Model -> Result ApiError (List Template) -> ( Model, Cmd Msgs.Msg )
handleGetTemplatesCompleted model result =
    case result of
        Ok templates ->
            ( { model
                | templates = Success templates
                , selectedTemplate = Maybe.map .uuid <| List.head templates
              }
            , Cmd.none
            )

        Err error ->
            ( { model | templates = getServerError error "DMP Templates could not be loaded" }
            , getResultCmd result
            )


handleClose : ( Model, Cmd Msgs.Msg )
handleClose =
    withNoCmd initialModel


handleSelectFormat : Model -> String -> ( Model, Cmd Msgs.Msg )
handleSelectFormat model format =
    withNoCmd <|
        { model | selectedFormat = format }


handleSelectTemplate : Model -> String -> ( Model, Cmd Msgs.Msg )
handleSelectTemplate model template =
    withNoCmd <|
        { model | selectedTemplate = Just template }
