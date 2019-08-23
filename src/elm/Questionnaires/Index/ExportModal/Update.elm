module Questionnaires.Index.ExportModal.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Templates as TemplatesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Locale exposing (lg)
import Msgs
import Questionnaires.Common.Template exposing (Template)
import Questionnaires.Index.ExportModal.Models exposing (Model, initialModel)
import Questionnaires.Index.ExportModal.Msgs exposing (Msg(..))
import Utils exposing (withNoCmd)


fetchData : AppState -> Cmd Msg
fetchData appState =
    TemplatesApi.getTemplates appState GetTemplatesCompleted


update : Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg appState model =
    case msg of
        GetTemplatesCompleted result ->
            handleGetTemplatesCompleted appState model result

        Close ->
            handleClose

        SelectFormat format ->
            handleSelectFormat model format

        SelectTemplate template ->
            handleSelectTemplate model template



-- Handlers


handleGetTemplatesCompleted : AppState -> Model -> Result ApiError (List Template) -> ( Model, Cmd Msgs.Msg )
handleGetTemplatesCompleted appState model result =
    case result of
        Ok templates ->
            ( { model
                | templates = Success templates
                , selectedTemplate = Maybe.map .uuid <| List.head templates
              }
            , Cmd.none
            )

        Err error ->
            ( { model | templates = getServerError error <| lg "apiError.templates.getListError" appState }
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
