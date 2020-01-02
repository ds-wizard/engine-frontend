module Wizard.Questionnaires.Index.ExportModal.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.Api.Templates as TemplatesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Questionnaires.Common.Template exposing (Template)
import Wizard.Questionnaires.Index.ExportModal.Models exposing (Model, initialModel)
import Wizard.Questionnaires.Index.ExportModal.Msgs exposing (Msg(..))
import Wizard.Utils exposing (withNoCmd)


fetchData : AppState -> Cmd Msg
fetchData appState =
    TemplatesApi.getTemplates appState GetTemplatesCompleted


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
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


handleGetTemplatesCompleted : AppState -> Model -> Result ApiError (List Template) -> ( Model, Cmd Wizard.Msgs.Msg )
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
            ( { model | templates = ApiError.toActionResult (lg "apiError.templates.getListError" appState) error }
            , getResultCmd result
            )


handleClose : ( Model, Cmd Wizard.Msgs.Msg )
handleClose =
    withNoCmd initialModel


handleSelectFormat : Model -> String -> ( Model, Cmd Wizard.Msgs.Msg )
handleSelectFormat model format =
    withNoCmd <|
        { model | selectedFormat = format }


handleSelectTemplate : Model -> String -> ( Model, Cmd Wizard.Msgs.Msg )
handleSelectTemplate model template =
    withNoCmd <|
        { model | selectedTemplate = Just template }
