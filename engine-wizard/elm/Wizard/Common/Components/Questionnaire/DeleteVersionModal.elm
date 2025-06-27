module Wizard.Common.Components.Questionnaire.DeleteVersionModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , setVersion
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, p, strong, text)
import Maybe.Extra as Maybe
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils exposing (flip)
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireVersion exposing (QuestionnaireVersion)
import Wizard.Api.Questionnaires as QuestionnairesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Modal as Modal



-- MODEL


type alias Model =
    { mbQuestionnaireVersion : Maybe QuestionnaireVersion
    , deleteResult : ActionResult ()
    }


init : Model
init =
    { mbQuestionnaireVersion = Nothing
    , deleteResult = Unset
    }


setVersion : QuestionnaireVersion -> Model -> Model
setVersion version model =
    { model
        | mbQuestionnaireVersion = Just version
        , deleteResult = Unset
    }



-- UPDATE


type Msg
    = Delete
    | DeleteComplete (Result ApiError ())
    | Close


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , questionnaireUuid : Uuid
    , deleteVersionCmd : QuestionnaireVersion -> Cmd msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Delete ->
            case model.mbQuestionnaireVersion of
                Just version ->
                    ( { model
                        | deleteResult = Loading
                      }
                    , Cmd.map cfg.wrapMsg <|
                        QuestionnairesApi.deleteVersion appState cfg.questionnaireUuid version.uuid DeleteComplete
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteComplete result ->
            case model.mbQuestionnaireVersion of
                Just version ->
                    case result of
                        Ok _ ->
                            ( { model | mbQuestionnaireVersion = Nothing }
                            , cfg.deleteVersionCmd version
                            )

                        Err error ->
                            ( { model | deleteResult = ApiError.toActionResult appState "Unable to delete version" error }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        Close ->
            ( { model | mbQuestionnaireVersion = Nothing }, Cmd.none )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.mbQuestionnaireVersion of
                Just version ->
                    let
                        deleteMessage =
                            gettext "Are you sure you want to delete version %s?" appState.locale
                                |> flip String.formatHtml [ strong [] [ text <| "\"" ++ version.name ++ "\"" ] ]
                    in
                    [ p [] deleteMessage
                    , p [] [ text (gettext "(Your answers will remain unchanged)" appState.locale) ]
                    ]

                Nothing ->
                    []

        cfg =
            Modal.confirmConfig (gettext "Delete version" appState.locale)
                |> Modal.confirmConfigContent content
                |> Modal.confirmConfigVisible (Maybe.isJust model.mbQuestionnaireVersion)
                |> Modal.confirmConfigActionResult (ActionResult.map (always "") model.deleteResult)
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) Delete
                |> Modal.confirmConfigCancelMsg Close
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "project-delete-version"
    in
    Modal.confirm appState cfg
