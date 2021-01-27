module Wizard.Common.Components.Questionnaire.DeleteVersionModal exposing
    ( Model
    , Msg
    , init
    , setVersion
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, p, strong, text)
import Maybe.Extra as Maybe
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.QuestionnaireVersion exposing (QuestionnaireVersion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lh, lx)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Modal as Modal


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Components.Questionnaire.DeleteVersionModal"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Common.Components.Questionnaire.DeleteVersionModal"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Components.Questionnaire.DeleteVersionModal"



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
                        QuestionnairesApi.deleteVersion cfg.questionnaireUuid version.uuid appState DeleteComplete
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
                    [ p [] (lh_ "deleteMessage" [ strong [] [ text <| "\"" ++ version.name ++ "\"" ] ] appState)
                    , p [] [ lx_ "deleteInfo" appState ]
                    ]

                Nothing ->
                    []
    in
    Modal.confirm appState
        { modalTitle = l_ "title" appState
        , modalContent = content
        , visible = Maybe.isJust model.mbQuestionnaireVersion
        , actionResult = ActionResult.map (always "") model.deleteResult
        , actionName = l_ "action" appState
        , actionMsg = Delete
        , cancelMsg = Just Close
        , dangerous = True
        }
