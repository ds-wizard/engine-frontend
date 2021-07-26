module Wizard.Common.Components.Questionnaire.VersionModal exposing
    ( Model
    , Msg
    , init
    , setEventUuid
    , setVersion
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.QuestionnaireVersion exposing (QuestionnaireVersion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Locale exposing (l, lh, lx)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire.VersionForm as VersionForm exposing (VersionForm)
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Modal as Modal


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Components.Questionnaire.VersionModal"



-- MODEL


type alias Model =
    { form : Form FormError VersionForm
    , mbEventUuid : Maybe Uuid
    , mbQuestionnaireVersion : Maybe QuestionnaireVersion
    , versionResult : ActionResult ()
    }


init : Model
init =
    { form = VersionForm.initEmpty
    , mbEventUuid = Nothing
    , mbQuestionnaireVersion = Nothing
    , versionResult = Unset
    }


setEventUuid : Uuid -> Model -> Model
setEventUuid eventUuid model =
    { model
        | mbEventUuid = Just eventUuid
        , mbQuestionnaireVersion = Nothing
        , versionResult = Unset
        , form = VersionForm.initEmpty
    }


setVersion : QuestionnaireVersion -> Model -> Model
setVersion version model =
    { model
        | mbEventUuid = Just version.eventUuid
        , mbQuestionnaireVersion = Just version
        , versionResult = Unset
        , form = VersionForm.init version
    }



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | PostVersionComplete (Result ApiError QuestionnaireVersion)
    | PutVersionComplete (Result ApiError QuestionnaireVersion)
    | Close


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , questionnaireUuid : Uuid
    , addVersionCmd : QuestionnaireVersion -> Cmd msg
    , renameVersionCmd : QuestionnaireVersion -> Cmd msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form, model.mbEventUuid ) of
                ( Form.Submit, Just versionForm, Just eventUuid ) ->
                    let
                        body =
                            VersionForm.encode eventUuid versionForm

                        cmd =
                            case model.mbQuestionnaireVersion of
                                Just version ->
                                    QuestionnairesApi.putVersion cfg.questionnaireUuid version.uuid body appState PutVersionComplete

                                Nothing ->
                                    QuestionnairesApi.postVersion cfg.questionnaireUuid body appState PostVersionComplete
                    in
                    ( { model
                        | form = Form.update VersionForm.validation formMsg model.form
                        , versionResult = Loading
                      }
                    , Cmd.map cfg.wrapMsg cmd
                    )

                _ ->
                    ( { model
                        | form = Form.update VersionForm.validation formMsg model.form
                      }
                    , Cmd.none
                    )

        PostVersionComplete result ->
            case result of
                Ok newVersion ->
                    ( { model | mbEventUuid = Nothing }
                    , cfg.addVersionCmd newVersion
                    )

                Err error ->
                    ( { model | versionResult = ApiError.toActionResult appState "Unable to create version" error }
                    , Cmd.none
                    )

        PutVersionComplete result ->
            case result of
                Ok newVersion ->
                    ( { model | mbEventUuid = Nothing }
                    , cfg.renameVersionCmd newVersion
                    )

                Err error ->
                    ( { model | versionResult = ApiError.toActionResult appState "Unable to update version" error }
                    , Cmd.none
                    )

        Close ->
            ( { model | mbEventUuid = Nothing }, Cmd.none )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        form =
            [ Html.map FormMsg <| FormGroup.input appState model.form "name" (l_ "form.name" appState)
            , Html.map FormMsg <| FormGroup.textarea appState model.form "description" (l_ "form.description" appState)
            ]

        modalTitle =
            case model.mbQuestionnaireVersion of
                Just _ ->
                    l_ "title.rename" appState

                Nothing ->
                    l_ "title.new" appState
    in
    Modal.confirm appState
        { modalTitle = modalTitle
        , modalContent = form
        , visible = Maybe.isJust model.mbEventUuid
        , actionResult = ActionResult.map (always "") model.versionResult
        , actionName = l_ "action" appState
        , actionMsg = FormMsg Form.Submit
        , cancelMsg = Just Close
        , dangerous = False
        , dataCy = "project-version"
        }
