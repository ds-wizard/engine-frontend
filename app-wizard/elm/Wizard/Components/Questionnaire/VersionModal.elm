module Wizard.Components.Questionnaire.VersionModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , setEventUuid
    , setVersion
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Shared.Components.FormGroup as FormGroup
import Shared.Components.Modal as Modal
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireVersion exposing (QuestionnaireVersion)
import Wizard.Api.Questionnaires as QuestionnairesApi
import Wizard.Components.Questionnaire.VersionForm as VersionForm exposing (VersionForm)
import Wizard.Data.AppState exposing (AppState)



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
                                    QuestionnairesApi.putVersion appState cfg.questionnaireUuid version.uuid body PutVersionComplete

                                Nothing ->
                                    QuestionnairesApi.postVersion appState cfg.questionnaireUuid body PostVersionComplete
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
            [ Html.map FormMsg <| FormGroup.input appState.locale model.form "name" (gettext "Name" appState.locale)
            , Html.map FormMsg <| FormGroup.textarea appState.locale model.form "description" (gettext "Description" appState.locale)
            ]

        modalTitle =
            case model.mbQuestionnaireVersion of
                Just _ ->
                    gettext "Rename version" appState.locale

                Nothing ->
                    gettext "New version" appState.locale

        cfg =
            Modal.confirmConfig modalTitle
                |> Modal.confirmConfigContent form
                |> Modal.confirmConfigVisible (Maybe.isJust model.mbEventUuid)
                |> Modal.confirmConfigActionResult (ActionResult.map (always "") model.versionResult)
                |> Modal.confirmConfigAction (gettext "Save" appState.locale) (FormMsg Form.Submit)
                |> Modal.confirmConfigCancelMsg Close
                |> Modal.confirmConfigDataCy "project-version"
    in
    Modal.confirm appState cfg
