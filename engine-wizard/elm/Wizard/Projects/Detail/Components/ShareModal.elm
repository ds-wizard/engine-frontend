module Wizard.Projects.Detail.Components.ShareModal exposing
    ( Model
    , Msg
    , init
    , openMsg
    , setQuestionnaire
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Html exposing (Html, div, p, strong, text)
import Html.Attributes exposing (class, classList)
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.Permission exposing (Permission)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnairePermission as QuestionnairePermission
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg, lgh, lgx)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Modal as Modal
import Wizard.Ports as Ports
import Wizard.Projects.Common.QuestionnaireEditForm as QuestionnaireEditForm exposing (QuestionnaireEditForm)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Components.ShareModal"



-- MODEL


type alias Model =
    { visible : Bool
    , savingSharing : ActionResult String
    , questionnaireEditForm : Form FormError QuestionnaireEditForm
    }


init : Model
init =
    { visible = False
    , savingSharing = Unset
    , questionnaireEditForm = QuestionnaireEditForm.initEmpty
    }


setQuestionnaire : QuestionnaireDetail -> Model -> Model
setQuestionnaire questionnaire model =
    { model | questionnaireEditForm = QuestionnaireEditForm.init questionnaire }



-- UPDATE


type Msg
    = ShowHide Bool
    | FormMsg Form.Msg
    | PutQuestionnaireComplete (Result ApiError ())


openMsg : Msg
openMsg =
    ShowHide True


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , questionnaireUuid : Uuid
    , permissions : List Permission
    }


update : UpdateConfig msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update cfg msg appState model =
    case msg of
        ShowHide visible ->
            ( { model | visible = visible }, Cmd.none )

        FormMsg formMsg ->
            handleFormMsg cfg formMsg appState model

        PutQuestionnaireComplete result ->
            handlePutQuestionnaireComplete appState model result


handleFormMsg : UpdateConfig msg -> Form.Msg -> AppState -> Model -> ( Model, Cmd msg )
handleFormMsg cfg formMsg appState model =
    case ( formMsg, Form.getOutput model.questionnaireEditForm ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    QuestionnaireEditForm.encode cfg.permissions form

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        QuestionnairesApi.putQuestionnaire cfg.questionnaireUuid body appState PutQuestionnaireComplete
            in
            ( { model | savingSharing = Loading }
            , cmd
            )

        _ ->
            ( { model | questionnaireEditForm = Form.update QuestionnaireEditForm.validation formMsg model.questionnaireEditForm }
            , Cmd.none
            )


handlePutQuestionnaireComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd msg )
handlePutQuestionnaireComplete appState model result =
    case result of
        Ok _ ->
            ( { model | visible = False, savingSharing = Unset }, Ports.refresh () )

        Err error ->
            ( { model | savingSharing = ApiError.toActionResult appState (lg "apiError.questionnaires.putError" appState) error }
            , Cmd.none
            )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        modalContent =
            [ Html.map FormMsg <| formView appState model.questionnaireEditForm
            ]

        modalConfig =
            { modalTitle = l_ "title" appState
            , modalContent = modalContent
            , visible = model.visible
            , actionResult = model.savingSharing
            , actionName = l_ "action" appState
            , actionMsg = FormMsg Form.Submit
            , cancelMsg = Just <| ShowHide False
            , dangerous = False
            }
    in
    Modal.confirm appState modalConfig


formView : AppState -> Form FormError QuestionnaireEditForm -> Html Form.Msg
formView appState form =
    let
        visibilityEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "visibilityEnabled" form).value

        visibilityEnabledInput =
            if appState.config.questionnaire.questionnaireVisibility.enabled then
                FormGroup.toggle form "visibilityEnabled" (lg "questionnaire.visibility" appState)

            else
                emptyNode

        visibilityPermissionInput =
            if appState.config.questionnaire.questionnaireVisibility.enabled then
                div
                    [ class "form-group form-group-toggle-extra"
                    , classList [ ( "visible", visibilityEnabled ) ]
                    ]
                    (lgh "questionnaire.visibilityPermission" [ visibilitySelect ] appState)

            else
                emptyNode

        visibilitySelect =
            if (Form.getFieldAsString "sharingPermission" form).value == Just "edit" then
                strong [] [ lgx "questionnairePermission.edit" appState ]

            else
                FormExtra.inlineSelect (QuestionnairePermission.formOptions appState) form "visibilityPermission"

        sharingEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "sharingEnabled" form).value

        sharingEnabledInput =
            if appState.config.questionnaire.questionnaireSharing.enabled then
                FormGroup.toggle form "sharingEnabled" (lg "questionnaire.sharing" appState)

            else
                emptyNode

        sharingPermissionInput =
            if appState.config.questionnaire.questionnaireSharing.enabled then
                div
                    [ class "form-group form-group-toggle-extra"
                    , classList [ ( "visible", sharingEnabled ) ]
                    ]
                    (lgh "questionnaire.sharingPermission" [ sharingSelect ] appState)

            else
                emptyNode

        sharingSelect =
            FormExtra.inlineSelect (QuestionnairePermission.formOptions appState) form "sharingPermission"
    in
    div []
        [ visibilityEnabledInput
        , visibilityPermissionInput
        , sharingEnabledInput
        , sharingPermissionInput
        ]
