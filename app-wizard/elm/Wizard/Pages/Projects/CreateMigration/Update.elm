module Wizard.Pages.Projects.CreateMigration.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Ports.Window as Window
import Common.Utils.CmdUtils exposing (withNoCmd)
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setSelected)
import Form
import Form.Field as Field
import Gettext exposing (gettext)
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.KnowledgeModels as KnowledgeModelsApi
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.PackageDetail as PackageDetail exposing (PackageDetail)
import Wizard.Api.Models.PackageSuggestion exposing (PackageSuggestion)
import Wizard.Api.Models.QuestionnaireDetailWrapper exposing (QuestionnaireDetailWrapper)
import Wizard.Api.Models.QuestionnaireMigration exposing (QuestionnaireMigration)
import Wizard.Api.Models.QuestionnaireSettings exposing (QuestionnaireSettings)
import Wizard.Api.Packages as PackagesApi
import Wizard.Api.Questionnaires as QuestionnairesApi
import Wizard.Components.TypeHintInput as TypeHintInput
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Projects.Common.QuestionnaireMigrationCreateForm as QuestionnaireMigrationCreateForm
import Wizard.Pages.Projects.CreateMigration.Models exposing (Model)
import Wizard.Pages.Projects.CreateMigration.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    QuestionnairesApi.getQuestionnaireSettings appState uuid GetQuestionnaireCompleted


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        AddTag tagUuid ->
            handleAddTag model tagUuid

        RemoveTag tagUuid ->
            handleRemoveTag model tagUuid

        ChangeUseAllQuestions value ->
            ( { model | useAllQuestions = value }, Cmd.none )

        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted appState wrapMsg model result

        Cancel ->
            ( model, Window.historyBack (Routing.toUrl (Routes.projectsIndex appState)) )

        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        SelectPackage package ->
            handleSelectPackage wrapMsg appState model package

        PostMigrationCompleted result ->
            handlePostMigrationCompleted appState model result

        GetKnowledgeModelPreviewCompleted result ->
            handleGetKnowledgeModelPreviewCompleted appState model result

        GetCurrentPackageCompleted result ->
            handleGetCurrentPackageCompleted appState wrapMsg model result

        GetSelectedPackageCompleted result ->
            handleGetSelectedPackageCompleted appState wrapMsg model result

        PackageTypeHintInputMsg typeHintInputMsg ->
            handlePackageTypeHintInputMsg wrapMsg typeHintInputMsg appState model



-- Handlers


handleAddTag : Model -> String -> ( Model, Cmd Wizard.Msgs.Msg )
handleAddTag model tagUuid =
    withNoCmd <|
        { model | selectedTags = tagUuid :: model.selectedTags }


handleRemoveTag : Model -> String -> ( Model, Cmd Wizard.Msgs.Msg )
handleRemoveTag model tagUuid =
    withNoCmd <|
        { model | selectedTags = List.filter (\t -> t /= tagUuid) model.selectedTags }


handleGetQuestionnaireCompleted : AppState -> (Msg -> Wizard.Msgs.Msg) -> Model -> Result ApiError (QuestionnaireDetailWrapper QuestionnaireSettings) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireCompleted appState wrapMsg model result =
    let
        setResult : ActionResult (QuestionnaireDetailWrapper QuestionnaireSettings) -> Model -> Model
        setResult q m =
            case q of
                Success questionnaire ->
                    { m
                        | questionnaire = Success questionnaire.data
                        , selectedTags = questionnaire.data.selectedQuestionTagUuids
                        , useAllQuestions = List.isEmpty questionnaire.data.selectedQuestionTagUuids
                    }

                _ ->
                    { m | questionnaire = ActionResult.map .data q }
    in
    loadCurrentPackage appState wrapMsg <|
        RequestHelpers.applyResult
            { setResult = setResult
            , defaultError = gettext "Unable to get the project." appState.locale
            , model = model
            , result = result
            , logoutMsg = Wizard.Msgs.logoutMsg
            , locale = appState.locale
            }


handleGetCurrentPackageCompleted : AppState -> (Msg -> Wizard.Msgs.Msg) -> Model -> Result ApiError PackageDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetCurrentPackageCompleted appState wrapMsg model result =
    let
        setResult r m =
            { m | currentPackage = r, selectedPackageDetail = r }
    in
    preselectKnowledgeModel appState wrapMsg <|
        RequestHelpers.applyResult
            { setResult = setResult
            , defaultError = gettext "Unable to get the knowledge model." appState.locale
            , model = model
            , result = result
            , logoutMsg = Wizard.Msgs.logoutMsg
            , locale = appState.locale
            }


handleGetSelectedPackageCompleted : AppState -> (Msg -> Wizard.Msgs.Msg) -> Model -> Result ApiError PackageDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetSelectedPackageCompleted appState wrapMsg model result =
    let
        setResult r m =
            { m | selectedPackageDetail = r }
    in
    preselectKnowledgeModel appState wrapMsg <|
        RequestHelpers.applyResult
            { setResult = setResult
            , defaultError = gettext "Unable to get the knowledge model." appState.locale
            , model = model
            , result = result
            , logoutMsg = Wizard.Msgs.logoutMsg
            , locale = appState.locale
            }


handleForm : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                selectedTags =
                    if model.useAllQuestions then
                        []

                    else
                        model.selectedTags

                body =
                    QuestionnaireMigrationCreateForm.encode selectedTags form

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.fetchQuestionnaireMigration appState model.questionnaireUuid body PostMigrationCompleted
            in
            ( { model | savingMigration = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update QuestionnaireMigrationCreateForm.validation formMsg model.form }
            in
            case getSelectedPackageId newModel of
                Just packageId ->
                    if needFetchKnowledgeModelPreview model packageId then
                        ( { newModel
                            | lastFetchedPreview = Just packageId
                            , knowledgeModelPreview = Loading
                            , selectedTags = []
                          }
                        , Cmd.map wrapMsg <|
                            KnowledgeModelsApi.fetchPreview appState (Just packageId) [] [] GetKnowledgeModelPreviewCompleted
                        )

                    else
                        ( newModel, Cmd.none )

                Nothing ->
                    ( { newModel | knowledgeModelPreview = Unset, selectedTags = [] }, Cmd.none )


handleSelectPackage : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> PackageSuggestion -> ( Model, Cmd Wizard.Msgs.Msg )
handleSelectPackage wrapMsg appState model package =
    let
        formMsg =
            Form.Input "packageId" Form.Select Field.EmptyField

        getSelectedPackageCmd =
            Cmd.map wrapMsg <|
                PackagesApi.getPackageWithoutDeprecatedVersions appState package.id GetSelectedPackageCompleted
    in
    ( { model
        | selectedPackage = Just package
        , selectedPackageDetail = Loading
        , knowledgeModelPreview = Unset
        , selectedTags = []
        , form = Form.update QuestionnaireMigrationCreateForm.validation formMsg model.form
      }
    , getSelectedPackageCmd
    )


handlePostMigrationCompleted : AppState -> Model -> Result ApiError QuestionnaireMigration -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostMigrationCompleted appState model result =
    case result of
        Ok migration ->
            ( model, cmdNavigate appState <| Routes.projectsMigration migration.newQuestionnaire.uuid )

        Err error ->
            ( { model | savingMigration = ApiError.toActionResult appState (gettext "Project migration could not be created." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handleGetKnowledgeModelPreviewCompleted : AppState -> Model -> Result ApiError KnowledgeModel -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetKnowledgeModelPreviewCompleted appState model result =
    let
        newModel =
            case result of
                Ok knowledgeModel ->
                    { model | knowledgeModelPreview = Success knowledgeModel }

                Err error ->
                    { model | knowledgeModelPreview = ApiError.toActionResult appState (gettext "Unable to get question tags for the knowledge model." appState.locale) error }

        cmd =
            RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
    in
    ( newModel, cmd )


handlePackageTypeHintInputMsg : (Msg -> Wizard.Msgs.Msg) -> TypeHintInput.Msg PackageSuggestion -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handlePackageTypeHintInputMsg wrapMsg typeHintInputMsg appState model =
    let
        cfg =
            { wrapMsg = wrapMsg << PackageTypeHintInputMsg
            , getTypeHints = PackagesApi.getPackagesSuggestions appState Nothing
            , getError = gettext "Unable to get knowledge models." appState.locale
            , setReply = wrapMsg << SelectPackage
            , clearReply = Nothing
            , filterResults = Nothing
            }

        ( packageTypeHintInputModel, cmd ) =
            TypeHintInput.update cfg typeHintInputMsg model.packageTypeHintInputModel
    in
    ( { model | packageTypeHintInputModel = packageTypeHintInputModel }, cmd )



-- Helpers


loadCurrentPackage : AppState -> (Msg -> Wizard.Msgs.Msg) -> ( Model, Cmd Wizard.Msgs.Msg ) -> ( Model, Cmd Wizard.Msgs.Msg )
loadCurrentPackage appState wrapMsg ( model, cmd ) =
    case model.questionnaire of
        Success questionnaire ->
            let
                getCurrentPackageCmd =
                    Cmd.map wrapMsg <|
                        PackagesApi.getPackageWithoutDeprecatedVersions appState questionnaire.package.id GetCurrentPackageCompleted
            in
            ( model, Cmd.batch [ cmd, getCurrentPackageCmd ] )

        _ ->
            ( model, cmd )


preselectKnowledgeModel : AppState -> (Msg -> Wizard.Msgs.Msg) -> ( Model, Cmd Wizard.Msgs.Msg ) -> ( Model, Cmd Wizard.Msgs.Msg )
preselectKnowledgeModel appState wrapMsg ( model, cmd ) =
    case model.selectedPackageDetail of
        Success package ->
            let
                mbLatestPackageId =
                    PackageDetail.getLatestPackageId package

                ( packageCmd, lastFetchedPreview ) =
                    case mbLatestPackageId of
                        Just latestPackageId ->
                            ( Cmd.map wrapMsg <|
                                KnowledgeModelsApi.fetchPreview appState (Just latestPackageId) [] [] GetKnowledgeModelPreviewCompleted
                            , Just latestPackageId
                            )

                        Nothing ->
                            ( Cmd.none, model.lastFetchedPreview )

                form =
                    Maybe.unwrap
                        QuestionnaireMigrationCreateForm.initEmpty
                        QuestionnaireMigrationCreateForm.init
                        mbLatestPackageId

                packageSuggestion =
                    PackageDetail.toPackageSuggestion package
            in
            ( { model
                | selectedPackage = Just packageSuggestion
                , form = form
                , packageTypeHintInputModel = setSelected (Just packageSuggestion) model.packageTypeHintInputModel
                , lastFetchedPreview = lastFetchedPreview
              }
            , Cmd.batch [ cmd, packageCmd ]
            )

        _ ->
            ( model, cmd )


getSelectedPackageId : Model -> Maybe String
getSelectedPackageId model =
    (Form.getFieldAsString "packageId" model.form).value


needFetchKnowledgeModelPreview : Model -> String -> Bool
needFetchKnowledgeModelPreview model packageId =
    model.lastFetchedPreview /= Just packageId
