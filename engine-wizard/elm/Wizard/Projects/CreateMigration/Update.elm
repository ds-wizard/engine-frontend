module Wizard.Projects.CreateMigration.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Form
import Form.Field as Field
import Gettext exposing (gettext)
import Maybe.Extra as Maybe
import Shared.Api.KnowledgeModels as KnowledgeModelsApi
import Shared.Api.Packages as PackagesApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.PackageDetail as PackageDetail exposing (PackageDetail)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireMigration exposing (QuestionnaireMigration)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Setters exposing (setQuestionnaire, setSelected)
import Shared.Utils exposing (withNoCmd)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Msgs
import Wizard.Projects.Common.QuestionnaireMigrationCreateForm as QuestionnaireMigrationCreateForm
import Wizard.Projects.CreateMigration.Models exposing (Model)
import Wizard.Projects.CreateMigration.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireCompleted


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


handleGetQuestionnaireCompleted : AppState -> (Msg -> Wizard.Msgs.Msg) -> Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireCompleted appState wrapMsg model result =
    loadCurrentPackage appState wrapMsg <|
        applyResult appState
            { setResult = setQuestionnaire
            , defaultError = gettext "Unable to get the project." appState.locale
            , model = model
            , result = result
            , logoutMsg = Wizard.Msgs.logoutMsg
            }


handleGetCurrentPackageCompleted : AppState -> (Msg -> Wizard.Msgs.Msg) -> Model -> Result ApiError PackageDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetCurrentPackageCompleted appState wrapMsg model result =
    let
        setResult r m =
            { m | currentPackage = r, selectedPackageDetail = r }
    in
    preselectKnowledgeModel appState wrapMsg <|
        applyResult appState
            { setResult = setResult
            , defaultError = gettext "Unable to get the knowledge model." appState.locale
            , model = model
            , result = result
            , logoutMsg = Wizard.Msgs.logoutMsg
            }


handleGetSelectedPackageCompleted : AppState -> (Msg -> Wizard.Msgs.Msg) -> Model -> Result ApiError PackageDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetSelectedPackageCompleted appState wrapMsg model result =
    let
        setResult r m =
            { m | selectedPackageDetail = r }
    in
    preselectKnowledgeModel appState wrapMsg <|
        applyResult appState
            { setResult = setResult
            , defaultError = gettext "Unable to get the knowledge model." appState.locale
            , model = model
            , result = result
            , logoutMsg = Wizard.Msgs.logoutMsg
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
                        QuestionnairesApi.fetchQuestionnaireMigration model.questionnaireUuid body appState PostMigrationCompleted
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
                            KnowledgeModelsApi.fetchPreview (Just packageId) [] [] appState GetKnowledgeModelPreviewCompleted
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
                PackagesApi.getPackageWithoutDeprecatedVersions package.id appState GetSelectedPackageCompleted
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
            , getResultCmd Wizard.Msgs.logoutMsg result
            )


handleGetKnowledgeModelPreviewCompleted : AppState -> Model -> Result ApiError KnowledgeModel -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetKnowledgeModelPreviewCompleted appState model result =
    let
        newModel =
            case result of
                Ok knowledgeModel ->
                    { model | knowledgeModelPreview = Success knowledgeModel }

                Err error ->
                    { model | knowledgeModelPreview = ApiError.toActionResult appState (gettext "Unable to get question tags for the Knowledge Model." appState.locale) error }

        cmd =
            getResultCmd Wizard.Msgs.logoutMsg result
    in
    ( newModel, cmd )


handlePackageTypeHintInputMsg : (Msg -> Wizard.Msgs.Msg) -> TypeHintInput.Msg PackageSuggestion -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handlePackageTypeHintInputMsg wrapMsg typeHintInputMsg appState model =
    let
        cfg =
            { wrapMsg = wrapMsg << PackageTypeHintInputMsg
            , getTypeHints = PackagesApi.getPackagesSuggestions
            , getError = gettext "Unable to get Knowledge Models." appState.locale
            , setReply = wrapMsg << SelectPackage
            , clearReply = Nothing
            , filterResults = Nothing
            }

        ( packageTypeHintInputModel, cmd ) =
            TypeHintInput.update cfg typeHintInputMsg appState model.packageTypeHintInputModel
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
                        PackagesApi.getPackageWithoutDeprecatedVersions questionnaire.package.id appState GetCurrentPackageCompleted
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
                                KnowledgeModelsApi.fetchPreview (Just latestPackageId) [] [] appState GetKnowledgeModelPreviewCompleted
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
