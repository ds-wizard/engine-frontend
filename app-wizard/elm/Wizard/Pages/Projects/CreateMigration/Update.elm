module Wizard.Pages.Projects.CreateMigration.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.TypeHintInput as TypeHintInput
import Common.Ports.Window as Window
import Common.Utils.CmdUtils exposing (withNoCmd)
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setSelected)
import Form
import Form.Field as Field
import Gettext exposing (gettext)
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Wizard.Api.KnowledgeModels as KnowledgeModelsApi
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModelPackageDetail as KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Wizard.Api.Models.ProjectMigration exposing (ProjectMigration)
import Wizard.Api.Models.ProjectSettings exposing (ProjectSettings)
import Wizard.Api.Projects as ProjectsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Projects.Common.ProjectMigrationCreateForm as ProjectMigrationCreateForm
import Wizard.Pages.Projects.CreateMigration.Models exposing (Model)
import Wizard.Pages.Projects.CreateMigration.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    ProjectsApi.getSettings appState uuid GetQuestionnaireCompleted


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
            handleGetProjectCompleted appState wrapMsg model result

        Cancel ->
            ( model, Window.historyBack (Routing.toUrl (Routes.projectsIndex appState)) )

        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        SelectKnowledgeModelPackage kmPackage ->
            handleSelectPackage wrapMsg appState model kmPackage

        PostMigrationCompleted result ->
            handlePostMigrationCompleted appState model result

        GetKnowledgeModelPreviewCompleted result ->
            handleGetKnowledgeModelPreviewCompleted appState model result

        GetCurrentKnowledgeModelPackageCompleted result ->
            handleGetCurrentPackageCompleted appState wrapMsg model result

        GetSelectedKnowledgeModelPackageCompleted result ->
            handleGetSelectedPackageCompleted appState wrapMsg model result

        KnowledgeModelPackageTypeHintInputMsg typeHintInputMsg ->
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


handleGetProjectCompleted : AppState -> (Msg -> Wizard.Msgs.Msg) -> Model -> Result ApiError (ProjectDetailWrapper ProjectSettings) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetProjectCompleted appState wrapMsg model result =
    let
        setResult : ActionResult (ProjectDetailWrapper ProjectSettings) -> Model -> Model
        setResult q m =
            case q of
                Success project ->
                    { m
                        | project = Success project.data
                        , selectedTags = project.data.selectedQuestionTagUuids
                        , useAllQuestions = List.isEmpty project.data.selectedQuestionTagUuids
                    }

                _ ->
                    { m | project = ActionResult.map .data q }
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


handleGetCurrentPackageCompleted : AppState -> (Msg -> Wizard.Msgs.Msg) -> Model -> Result ApiError KnowledgeModelPackageDetail -> ( Model, Cmd Wizard.Msgs.Msg )
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


handleGetSelectedPackageCompleted : AppState -> (Msg -> Wizard.Msgs.Msg) -> Model -> Result ApiError KnowledgeModelPackageDetail -> ( Model, Cmd Wizard.Msgs.Msg )
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
                    ProjectMigrationCreateForm.encode selectedTags form

                cmd =
                    Cmd.map wrapMsg <|
                        ProjectsApi.fetchMigration appState model.projectUuid body PostMigrationCompleted
            in
            ( { model | savingMigration = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update ProjectMigrationCreateForm.validation formMsg model.form }
            in
            case getSelectedPackageId newModel of
                Just kmPackageId ->
                    if needFetchKnowledgeModelPreview model kmPackageId then
                        ( { newModel
                            | lastFetchedPreview = Just kmPackageId
                            , knowledgeModelPreview = Loading
                            , selectedTags = []
                          }
                        , Cmd.map wrapMsg <|
                            KnowledgeModelsApi.fetchPreview appState (Just kmPackageId) [] [] GetKnowledgeModelPreviewCompleted
                        )

                    else
                        ( newModel, Cmd.none )

                Nothing ->
                    ( { newModel | knowledgeModelPreview = Unset, selectedTags = [] }, Cmd.none )


handleSelectPackage : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> KnowledgeModelPackageSuggestion -> ( Model, Cmd Wizard.Msgs.Msg )
handleSelectPackage wrapMsg appState model kmPackage =
    let
        formMsg =
            Form.Input "knowledgeModelPackageId" Form.Select Field.EmptyField

        getSelectedPackageCmd =
            Cmd.map wrapMsg <|
                KnowledgeModelPackagesApi.getKnowledgeModelPackageWithoutDeprecatedVersions appState kmPackage.id GetSelectedKnowledgeModelPackageCompleted
    in
    ( { model
        | selectedPackage = Just kmPackage
        , selectedPackageDetail = Loading
        , knowledgeModelPreview = Unset
        , selectedTags = []
        , form = Form.update ProjectMigrationCreateForm.validation formMsg model.form
      }
    , getSelectedPackageCmd
    )


handlePostMigrationCompleted : AppState -> Model -> Result ApiError ProjectMigration -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostMigrationCompleted appState model result =
    case result of
        Ok migration ->
            ( model, cmdNavigate appState <| Routes.projectsMigration migration.newProject.uuid )

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


handlePackageTypeHintInputMsg : (Msg -> Wizard.Msgs.Msg) -> TypeHintInput.Msg KnowledgeModelPackageSuggestion -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handlePackageTypeHintInputMsg wrapMsg typeHintInputMsg appState model =
    let
        cfg =
            { wrapMsg = wrapMsg << KnowledgeModelPackageTypeHintInputMsg
            , getTypeHints = KnowledgeModelPackagesApi.getKnowledgeModelPackagesSuggestions appState Nothing
            , getError = gettext "Unable to get knowledge models." appState.locale
            , setReply = wrapMsg << SelectKnowledgeModelPackage
            , clearReply = Nothing
            , filterResults = Nothing
            }

        ( packageTypeHintInputModel, cmd ) =
            TypeHintInput.update cfg typeHintInputMsg model.knowledgeModelPackageTypeHintInputModel
    in
    ( { model | knowledgeModelPackageTypeHintInputModel = packageTypeHintInputModel }, cmd )



-- Helpers


loadCurrentPackage : AppState -> (Msg -> Wizard.Msgs.Msg) -> ( Model, Cmd Wizard.Msgs.Msg ) -> ( Model, Cmd Wizard.Msgs.Msg )
loadCurrentPackage appState wrapMsg ( model, cmd ) =
    case model.project of
        Success project ->
            let
                getCurrentPackageCmd =
                    Cmd.map wrapMsg <|
                        KnowledgeModelPackagesApi.getKnowledgeModelPackageWithoutDeprecatedVersions appState project.knowledgeModelPackage.id GetCurrentKnowledgeModelPackageCompleted
            in
            ( model, Cmd.batch [ cmd, getCurrentPackageCmd ] )

        _ ->
            ( model, cmd )


preselectKnowledgeModel : AppState -> (Msg -> Wizard.Msgs.Msg) -> ( Model, Cmd Wizard.Msgs.Msg ) -> ( Model, Cmd Wizard.Msgs.Msg )
preselectKnowledgeModel appState wrapMsg ( model, cmd ) =
    case model.selectedPackageDetail of
        Success kmPackage ->
            let
                mbLatestPackageId =
                    KnowledgeModelPackageDetail.getLatestPackageId kmPackage

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
                        ProjectMigrationCreateForm.initEmpty
                        ProjectMigrationCreateForm.init
                        mbLatestPackageId

                packageSuggestion =
                    KnowledgeModelPackageDetail.toPackageSuggestion kmPackage
            in
            ( { model
                | selectedPackage = Just packageSuggestion
                , form = form
                , knowledgeModelPackageTypeHintInputModel = setSelected (Just packageSuggestion) model.knowledgeModelPackageTypeHintInputModel
                , lastFetchedPreview = lastFetchedPreview
              }
            , Cmd.batch [ cmd, packageCmd ]
            )

        _ ->
            ( model, cmd )


getSelectedPackageId : Model -> Maybe String
getSelectedPackageId model =
    (Form.getFieldAsString "knowledgeModelPackageId" model.form).value


needFetchKnowledgeModelPreview : Model -> String -> Bool
needFetchKnowledgeModelPreview model kmPackageId =
    model.lastFetchedPreview /= Just kmPackageId
