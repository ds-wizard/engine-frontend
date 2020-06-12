module Wizard.Questionnaires.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg, lgf)
import Shared.Utils exposing (dispatch)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.Api.Questionnaires as QuestionnairesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Msgs
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Update as DeleteQuestionnaireModal
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Index.Models exposing (Model)
import Wizard.Questionnaires.Index.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        DeleteQuestionnaireMigration uuid ->
            handleDeleteMigration wrapMsg appState model uuid

        DeleteQuestionnaireMigrationCompleted result ->
            handleDeleteMigrationCompleted wrapMsg appState model result

        CloneQuestionnaire questionnaire ->
            handleCloneQuestionnaire wrapMsg appState model questionnaire

        CloneQuestionnaireCompleted result ->
            handleCloneQuestionnaireCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

        DeleteQuestionnaireModalMsg modalMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << DeleteQuestionnaireModalMsg
                    , deleteCompleteCmd =
                        dispatch (wrapMsg <| ListingMsg ListingMsgs.Reload)
                    }

                ( deleteModalModel, cmd ) =
                    DeleteQuestionnaireModal.update updateConfig modalMsg appState model.deleteModalModel
            in
            ( { model | deleteModalModel = deleteModalModel }
            , cmd
            )


handleDeleteMigration : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> String -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteMigration wrapMsg appState model uuid =
    ( { model | deletingMigration = Loading }
    , QuestionnairesApi.deleteQuestionnaireMigration uuid appState (wrapMsg << DeleteQuestionnaireMigrationCompleted)
    )


handleDeleteMigrationCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteMigrationCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            let
                ( questionnaires, cmd ) =
                    Listing.update (listingUpdateConfig wrapMsg appState) appState ListingMsgs.Reload model.questionnaires
            in
            ( { model
                | deletingMigration = Success <| lg "apiSuccess.questionnaires.migration.delete" appState
                , questionnaires = questionnaires
              }
            , cmd
            )

        Err error ->
            ( { model | deletingMigration = ApiError.toActionResult (lg "apiError.questionnaires.migrations.deleteError" appState) error }
            , getResultCmd result
            )


handleCloneQuestionnaire : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Questionnaire -> ( Model, Cmd Wizard.Msgs.Msg )
handleCloneQuestionnaire wrapMsg appState model questionnaire =
    ( { model | cloningQuestionnaire = Loading }
    , QuestionnairesApi.cloneQuestionnaire questionnaire.uuid appState (wrapMsg << CloneQuestionnaireCompleted)
    )


handleCloneQuestionnaireCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError Questionnaire -> ( Model, Cmd Wizard.Msgs.Msg )
handleCloneQuestionnaireCompleted wrapMsg appState model result =
    case result of
        Ok questionnaire ->
            ( { model | cloningQuestionnaire = Success <| lgf "apiSuccess.questionnaires.clone" [ questionnaire.name ] appState }
            , Cmd.map wrapMsg fetchData
            )

        Err error ->
            ( { model | cloningQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.cloneError" appState) error }
            , getResultCmd result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg Questionnaire -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( questionnaires, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.questionnaires
    in
    ( { model | questionnaires = questionnaires }
    , cmd
    )



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig Questionnaire
listingUpdateConfig wrapMsg appState =
    { getRequest = QuestionnairesApi.getQuestionnaires
    , getError = lg "apiError.questionnaires.getListError" appState
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.QuestionnairesRoute << IndexRoute
    }
