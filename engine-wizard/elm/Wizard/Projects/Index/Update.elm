module Wizard.Projects.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Utils exposing (dispatch, flip)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Msgs
import Wizard.Projects.Common.CloneProjectModal.Update as CloneProjectModal
import Wizard.Projects.Common.DeleteProjectModal.Update as DeleteProjectModal
import Wizard.Projects.Detail.PlanDetailRoute as PlanDetailRoute
import Wizard.Projects.Index.Models exposing (Model)
import Wizard.Projects.Index.Msgs exposing (Msg(..))
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


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
                    DeleteProjectModal.update updateConfig modalMsg appState model.deleteModalModel
            in
            ( { model | deleteModalModel = deleteModalModel }
            , cmd
            )

        CloneQuestionnaireModalMsg modalMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << CloneQuestionnaireModalMsg
                    , cloneCompleteCmd =
                        cmdNavigate appState << Routes.PlansRoute << flip DetailRoute PlanDetailRoute.Questionnaire << .uuid
                    }

                ( deleteModalModel, cmd ) =
                    CloneProjectModal.update updateConfig modalMsg appState model.cloneModalModel
            in
            ( { model | cloneModalModel = deleteModalModel }
            , cmd
            )


handleDeleteMigration : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Uuid -> ( Model, Cmd Wizard.Msgs.Msg )
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
    , toRoute = Routes.PlansRoute << IndexRoute
    }
