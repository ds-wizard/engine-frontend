module Wizard.Projects.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Debouncer.Extra as Debouncer
import Dict
import Maybe.Extra as Maybe
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.Users as UsersApi
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Utils exposing (dispatch, flip, stringToBool)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Msgs
import Wizard.Projects.Common.CloneProjectModal.Update as CloneProjectModal
import Wizard.Projects.Common.DeleteProjectModal.Update as DeleteProjectModal
import Wizard.Projects.Detail.ProjectDetailRoute as PlanDetailRoute
import Wizard.Projects.Index.Models exposing (Model)
import Wizard.Projects.Index.Msgs exposing (Msg(..))
import Wizard.Projects.Routes exposing (Route(..), indexRouteIsTemplateFilterId, indexRouteProjectTagsFilterId, indexRouteUsersFilterId)
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    let
        selectedUsersCmd =
            case Dict.get indexRouteUsersFilterId model.questionnaires.filters.values of
                Just userUuids ->
                    UsersApi.getUsersSuggestionsWithOptions
                        model.questionnaires.paginationQueryString
                        (String.split "," userUuids)
                        []
                        appState
                        UsersFilterGetValuesComplete

                Nothing ->
                    Cmd.none
    in
    Cmd.batch
        [ Cmd.map ListingMsg Listing.fetchData
        , dispatch (ProjectTagsFilterSearch "")
        , dispatch (UsersFilterSearch "")
        , selectedUsersCmd
        ]


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
                        cmdNavigate appState << Routes.ProjectsRoute << flip DetailRoute PlanDetailRoute.Questionnaire << .uuid
                    }

                ( deleteModalModel, cmd ) =
                    CloneProjectModal.update updateConfig modalMsg appState model.cloneModalModel
            in
            ( { model | cloneModalModel = deleteModalModel }
            , cmd
            )

        ProjectTagsFilterInput value ->
            ( { model | projectTagsFilterSearchValue = value }
            , dispatch (wrapMsg <| DebouncerMsg <| Debouncer.provideInput <| ProjectTagsFilterSearch value)
            )

        ProjectTagsFilterSearch value ->
            let
                queryString =
                    PaginationQueryString.fromQ value
                        |> PaginationQueryString.withSize (Just 10)

                selectedTags =
                    model.questionnaires.filters.values
                        |> Dict.get indexRouteProjectTagsFilterId
                        |> Maybe.unwrap [] (String.split ",")

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.getProjectTagsSuggestions queryString selectedTags appState ProjectTagsFilterSearchComplete
            in
            ( model, cmd )

        ProjectTagsFilterSearchComplete result ->
            applyResult appState
                { setResult = \r m -> { m | projectTagsFilterTags = r }
                , defaultError = lg "apiError.questionnaires.getProjectTagsSuggestionsError" appState
                , model = model
                , result = result
                }

        UsersFilterGetValuesComplete result ->
            applyResult appState
                { setResult = \r m -> { m | userFilterSelectedUsers = r }
                , defaultError = lg "apiError.users.getListError" appState
                , model = model
                , result = result
                }

        UsersFilterInput value ->
            ( { model | userFilterSearchValue = value }
            , dispatch (wrapMsg <| DebouncerMsg <| Debouncer.provideInput <| UsersFilterSearch value)
            )

        UsersFilterSearch value ->
            let
                queryString =
                    PaginationQueryString.fromQ value
                        |> PaginationQueryString.withSize (Just 10)

                selectedUsers =
                    model.questionnaires.filters.values
                        |> Dict.get indexRouteUsersFilterId
                        |> Maybe.unwrap [] (String.split ",")

                cmd =
                    Cmd.map wrapMsg <|
                        UsersApi.getUsersSuggestionsWithOptions queryString [] selectedUsers appState UsersFilterSearchComplete
            in
            ( model, cmd )

        UsersFilterSearchComplete result ->
            applyResult appState
                { setResult = \r m -> { m | userFilterUsers = r }
                , defaultError = lg "apiError.users.getListError" appState
                , model = model
                , result = result
                }

        DebouncerMsg debounceMsg ->
            let
                updateConfig =
                    { mapMsg = wrapMsg << DebouncerMsg
                    , getDebouncer = .debouncer
                    , setDebouncer = \d m -> { m | debouncer = d }
                    }

                update_ updateMsg updateModel =
                    update wrapMsg updateMsg appState updateModel
            in
            Debouncer.update update_ updateConfig debounceMsg model


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
                    Listing.update (listingUpdateConfig wrapMsg appState model) appState ListingMsgs.Reload model.questionnaires
            in
            ( { model
                | deletingMigration = Success <| lg "apiSuccess.questionnaires.migration.delete" appState
                , questionnaires = questionnaires
              }
            , cmd
            )

        Err error ->
            ( { model | deletingMigration = ApiError.toActionResult appState (lg "apiError.questionnaires.migrations.deleteError" appState) error }
            , getResultCmd result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg Questionnaire -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( questionnaires, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState model) appState listingMsg model.questionnaires
    in
    ( { model | questionnaires = questionnaires }
    , cmd
    )



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Listing.UpdateConfig Questionnaire
listingUpdateConfig wrapMsg appState model =
    let
        isTemplate =
            Maybe.map stringToBool <|
                PaginationQueryFilters.getValue indexRouteIsTemplateFilterId model.questionnaires.filters

        users =
            PaginationQueryFilters.getValue indexRouteUsersFilterId model.questionnaires.filters

        usersOp =
            PaginationQueryFilters.getOp indexRouteUsersFilterId model.questionnaires.filters

        projectTags =
            PaginationQueryFilters.getValue indexRouteProjectTagsFilterId model.questionnaires.filters

        projectTagsOp =
            PaginationQueryFilters.getOp indexRouteProjectTagsFilterId model.questionnaires.filters
    in
    { getRequest =
        QuestionnairesApi.getQuestionnaires
            { isTemplate = isTemplate
            , userUuids = users
            , userUuidsOp = usersOp
            , projectTags = projectTags
            , projectTagsOp = projectTagsOp
            }
    , getError = lg "apiError.questionnaires.getListError" appState
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.projectIndexWithFilters model.questionnaires.filters
    }
