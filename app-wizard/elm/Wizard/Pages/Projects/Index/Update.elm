module Wizard.Pages.Projects.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Debouncer.Extra as Debouncer
import Dict
import Gettext exposing (gettext)
import Html.Attributes.Extensions exposing (selectDataTour)
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Utils.Driver as Driver exposing (TourConfig)
import Shared.Utils.RequestHelpers as RequestHelpers
import Shared.Utils.Setters exposing (setDebouncer)
import Task.Extra as Task
import Uuid exposing (Uuid)
import Wizard.Api.Models.PackageSuggestion as PackageSuggestion
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Api.Packages as PackagesApi
import Wizard.Api.Questionnaires as QuestionnairesApi
import Wizard.Api.Users as UsersApi
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.Update as Listing
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Projects.Common.CloneProjectModal.Update as CloneProjectModal
import Wizard.Pages.Projects.Common.DeleteProjectModal.Update as DeleteProjectModal
import Wizard.Pages.Projects.Index.Models exposing (Model)
import Wizard.Pages.Projects.Index.Msgs exposing (Msg(..))
import Wizard.Pages.Projects.Routes exposing (indexRoutePackagesFilterId, indexRouteProjectTagsFilterId, indexRouteUsersFilterId)
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)
import Wizard.Utils.Driver as Driver
import Wizard.Utils.TourId as TourId


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    let
        selectedUsersCmd =
            case Dict.get indexRouteUsersFilterId model.questionnaires.filters.values of
                Just userUuids ->
                    UsersApi.getUsersSuggestionsWithOptions appState
                        PaginationQueryString.empty
                        (String.split "," userUuids)
                        []
                        UsersFilterGetValuesComplete

                Nothing ->
                    Cmd.none

        selectedPackagesCmd =
            case Dict.get indexRoutePackagesFilterId model.questionnaires.filters.values of
                Just packageIds ->
                    PackagesApi.getPackagesSuggestionsWithOptions appState
                        PaginationQueryString.empty
                        (String.split "," packageIds)
                        []
                        PackagesFilterGetValuesComplete

                Nothing ->
                    Cmd.none
    in
    Cmd.batch
        [ Cmd.map ListingMsg Listing.fetchData
        , Task.dispatch (ProjectTagsFilterSearch "")
        , Task.dispatch (UsersFilterSearch "")
        , Task.dispatch (PackagesFilterSearch "")
        , selectedUsersCmd
        , selectedPackagesCmd
        , Driver.init (tour appState)
        ]


tour : AppState -> TourConfig
tour appState =
    Driver.fromAppState TourId.projectsIndex appState
        |> Driver.addStep
            { element = Nothing
            , popover =
                { title = gettext "Projects" appState.locale
                , description = gettext "All your projects will be here, including those shared with you." appState.locale
                }
            }
        |> Driver.addStep
            { element = selectDataTour "projects_create-button"
            , popover =
                { title = gettext "Create Project" appState.locale
                , description = gettext "Click here to start a new project." appState.locale
                }
            }


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        DeleteQuestionnaireMigration uuid ->
            handleDeleteMigration wrapMsg appState model uuid

        DeleteQuestionnaireMigrationCompleted result ->
            handleDeleteMigrationCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

        ListingFilterAddSelectedPackage package listingMsg ->
            let
                updatePackages packages =
                    { packages | items = List.uniqueBy (PackageSuggestion.packageIdAll << .id) (package :: packages.items) }

                newModel =
                    { model | packagesFilterSelectedPackages = ActionResult.map updatePackages model.packagesFilterSelectedPackages }
            in
            handleListingMsg wrapMsg appState listingMsg newModel

        ListingFilterAddSelectedUser user listingMsg ->
            let
                updateUsers users =
                    { users | items = List.uniqueBy .uuid (user :: users.items) }

                newModel =
                    { model | userFilterSelectedUsers = ActionResult.map updateUsers model.userFilterSelectedUsers }
            in
            handleListingMsg wrapMsg appState listingMsg newModel

        DeleteQuestionnaireModalMsg modalMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << DeleteQuestionnaireModalMsg
                    , deleteCompleteCmd = Task.dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
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
                        cmdNavigate appState << Routes.projectsDetail << .uuid
                    }

                ( deleteModalModel, cmd ) =
                    CloneProjectModal.update updateConfig modalMsg appState model.cloneModalModel
            in
            ( { model | cloneModalModel = deleteModalModel }
            , cmd
            )

        ProjectTagsFilterInput value ->
            ( { model | projectTagsFilterSearchValue = value }
            , Task.dispatch (wrapMsg <| DebouncerMsg <| Debouncer.provideInput <| ProjectTagsFilterSearch value)
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
                        QuestionnairesApi.getProjectTagsSuggestions appState queryString selectedTags (ProjectTagsFilterSearchComplete value)
            in
            ( model, cmd )

        ProjectTagsFilterSearchComplete search result ->
            case result of
                Ok data ->
                    let
                        model_ =
                            if String.isEmpty search then
                                { model | projectTagsExist = Success (not <| List.isEmpty data.items) }

                            else
                                model
                    in
                    ( { model_ | projectTagsFilterTags = Success data }
                    , Cmd.none
                    )

                Err err ->
                    let
                        model_ =
                            if String.isEmpty search then
                                { model | projectTagsExist = Error "" }

                            else
                                model
                    in
                    ( { model_ | projectTagsFilterTags = ApiError.toActionResult appState (gettext "Unable to get project tags." appState.locale) err }
                    , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                    )

        UsersFilterGetValuesComplete result ->
            RequestHelpers.applyResult
                { setResult = \r m -> { m | userFilterSelectedUsers = r }
                , defaultError = gettext "Unable to get users." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }

        UsersFilterInput value ->
            ( { model | userFilterSearchValue = value }
            , Task.dispatch (wrapMsg <| DebouncerMsg <| Debouncer.provideInput <| UsersFilterSearch value)
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
                        UsersApi.getUsersSuggestionsWithOptions appState queryString [] selectedUsers UsersFilterSearchComplete
            in
            ( model, cmd )

        UsersFilterSearchComplete result ->
            RequestHelpers.applyResult
                { setResult = \r m -> { m | userFilterUsers = r }
                , defaultError = gettext "Unable to get users." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }

        PackagesFilterGetValuesComplete result ->
            RequestHelpers.applyResult
                { setResult = \r m -> { m | packagesFilterSelectedPackages = r }
                , defaultError = gettext "Unable to get Knowledge Models." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }

        PackagesFilterInput value ->
            ( { model | packagesFilterSearchValue = value }
            , Task.dispatch (wrapMsg <| DebouncerMsg <| Debouncer.provideInput <| PackagesFilterSearch value)
            )

        PackagesFilterSearch value ->
            let
                queryString =
                    PaginationQueryString.fromQ value
                        |> PaginationQueryString.withSize (Just 10)

                selectedKMs =
                    model.questionnaires.filters.values
                        |> Dict.get indexRoutePackagesFilterId
                        |> Maybe.unwrap [] (String.split ",")

                cmd =
                    Cmd.map wrapMsg <|
                        PackagesApi.getPackagesSuggestionsWithOptions appState queryString [] selectedKMs PackagesFilterSearchComplete
            in
            ( model, cmd )

        PackagesFilterSearchComplete result ->
            RequestHelpers.applyResult
                { setResult = \r m -> { m | packagesFilterPackages = r }
                , defaultError = gettext "Unable to get knowledge models." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }

        DebouncerMsg debounceMsg ->
            let
                updateConfig =
                    { mapMsg = wrapMsg << DebouncerMsg
                    , getDebouncer = .debouncer
                    , setDebouncer = setDebouncer
                    }

                update_ updateMsg updateModel =
                    update wrapMsg updateMsg appState updateModel
            in
            Debouncer.update update_ updateConfig debounceMsg model


handleDeleteMigration : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Uuid -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteMigration wrapMsg appState model uuid =
    ( { model | deletingMigration = Loading }
    , QuestionnairesApi.deleteQuestionnaireMigration appState uuid (wrapMsg << DeleteQuestionnaireMigrationCompleted)
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
                | deletingMigration = Success <| gettext "Project migration was successfully canceled." appState.locale
                , questionnaires = questionnaires
              }
            , cmd
            )

        Err error ->
            ( { model | deletingMigration = ApiError.toActionResult appState (gettext "Project migration could not be deleted." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
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
    { getRequest = QuestionnairesApi.getQuestionnaires appState
    , getError = gettext "Unable to get projects." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.projectsIndexWithFilters
    }
