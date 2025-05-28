module Wizard.Projects.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Debouncer.Extra as Debouncer
import Dict
import Gettext exposing (gettext)
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Api.Packages as PackagesApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.Users as UsersApi
import Shared.Data.PackageSuggestion as PackageSuggestion
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Setters exposing (setDebouncer)
import Shared.Utils exposing (dispatch)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Common.Driver as Driver exposing (TourConfig)
import Wizard.Common.Html.Attribute exposing (selectDataTour)
import Wizard.Common.TourId as TourId
import Wizard.Msgs
import Wizard.Projects.Common.CloneProjectModal.Update as CloneProjectModal
import Wizard.Projects.Common.DeleteProjectModal.Update as DeleteProjectModal
import Wizard.Projects.Index.Models exposing (Model)
import Wizard.Projects.Index.Msgs exposing (Msg(..))
import Wizard.Projects.Routes exposing (indexRoutePackagesFilterId, indexRouteProjectTagsFilterId, indexRouteUsersFilterId)
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    let
        selectedUsersCmd =
            case Dict.get indexRouteUsersFilterId model.questionnaires.filters.values of
                Just userUuids ->
                    UsersApi.getUsersSuggestionsWithOptions
                        PaginationQueryString.empty
                        (String.split "," userUuids)
                        []
                        appState
                        UsersFilterGetValuesComplete

                Nothing ->
                    Cmd.none

        selectedPackagesCmd =
            case Dict.get indexRoutePackagesFilterId model.questionnaires.filters.values of
                Just packageIds ->
                    PackagesApi.getPackagesSuggestionsWithOptions
                        PaginationQueryString.empty
                        (String.split "," packageIds)
                        []
                        appState
                        PackagesFilterGetValuesComplete

                Nothing ->
                    Cmd.none
    in
    Cmd.batch
        [ Cmd.map ListingMsg Listing.fetchData
        , dispatch (ProjectTagsFilterSearch "")
        , dispatch (UsersFilterSearch "")
        , dispatch (PackagesFilterSearch "")
        , selectedUsersCmd
        , selectedPackagesCmd
        , Driver.init (tour appState)
        ]


tour : AppState -> TourConfig
tour appState =
    Driver.tourConfig TourId.projectsIndex appState
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
                    , deleteCompleteCmd = dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
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
                        QuestionnairesApi.getProjectTagsSuggestions queryString selectedTags appState (ProjectTagsFilterSearchComplete value)
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
                    , getResultCmd Wizard.Msgs.logoutMsg result
                    )

        UsersFilterGetValuesComplete result ->
            applyResult appState
                { setResult = \r m -> { m | userFilterSelectedUsers = r }
                , defaultError = gettext "Unable to get users." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
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
                , defaultError = gettext "Unable to get users." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }

        PackagesFilterGetValuesComplete result ->
            applyResult appState
                { setResult = \r m -> { m | packagesFilterSelectedPackages = r }
                , defaultError = gettext "Unable to get Knowledge Models." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }

        PackagesFilterInput value ->
            ( { model | packagesFilterSearchValue = value }
            , dispatch (wrapMsg <| DebouncerMsg <| Debouncer.provideInput <| PackagesFilterSearch value)
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
                        PackagesApi.getPackagesSuggestionsWithOptions queryString [] selectedKMs appState PackagesFilterSearchComplete
            in
            ( model, cmd )

        PackagesFilterSearchComplete result ->
            applyResult appState
                { setResult = \r m -> { m | packagesFilterPackages = r }
                , defaultError = gettext "Unable to get knowledge models." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
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
                | deletingMigration = Success <| gettext "Project migration was successfully canceled." appState.locale
                , questionnaires = questionnaires
              }
            , cmd
            )

        Err error ->
            ( { model | deletingMigration = ApiError.toActionResult appState (gettext "Project migration could not be deleted." appState.locale) error }
            , getResultCmd Wizard.Msgs.logoutMsg result
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
    , getError = gettext "Unable to get projects." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.projectsIndexWithFilters
    }
