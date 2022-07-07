module Wizard.KnowledgeModels.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Shared.Api.Packages as PackagesApi
import Shared.Data.Package exposing (Package)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.KnowledgeModels.Index.Models exposing (Model)
import Wizard.KnowledgeModels.Index.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ShowHideDeletePackage package ->
            ( { model | packageToBeDeleted = package, deletingPackage = Unset }, Cmd.none )

        DeletePackage ->
            handleDeletePackage wrapMsg appState model

        DeletePackageCompleted result ->
            deletePackageCompleted appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

        ExportPackage package ->
            ( model, Ports.downloadFile (PackagesApi.exportPackageUrl package.id appState) )


handleDeletePackage : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeletePackage wrapMsg appState model =
    case model.packageToBeDeleted of
        Just package ->
            ( { model | deletingPackage = Loading }
            , Cmd.map wrapMsg <|
                PackagesApi.deletePackage package.organizationId package.kmId appState DeletePackageCompleted
            )

        Nothing ->
            ( model, Cmd.none )


deletePackageCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deletePackageCompleted appState model result =
    case result of
        Ok _ ->
            ( model
            , cmdNavigate appState (Listing.toRouteAfterDelete Routes.knowledgeModelsIndexWithFilters model.packages)
            )

        Err error ->
            ( { model | deletingPackage = ApiError.toActionResult appState (lg "apiError.packages.deleteError" appState) error }
            , getResultCmd result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg Package -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( packages, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.packages
    in
    ( { model | packages = packages }
    , cmd
    )



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig Package
listingUpdateConfig wrapMsg appState =
    { getRequest = PackagesApi.getPackages
    , getError = lg "apiError.packages.getListError" appState
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.knowledgeModelsIndexWithFilters PaginationQueryFilters.empty
    }
