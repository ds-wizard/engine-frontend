module Wizard.KnowledgeModels.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lg)
import Wizard.Common.Api exposing (applyResultTransform, getResultCmd)
import Wizard.Common.Api.Packages as PackagesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing as Listing
import Wizard.Common.Setters exposing (setPackages)
import Wizard.KnowledgeModels.Common.Package as Package
import Wizard.KnowledgeModels.Index.Models exposing (Model)
import Wizard.KnowledgeModels.Index.Msgs exposing (Msg(..))
import Wizard.Msgs


l_ : String -> AppState -> String
l_ =
    l "Wizard.KnowledgeModels.Index.Update"


fetchData : AppState -> Cmd Msg
fetchData appState =
    PackagesApi.getPackages appState GetPackagesCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetPackagesCompleted result ->
            applyResultTransform
                { setResult = setPackages
                , defaultError = lg "apiError.packages.getListError" appState
                , model = model
                , result = result
                , transform = Listing.modelFromList << List.sortWith Package.compare
                }

        ShowHideDeletePackage package ->
            ( { model | packageToBeDeleted = package, deletingPackage = Unset }, Cmd.none )

        DeletePackage ->
            handleDeletePackage wrapMsg appState model

        DeletePackageCompleted result ->
            deletePackageCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            ( { model | packages = ActionResult.map (Listing.update listingMsg) model.packages }
            , Cmd.none
            )


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


deletePackageCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deletePackageCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model
                | deletingPackage = Success <| lg "apiSuccess.packages.delete" appState
                , packages = Loading
                , packageToBeDeleted = Nothing
              }
            , Cmd.map wrapMsg <| fetchData appState
            )

        Err error ->
            ( { model | deletingPackage = ApiError.toActionResult (lg "apiError.packages.deleteError" appState) error }
            , getResultCmd result
            )
