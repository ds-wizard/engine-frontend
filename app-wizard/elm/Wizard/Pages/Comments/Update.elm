module Wizard.Pages.Comments.Update exposing (fetchData, update)

import Gettext exposing (gettext)
import Wizard.Api.Models.ProjectCommentThreadAssigned exposing (ProjectCommentThreadAssigned)
import Wizard.Api.ProjectCommentThreads as ProjectCommentThreadsApi
import Wizard.Components.Listing.Update as Listing
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Comments.Models exposing (Model)
import Wizard.Pages.Comments.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        ListingMsg listingMsg ->
            let
                ( commentThreads, cmd ) =
                    Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.commentThreads
            in
            ( { model | commentThreads = commentThreads }
            , cmd
            )


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig ProjectCommentThreadAssigned
listingUpdateConfig wrapMsg appState =
    { getRequest = ProjectCommentThreadsApi.getCommentThreads appState
    , getError = gettext "Unable to get comments." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.commentsIndexWithFilters
    }
