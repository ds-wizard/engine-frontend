module Wizard.Pages.ProjectActions.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Wizard.Api.Models.ProjectAction exposing (ProjectAction)
import Wizard.Api.ProjectActions as ProjectActionsApi
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.Update as Listing
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.ProjectActions.Index.Models exposing (Model)
import Wizard.Pages.ProjectActions.Index.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

        ToggleEnabled questionnaireAction ->
            ( { model | togglingEnabled = Loading }
            , Cmd.map wrapMsg <|
                ProjectActionsApi.put appState
                    { questionnaireAction | enabled = not questionnaireAction.enabled }
                    ToggleEnabledComplete
            )

        ToggleEnabledComplete result ->
            RequestHelpers.applyResultTransformCmd
                { setResult = \r m -> { m | togglingEnabled = r }
                , defaultError = gettext "Unable to change project action." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , transform = always (gettext "Project action was changed successfully." appState.locale)
                , cmd = Cmd.map (wrapMsg << ListingMsg) Listing.fetchData
                , locale = appState.locale
                }


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg ProjectAction -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( questionnaireActions, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.questionnaireActions
    in
    ( { model | questionnaireActions = questionnaireActions }
    , cmd
    )



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig ProjectAction
listingUpdateConfig wrapMsg appState =
    { getRequest = ProjectActionsApi.getList appState
    , getError = gettext "Unable to get project actions." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.projectActionsIndexWithFilters
    }
