module Wizard.ProjectActions.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Utils.RequestHelpers as RequestHelpers
import Wizard.Api.Models.QuestionnaireAction exposing (QuestionnaireAction)
import Wizard.Api.QuestionnaireActions as QuestionnaireActionsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Msgs
import Wizard.ProjectActions.Index.Models exposing (Model)
import Wizard.ProjectActions.Index.Msgs exposing (Msg(..))
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
                QuestionnaireActionsApi.putQuestionnaireAction appState
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


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg QuestionnaireAction -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( questionnaireActions, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.questionnaireActions
    in
    ( { model | questionnaireActions = questionnaireActions }
    , cmd
    )



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig QuestionnaireAction
listingUpdateConfig wrapMsg appState =
    { getRequest = QuestionnaireActionsApi.getQuestionnaireActions appState
    , getError = gettext "Unable to get project actions." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.projectActionsIndexWithFilters
    }
