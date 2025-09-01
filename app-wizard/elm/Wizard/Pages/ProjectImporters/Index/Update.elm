module Wizard.Pages.ProjectImporters.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Utils.RequestHelpers as RequestHelpers
import Wizard.Api.Models.QuestionnaireImporter exposing (QuestionnaireImporter)
import Wizard.Api.QuestionnaireImporters as QuestionnaireImportersApi
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.Update as Listing
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.ProjectImporters.Index.Models exposing (Model)
import Wizard.Pages.ProjectImporters.Index.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

        ToggleEnabled questionnaireImporter ->
            ( { model | togglingEnabled = Loading }
            , Cmd.map wrapMsg <|
                QuestionnaireImportersApi.putQuestionnaireImporter appState
                    { questionnaireImporter | enabled = not questionnaireImporter.enabled }
                    ToggleEnabledComplete
            )

        ToggleEnabledComplete result ->
            RequestHelpers.applyResultTransformCmd
                { setResult = \r m -> { m | togglingEnabled = r }
                , defaultError = gettext "Unable to change project importer." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , transform = always (gettext "Project importer was changed successfully." appState.locale)
                , cmd = Cmd.map (wrapMsg << ListingMsg) Listing.fetchData
                , locale = appState.locale
                }


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg QuestionnaireImporter -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( questionnaireImporters, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.questionnaireImporters
    in
    ( { model | questionnaireImporters = questionnaireImporters }
    , cmd
    )



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig QuestionnaireImporter
listingUpdateConfig wrapMsg appState =
    { getRequest = QuestionnaireImportersApi.getQuestionnaireImporters appState
    , getError = gettext "Unable to get project importers." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.projectImportersIndexWithFilters
    }
