module Wizard.ProjectImporters.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Api.QuestionnaireImporters as QuestionnaireImportersApi
import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Wizard.Common.Api exposing (applyResultTransformCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Msgs
import Wizard.ProjectImporters.Index.Models exposing (Model)
import Wizard.ProjectImporters.Index.Msgs exposing (Msg(..))
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
                QuestionnaireImportersApi.putQuestionnaireImporter
                    { questionnaireImporter | enabled = not questionnaireImporter.enabled }
                    appState
                    ToggleEnabledComplete
            )

        ToggleEnabledComplete result ->
            applyResultTransformCmd appState
                { setResult = \r m -> { m | togglingEnabled = r }
                , defaultError = gettext "Unable to change project importer." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , transform = always (gettext "Project importer was changed successfully." appState.locale)
                , cmd = Cmd.map (wrapMsg << ListingMsg) Listing.fetchData
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
    { getRequest = QuestionnaireImportersApi.getQuestionnaireImporters
    , getError = gettext "Unable to get project importers." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.projectImportersIndexWithFilters
    }
