module Wizard.Pages.KnowledgeModels.Compare.Update exposing (fetchData, update)

import ActionResult
import Common.Api.ApiError as ApiError
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Uuid exposing (Uuid)
import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Wizard.Components.KMComparison as KMComparison
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KnowledgeModels.Common.CompareSelectModal as CompareSelectModal
import Wizard.Pages.KnowledgeModels.Compare.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Compare.Msgs exposing (Msg(..))


fetchData : AppState -> Maybe Uuid -> Cmd Msg
fetchData appState mbLeftKmUuid =
    case mbLeftKmUuid of
        Just leftKmUuid ->
            KnowledgeModelPackagesApi.getKnowledgeModelPackage appState leftKmUuid GetInitialKnowledgeModelCompleted

        Nothing ->
            Cmd.none


update : AppState -> (Msg -> Wizard.Msgs.Msg) -> Msg -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update appState wrapMsg msg model =
    let
        compareSelectModalUpdateConfig =
            { wrapMsg = wrapMsg << CompareSelectModalMsg
            , logoutMsg = Wizard.Msgs.logoutMsg
            , compareMsg = wrapMsg << KMComparisonMsg << KMComparison.compare
            }
    in
    case msg of
        GetInitialKnowledgeModelCompleted result ->
            case result of
                Ok kmPackage ->
                    let
                        ( compareSelectModalModel, cmd ) =
                            CompareSelectModal.update appState compareSelectModalUpdateConfig (CompareSelectModal.setInitialLeftKm kmPackage) model.compareSelectModalModel
                    in
                    ( { model | compareSelectModalModel = compareSelectModalModel, initialLoading = ActionResult.Success () }, cmd )

                Err error ->
                    let
                        initialLoading =
                            ApiError.toActionResult appState (gettext "Unable to load knowledge model." appState.locale) error
                                |> ActionResult.map (always ())
                    in
                    ( { model | initialLoading = initialLoading }
                    , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                    )

        KMComparisonMsg kmComparisonMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << KMComparisonMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( kmComparisonModel, cmd ) =
                    KMComparison.update appState updateConfig kmComparisonMsg model.kmComparisonModel
            in
            ( { model | kmComparisonModel = kmComparisonModel }, cmd )

        CompareSelectModalMsg compareSelectModalMsg ->
            let
                ( compareSelectModalModel, cmd ) =
                    CompareSelectModal.update appState compareSelectModalUpdateConfig compareSelectModalMsg model.compareSelectModalModel
            in
            ( { model | compareSelectModalModel = compareSelectModalModel }, cmd )
