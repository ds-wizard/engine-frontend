module KMEditor.Editor2.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Jwt
import KMEditor.Common.Models exposing (Branch)
import KMEditor.Common.Models.Events exposing (Event)
import KMEditor.Editor2.Models exposing (EditorType(..), Model, applyCurrentEditorChanges)
import KMEditor.Editor2.Msgs exposing (Msg(..))
import KMEditor.Editor2.Preview.Models
import KMEditor.Editor2.Preview.Update
import KMEditor.Editor2.TagEditor.Models as TagEditorModel
import KMEditor.Editor2.TagEditor.Update
import KMEditor.Requests exposing (getBranch, getLevels, getMetrics, postForPreview, putBranch)
import KMEditor.Routing exposing (Route(..))
import Models exposing (State)
import Msgs
import Random exposing (Seed)
import Requests exposing (getResultCmd)
import Routing exposing (cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
fetchData wrapMsg uuid session =
    Cmd.map wrapMsg <|
        Cmd.batch
            [ fetchBranch uuid session
            , fetchMetrics session
            , fetchLevels session
            ]


fetchBranch : String -> Session -> Cmd Msg
fetchBranch uuid session =
    getBranch uuid session
        |> Jwt.send GetBranchCompleted


fetchMetrics : Session -> Cmd Msg
fetchMetrics session =
    getMetrics session
        |> Jwt.send GetMetricsCompleted


fetchLevels : Session -> Cmd Msg
fetchLevels session =
    getLevels session
        |> Jwt.send GetLevelsCompleted


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        GetBranchCompleted result ->
            let
                ( newModel, cmd ) =
                    case result of
                        Ok branch ->
                            fetchPreview wrapMsg state.session { model | branch = Success branch }

                        Err error ->
                            ( { model | branch = getServerErrorJwt error "Unable to get Knowledge Model metadata" }
                            , getResultCmd result
                            )
            in
            ( state.seed, newModel, cmd )

        GetMetricsCompleted result ->
            let
                ( newModel, cmd ) =
                    case result of
                        Ok metrics ->
                            fetchPreview wrapMsg state.session { model | metrics = Success metrics }

                        Err error ->
                            ( { model | metrics = getServerErrorJwt error "Unable to get metrics" }
                            , getResultCmd result
                            )
            in
            ( state.seed, newModel, cmd )

        GetLevelsCompleted result ->
            let
                ( newModel, cmd ) =
                    case result of
                        Ok levels ->
                            fetchPreview wrapMsg state.session { model | levels = Success levels }

                        Err error ->
                            ( { model | levels = getServerErrorJwt error "Unable to get levels" }
                            , getResultCmd result
                            )
            in
            ( state.seed, newModel, cmd )

        GetPreviewCompleted result ->
            let
                newModel =
                    case result of
                        Ok km ->
                            { model
                                | preview = Success km
                                , previewEditorModel = Just <| KMEditor.Editor2.Preview.Models.initialModel km
                                , tagEditorModel = Just <| TagEditorModel.initialModel km
                            }

                        Err error ->
                            { model | preview = getServerErrorJwt error "Unable to get Knowledge Model" }

                cmd =
                    getResultCmd result
            in
            ( state.seed, newModel, cmd )

        OpenEditor editor ->
            let
                ( newSeed, modelWithEvents ) =
                    applyCurrentEditorChanges state.seed model

                ( newModel, cmd ) =
                    fetchPreview wrapMsg state.session { modelWithEvents | currentEditor = editor }
            in
            ( newSeed, newModel, cmd )

        PreviewEditorMsg previewMsg ->
            let
                previewEditorModel =
                    model.previewEditorModel
                        |> Maybe.map (KMEditor.Editor2.Preview.Update.update previewMsg)
            in
            ( state.seed, { model | previewEditorModel = previewEditorModel }, Cmd.none )

        TagEditorMsg tagMsg ->
            let
                tagEditorModel =
                    model.tagEditorModel
                        |> Maybe.map (KMEditor.Editor2.TagEditor.Update.update tagMsg)
            in
            ( state.seed, { model | tagEditorModel = tagEditorModel }, Cmd.none )

        Save ->
            let
                ( newSeed, newModel ) =
                    applyCurrentEditorChanges state.seed model

                cmd =
                    model.branch
                        |> ActionResult.map (putBranchCmd wrapMsg state.session newModel)
                        |> ActionResult.withDefault Cmd.none
            in
            ( newSeed, { newModel | saving = Loading }, cmd )

        SaveCompleted result ->
            case result of
                Ok _ ->
                    ( state.seed, model, cmdNavigate state.key <| Routing.KMEditor IndexRoute )

                Err error ->
                    ( state.seed
                    , { model | saving = getServerErrorJwt error "Knowledge model could not be saved" }
                    , getResultCmd result
                    )


fetchPreview : (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
fetchPreview wrapMsg session model =
    case ActionResult.combine3 model.branch model.metrics model.levels of
        Success ( branch, _, _ ) ->
            ( { model | preview = Loading }
            , Cmd.map wrapMsg <| createPreviewRequest branch model.sessionEvents session
            )

        _ ->
            ( model, Cmd.none )


putBranchCmd : (Msg -> Msgs.Msg) -> Session -> Model -> Branch -> Cmd Msgs.Msg
putBranchCmd wrapMsg session model branch =
    putBranch model.branchUuid branch.name branch.kmId model.sessionEvents session
        |> Jwt.send SaveCompleted
        |> Cmd.map wrapMsg


createPreviewRequest : Branch -> List Event -> Session -> Cmd Msg
createPreviewRequest branch sessionEvents session =
    postForPreview branch.parentPackageId (branch.events ++ sessionEvents) [] session
        |> Jwt.send GetPreviewCompleted
