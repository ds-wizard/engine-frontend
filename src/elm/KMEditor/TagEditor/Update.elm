module KMEditor.TagEditor.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Jwt
import KMEditor.Common.Models.Events exposing (Event, encodeEvents)
import KMEditor.Requests exposing (getKnowledgeModelData, postEventsBulk)
import KMEditor.Routing exposing (Route(..))
import KMEditor.TagEditor.Models exposing (Model, addQuestionTag, generateEvents, initialModel, removeQuestionTag, setKnowledgeModel)
import KMEditor.TagEditor.Msgs exposing (Msg(..))
import Models exposing (State)
import Msgs
import Random exposing (Seed)
import Requests exposing (getResultCmd)
import Routing exposing (cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
fetchData wrapMsg uuid session =
    getKnowledgeModelData uuid session
        |> Jwt.send GetKnowledgeModelCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        GetKnowledgeModelCompleted result ->
            let
                newModel =
                    case result of
                        Ok knowledgeModel ->
                            setKnowledgeModel model knowledgeModel

                        Err error ->
                            { model | knowledgeModel = getServerErrorJwt error "Unable to get knowledge model" }

                cmd =
                    getResultCmd result
            in
            ( state.seed, newModel, cmd )

        Highlight tagUuid ->
            ( state.seed, { model | highlightedTagUuid = Just tagUuid }, Cmd.none )

        CancelHighlight ->
            ( state.seed, { model | highlightedTagUuid = Nothing }, Cmd.none )

        AddTag questionUuid tagUuid ->
            ( state.seed, addQuestionTag model questionUuid tagUuid, Cmd.none )

        RemoveTag questionUuid tagUuid ->
            ( state.seed, removeQuestionTag model questionUuid tagUuid, Cmd.none )

        Submit ->
            let
                ( newSeed, events ) =
                    model.knowledgeModel
                        |> ActionResult.map (generateEvents model state.seed)
                        |> ActionResult.withDefault ( state.seed, [] )

                cmd =
                    sendEventsCmd wrapMsg state.session model events
            in
            ( newSeed, { model | submitting = Loading }, cmd )

        SubmitCompleted result ->
            case result of
                Ok _ ->
                    ( state.seed
                    , initialModel ""
                    , cmdNavigate state.key <| Routing.KMEditor IndexRoute
                    )

                Err error ->
                    ( state.seed
                    , { model | submitting = getServerErrorJwt error "Knowledge model could not be saved" }
                    , getResultCmd result
                    )

        Discard ->
            ( state.seed
            , initialModel ""
            , cmdNavigate state.key <| Routing.KMEditor IndexRoute
            )


sendEventsCmd : (Msg -> Msgs.Msg) -> Session -> Model -> List Event -> Cmd Msgs.Msg
sendEventsCmd wrapMsg session model events =
    encodeEvents events
        |> postEventsBulk session model.branchUuid
        |> Jwt.send SubmitCompleted
        |> Cmd.map wrapMsg
