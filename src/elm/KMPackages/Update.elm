module KMPackages.Update exposing (..)

import Auth.Models exposing (Session)
import KMPackages.Detail.Update
import KMPackages.Import.Update
import KMPackages.Index.Update
import KMPackages.Models exposing (Model)
import KMPackages.Msgs exposing (Msg(..))
import KMPackages.Routing exposing (Route(..))
import Msgs


fetchData : Route -> (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData route wrapMsg session =
    case route of
        Detail organizationId kmId ->
            KMPackages.Detail.Update.fetchData (wrapMsg << DetailMsg) organizationId kmId session

        Index ->
            KMPackages.Index.Update.fetchData (wrapMsg << IndexMsg) session

        _ ->
            Cmd.none


update : Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg session model =
    case msg of
        DetailMsg msg ->
            let
                ( detailModel, cmd ) =
                    KMPackages.Detail.Update.update msg (wrapMsg << DetailMsg) session model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )

        ImportMsg msg ->
            let
                ( importModel, cmd ) =
                    KMPackages.Import.Update.update msg (wrapMsg << ImportMsg) session model.importModel
            in
            ( { model | importModel = importModel }, cmd )

        IndexMsg msg ->
            let
                ( indexModel, cmd ) =
                    KMPackages.Index.Update.update msg (wrapMsg << IndexMsg) session model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
