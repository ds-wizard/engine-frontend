module KMEditor.Editor.TagEditor.Update exposing (update)

import KMEditor.Editor.TagEditor.Models exposing (Model, addQuestionTag, removeQuestionTag)
import KMEditor.Editor.TagEditor.Msgs exposing (Msg(..))
import Ports


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Highlight tagUuid ->
            ( { model | highlightedTagUuid = Just tagUuid }, Cmd.none )

        CancelHighlight ->
            ( { model | highlightedTagUuid = Nothing }, Cmd.none )

        AddTag questionUuid tagUuid ->
            ( addQuestionTag model questionUuid tagUuid, Cmd.none )

        RemoveTag questionUuid tagUuid ->
            ( removeQuestionTag model questionUuid tagUuid, Cmd.none )

        CopyUuid uuid ->
            ( model, Ports.copyToClipboard uuid )
