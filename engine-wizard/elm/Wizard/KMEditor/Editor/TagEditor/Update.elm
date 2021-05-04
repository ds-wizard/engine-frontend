module Wizard.KMEditor.Editor.TagEditor.Update exposing (update)

import Shared.Copy as Copy
import Wizard.KMEditor.Editor.TagEditor.Models exposing (Model, addQuestionTag, removeQuestionTag)
import Wizard.KMEditor.Editor.TagEditor.Msgs exposing (Msg(..))


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
            ( model, Copy.copyToClipboard uuid )
