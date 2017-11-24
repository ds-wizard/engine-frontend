module KnowledgeModels.Editor.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import Jwt
import KnowledgeModels.Editor.Models exposing (..)
import KnowledgeModels.Editor.Models.Editors exposing (..)
import KnowledgeModels.Editor.Models.Entities exposing (..)
import KnowledgeModels.Editor.Models.Events exposing (..)
import KnowledgeModels.Editor.Models.Forms exposing (..)
import KnowledgeModels.Editor.Msgs exposing (..)
import KnowledgeModels.Requests exposing (getKnowledgeModelData, postEventsBulk)
import List.Extra exposing (getAt)
import Msgs
import Random.Pcg exposing (Seed)
import Reorderable
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)
import Set
import Utils exposing (getUuid, tuplePrepend)


getKnowledgeModelCmd : String -> Session -> Cmd Msgs.Msg
getKnowledgeModelCmd uuid session =
    getKnowledgeModelData uuid session
        |> toCmd GetKnowledgeModelCompleted Msgs.KnowledgeModelsEditorMsg


getKnowledgeModelCompleted : Model -> Result Jwt.JwtError KnowledgeModel -> ( Model, Cmd Msgs.Msg )
getKnowledgeModelCompleted model result =
    let
        newModel =
            case result of
                Ok knowledgeModel ->
                    { model | knowledgeModelEditor = Success <| createKnowledgeModelEditor knowledgeModel }

                Err error ->
                    { model | knowledgeModelEditor = Error "Unable to get knowledge model" }
    in
    ( newModel, Cmd.none )


postEventsBulkCmd : String -> List Event -> Session -> Cmd Msgs.Msg
postEventsBulkCmd uuid events session =
    encodeEvents events
        |> postEventsBulk session uuid
        |> toCmd SaveCompleted Msgs.KnowledgeModelsEditorMsg


postEventsBulkCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postEventsBulkCompleted model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate KnowledgeModels )

        Err error ->
            ( { model | saving = Error "Knowledge model could not be saved" }, Cmd.none )


updateEdit : KnowledgeModelMsg -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
updateEdit msg seed session model =
    case model.knowledgeModelEditor of
        Success knowledgeModelEditor ->
            let
                ( newSeed, newKnolwedgeModelEditor, maybeEvent, submit ) =
                    updateKnowledgeModel msg seed knowledgeModelEditor

                newEvents =
                    case maybeEvent of
                        Just event ->
                            model.events ++ [ event ]

                        Nothing ->
                            model.events

                ( newModel, cmd ) =
                    if submit then
                        ( { model | saving = Loading }, postEventsBulkCmd model.branchUuid newEvents session )
                    else
                        ( model, Cmd.none )
            in
            ( newSeed, { newModel | knowledgeModelEditor = Success newKnolwedgeModelEditor, events = newEvents }, cmd )

        _ ->
            ( seed, model, Cmd.none )


updateKnowledgeModel : KnowledgeModelMsg -> Seed -> KnowledgeModelEditor -> ( Seed, KnowledgeModelEditor, Maybe Event, Bool )
updateKnowledgeModel msg seed ((KnowledgeModelEditor editor) as originalEditor) =
    case msg of
        KnowledgeModelFormMsg formMsg ->
            case ( formMsg, Form.getOutput editor.form, formChanged editor.form || editor.chaptersDirty ) of
                ( Form.Submit, Just knowledgeModelForm, True ) ->
                    let
                        newKnowledgeModel =
                            updateKnowledgeModelWithForm editor.knowledgeModel knowledgeModelForm

                        chapterIds =
                            List.map (\(ChapterEditor e) -> e.chapter.uuid) editor.chapters

                        ( event, newSeed ) =
                            createEditKnowledgeModelEvent seed newKnowledgeModel chapterIds
                    in
                    ( newSeed, originalEditor, Just event, True )

                ( Form.Submit, Just chapterForm, False ) ->
                    ( seed, originalEditor, Nothing, True )

                _ ->
                    let
                        newForm =
                            Form.update knowledgeModelFormValidation formMsg editor.form
                    in
                    ( seed, KnowledgeModelEditor { editor | form = newForm }, Nothing, False )

        AddChapter ->
            let
                ( newUuid, seed2 ) =
                    getUuid seed

                chapter =
                    { uuid = newUuid, title = "New chapter", text = "Chapter text", questions = [] }

                chapterForm =
                    chapterFormInitials chapter
                        |> initForm chapterFormValidation

                newChapterEditor =
                    ChapterEditor { active = True, form = chapterForm, chapter = chapter, questions = [], order = List.length editor.chapters }

                newChapters =
                    editor.chapters ++ [ newChapterEditor ]

                ( event, newSeed ) =
                    createAddChapterEvent editor.knowledgeModel seed2 chapter
            in
            ( newSeed, KnowledgeModelEditor { editor | chapters = newChapters }, Just event, False )

        ViewChapter uuid ->
            let
                activateChapter seed (ChapterEditor chapterEditor) =
                    ( seed, ChapterEditor { chapterEditor | active = True }, Nothing )

                ( newSeed, newChapters, event ) =
                    updateInList editor.chapters seed (\(ChapterEditor e) -> e.chapter.uuid == uuid) activateChapter
            in
            ( seed, KnowledgeModelEditor { editor | chapters = newChapters }, event, False )

        DeleteChapter index ->
            case getAt index editor.chapters of
                Just (ChapterEditor chapterEditor) ->
                    let
                        newChapters =
                            List.take index editor.chapters ++ List.drop (index + 1) editor.chapters

                        ( event, newSeed ) =
                            createDeleteChapterEvent editor.knowledgeModel seed chapterEditor.chapter
                    in
                    ( newSeed, KnowledgeModelEditor { editor | chapters = newChapters }, Just event, False )

                Nothing ->
                    ( seed, originalEditor, Nothing, False )

        ReorderChapterList newChapters ->
            ( seed, KnowledgeModelEditor { editor | chapters = newChapters, chaptersDirty = True }, Nothing, False )

        ChapterMsg uuid chapterMsg ->
            let
                ( newSeed, newChapters, event ) =
                    updateInList editor.chapters seed (\(ChapterEditor e) -> e.chapter.uuid == uuid) (updateChapter editor.knowledgeModel chapterMsg)
            in
            ( newSeed, KnowledgeModelEditor { editor | chapters = newChapters }, event, False )


updateChapter : KnowledgeModel -> ChapterMsg -> Seed -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
updateChapter knowledgeModel msg seed ((ChapterEditor editor) as originalEditor) =
    case msg of
        ChapterFormMsg formMsg ->
            case ( formMsg, Form.getOutput editor.form, formChanged editor.form ) of
                ( Form.Submit, Just chapterForm, True ) ->
                    let
                        newChapter =
                            updateChapterEditorWithForm editor.chapter chapterForm

                        newForm =
                            chapterFormInitials newChapter |> initForm chapterFormValidation

                        newEditor =
                            { editor | active = False, form = newForm, chapter = newChapter }

                        ( event, newSeed ) =
                            createEditChapterEvent knowledgeModel seed newChapter
                    in
                    ( newSeed, ChapterEditor newEditor, Just event )

                ( Form.Submit, Just chapterForm, False ) ->
                    ( seed, ChapterEditor { editor | active = False }, Nothing )

                _ ->
                    let
                        newForm =
                            Form.update chapterFormValidation formMsg editor.form
                    in
                    ( seed, ChapterEditor { editor | form = newForm }, Nothing )

        ChapterCancel ->
            let
                newForm =
                    chapterFormInitials editor.chapter |> initForm chapterFormValidation
            in
            ( seed, ChapterEditor { editor | active = False, form = newForm }, Nothing )

        _ ->
            ( seed, originalEditor, Nothing )


updateInList : List t -> Seed -> (t -> Bool) -> (Seed -> t -> ( Seed, t, Maybe Event )) -> ( Seed, List t, Maybe Event )
updateInList list seed predicate updateFunction =
    let
        fn =
            \item ( currentSeed, items, currentEvent ) ->
                if predicate item then
                    let
                        ( updatedSeed, updatedItem, event ) =
                            updateFunction seed item
                    in
                    ( updatedSeed, items ++ [ updatedItem ], event )
                else
                    ( currentSeed, items ++ [ item ], currentEvent )
    in
    List.foldl fn ( seed, [], Nothing ) list


formChanged : Form () a -> Bool
formChanged form =
    Set.size (Form.getChangedFields form) > 0


update : Msg -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg seed session model =
    case msg of
        GetKnowledgeModelCompleted result ->
            getKnowledgeModelCompleted model result |> tuplePrepend seed

        Edit knowledgeModelMsg ->
            updateEdit knowledgeModelMsg seed session model

        SaveCompleted result ->
            postEventsBulkCompleted model result |> tuplePrepend seed

        ReorderableMsg reorderableMsg ->
            let
                newReorderableState =
                    Reorderable.update reorderableMsg model.reorderableState
            in
            ( seed, { model | reorderableState = newReorderableState }, Cmd.none )
