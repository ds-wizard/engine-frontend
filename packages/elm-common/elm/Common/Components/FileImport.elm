module Common.Components.FileImport exposing
    ( Model
    , Msg
    , UpdateConfig
    , ViewConfig
    , init
    , subscriptions
    , update
    , view
    )

{-| Importing one or more files: a knowledge model bundle, a document template
archive, a locale.

Choose or drop several files, see them listed, import them all, see which ones
failed, try again for those — built on `Ui.FileUpload`, so that:

  - **The button is the control.** Dropping is an extra way in; nothing needs a
    pointer (2.1.1, 2.5.7).
  - **Each file's state is words, not only an icon.** The existing list showed a
    colored icon with the error in a hover tooltip, which said nothing to a
    screen reader or to anyone who cannot tell red from green (1.4.1), and put
    the reason where only a mouse could reach it. Here each row has a badge
    ("Importing", "Imported", "Failed") and a failed row says why underneath.
  - **The outcome is announced once.** All imported is a polite `Ui.Alert`;
    some failed is an assertive one, because it asks for something to be done.
  - **Progress is real or absent.** The upload request reports no byte
    progress, so a single file shows a busy Import button rather than a bar
    with an invented value. Several files do have honest progress — how many of
    them are done — and that is a `Ui.ProgressBar`.
  - **Focus is not lost.** Cancel and a finished import both remove the button
    that had focus; focus moves to the page heading, the start of the region
    that changed, instead of falling to the document.

Cmd/Ctrl+Enter imports, through `subscriptions`, while there is something to
import.

-}

import ActionResult exposing (ActionResult)
import Browser.Dom as Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Request exposing (ToMsg)
import Common.Components.UiWords as UiWords
import Common.Data.Context exposing (WithLocale, WithNavigator)
import Common.Utils.RequestHelpers as RequestHelpers
import Dict exposing (Dict)
import File exposing (File)
import Gettext exposing (gettext)
import Html exposing (Html, text)
import List.Extra as List
import Maybe.Extra as Maybe
import Task
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.FileUpload as FileUpload
import Ui.Form as Form
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.ListItem as ListItem
import Ui.ProgressBar as ProgressBar
import Ui.Text as Text


type Model
    = Model
        { upload : FileUpload.Model

        -- Keyed by the file's position in the chosen list, not its name: two
        -- files with the same name from different folders are two imports.
        , results : Dict Int (ActionResult ())
        }


init : Model
init =
    Model { upload = FileUpload.init, results = Dict.empty }


type Msg
    = UploadMsg FileUpload.Msg
    | Submit
    | Cancel
    | Completed Int (Result ApiError ())
    | Noop


type alias UpdateConfig msg =
    { mimes : List String
    , upload : File -> ToMsg () msg -> Cmd msg
    , wrapMsg : Msg -> msg
    , logoutMsg : msg
    , getError : String
    }


update : WithLocale b -> UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update appState cfg msg (Model model) =
    case msg of
        UploadMsg uploadMsg ->
            let
                ( upload, uploadCmd ) =
                    FileUpload.updateMultiple { mimes = cfg.mimes } uploadMsg model.upload
            in
            ( Model { model | upload = upload }, Cmd.map (cfg.wrapMsg << UploadMsg) uploadCmd )

        Submit ->
            if anyLoading model.results || allImported (Model model) then
                ( Model model, Cmd.none )

            else
                let
                    -- A file that already went in is not sent again when the
                    -- rest are retried.
                    pending =
                        FileUpload.files model.upload
                            |> List.indexedMap Tuple.pair
                            |> List.filterNot (\( index, _ ) -> Maybe.unwrap False ActionResult.isSuccess (Dict.get index model.results))

                    results =
                        List.foldl (\( index, _ ) -> Dict.insert index ActionResult.Loading) model.results pending
                in
                ( Model { model | results = results }
                , pending
                    |> List.map (\( index, file ) -> cfg.upload file (cfg.wrapMsg << Completed index))
                    |> Cmd.batch
                )

        Cancel ->
            ( init, Cmd.map cfg.wrapMsg focusHeading )

        Completed index result ->
            let
                outcome =
                    case result of
                        Ok _ ->
                            ActionResult.Success ()

                        Err error ->
                            ApiError.toActionResult appState cfg.getError error

                next =
                    Model { model | results = Dict.insert index outcome model.results }
            in
            ( next
            , Cmd.batch
                [ RequestHelpers.getResultCmd cfg.logoutMsg result
                , if allImported next then
                    Cmd.map cfg.wrapMsg focusHeading

                  else
                    Cmd.none
                ]
            )

        Noop ->
            ( Model model, Cmd.none )


focusHeading : Cmd Msg
focusHeading =
    Task.attempt (always Noop) (Dom.focus AppShell.mainHeadingId)


anyLoading : Dict Int (ActionResult ()) -> Bool
anyLoading results =
    List.any ActionResult.isLoading (Dict.values results)


allImported : Model -> Bool
allImported (Model model) =
    let
        count =
            List.length (FileUpload.files model.upload)
    in
    (count > 0)
        && (Dict.size model.results == count)
        && List.all ActionResult.isSuccess (Dict.values model.results)


{-| The submit shortcut, only while there are files waiting to be imported. A
shortcut that stays bound on an empty drop area or after everything is done
would fire on a form that is not there.
-}
subscriptions : WithNavigator a -> Model -> Sub Msg
subscriptions appState (Model model) =
    if List.isEmpty (FileUpload.files model.upload) || anyLoading model.results || allImported (Model model) then
        Sub.none

    else
        Form.submitShortcut { isMac = appState.navigator.isMac, onSubmit = Submit }


type alias ViewConfig msg =
    { label : String
    , hint : String
    , doneUrl : Maybe String
    , doneLabel : String
    , validate : Maybe (File -> Maybe String)
    , wrapMsg : Msg -> msg
    }


view : WithLocale b -> ViewConfig msg -> Model -> Html msg
view appState cfg (Model model) =
    case FileUpload.files model.upload of
        [] ->
            FileUpload.toHtmlWithWords (UiWords.fileUpload appState)
                { toMsg = cfg.wrapMsg << UploadMsg
                , label = cfg.label
                , buttonLabel = gettext "Choose files" appState.locale
                , hint = cfg.hint
                , invalid = False
                , onClear = Nothing
                }
                model.upload

        files ->
            viewFiles appState cfg (Model model) files


viewFiles : WithLocale b -> ViewConfig msg -> Model -> List File -> Html msg
viewFiles appState cfg (Model model) files =
    let
        results =
            Dict.values model.results

        loading =
            anyLoading model.results

        outcome =
            if allImported (Model model) then
                Alert.toHtml (Alert.success (gettext "All files were uploaded successfully." appState.locale))

            else if not loading && List.any isError results then
                Alert.toHtml (Alert.danger (gettext "Unable to upload some files." appState.locale))

            else
                text ""

        progress =
            if loading && List.length files > 1 then
                ProgressBar.toHtmlWithWords (UiWords.progressBar appState)
                    { label = gettext "Uploaded files" appState.locale
                    , value = toFloat (List.count ActionResult.isSuccess results + List.count isError results)
                    , max = toFloat (List.length files)
                    , tone = ProgressBar.Brand
                    }

            else
                text ""

        actions =
            if allImported (Model model) then
                case cfg.doneUrl of
                    Just url ->
                        Form.actions [ Button.linkPrimary cfg.doneLabel url |> Button.withTestId "file-import_done" ]

                    Nothing ->
                        text ""

            else
                Form.footer
                    { cancel =
                        Button.ghost (gettext "Cancel" appState.locale) (cfg.wrapMsg Cancel)
                            |> Button.withDisabled loading
                    , submit =
                        Button.primary (gettext "Import" appState.locale) (cfg.wrapMsg Submit)
                            |> Button.withLoading loading
                    }
    in
    Layout.stack
        [ outcome
        , ListItem.tiles (List.indexedMap (viewFile appState cfg (Model model)) files)
        , progress
        , actions
        ]


isError : ActionResult a -> Bool
isError result =
    case result of
        ActionResult.Error _ ->
            True

        _ ->
            False


viewFile : WithLocale b -> ViewConfig msg -> Model -> Int -> File -> ListItem.ListItem msg
viewFile appState cfg (Model model) index file =
    let
        ( badge, detail ) =
            case Dict.get index model.results of
                Just ActionResult.Loading ->
                    ( Badge.neutral (gettext "Importing" appState.locale), Nothing )

                Just (ActionResult.Success _) ->
                    ( Badge.success (gettext "Imported" appState.locale), Nothing )

                Just (ActionResult.Error error) ->
                    ( Badge.danger (gettext "Failed" appState.locale), Just (Text.error error) )

                _ ->
                    case Maybe.andThen (\validate -> validate file) cfg.validate of
                        Just message ->
                            ( Badge.warning (gettext "Check the file" appState.locale), Just (Text.bodySmall message) )

                        Nothing ->
                            ( Badge.neutral (gettext "Ready" appState.locale), Nothing )

        row =
            ListItem.item (text (File.name file))
                |> ListItem.withIcon (Icon.toHtml (Icon.solid "fa-file"))
                |> ListItem.withFragments [ Badge.toHtml badge ]
                |> ListItem.withTestId ("import-files_item_" ++ File.name file)
    in
    case detail of
        Just message ->
            ListItem.withDetail message row

        Nothing ->
            row
