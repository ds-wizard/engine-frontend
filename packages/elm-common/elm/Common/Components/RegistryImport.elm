module Common.Components.RegistryImport exposing
    ( Model
    , Msg
    , UpdateConfig
    , ViewConfig
    , init
    , submittedId
    , subscriptions
    , update
    , view
    )

{-| Importing something from the registry by its id: a knowledge model, a
document template, a locale.

The three wizard pages were one form written three times — an id, Import, and
on success a sentence and a "View detail" button. Here it is one form, generic
in what the request returns (`r`), because each API answers with something
different and the success needs it for the link: the knowledge model pull a
`UuidResponse`, the locale pull the locale itself.

What is different from the wizard:

  - **An empty id is an error said beside the field.** The existing form
    ignored the submit silently, which to a keyboard or screen reader user is a
    button that does nothing. Import is never disabled for it.
  - **A server error goes to the form's error summary and focus follows it**
    (`Common.Components.Form.errorSummary`), so the reason is where the reader
    is taken, not above a button they are still sitting on.
  - **Success is announced** — a polite `Ui.Alert` — and focus moves to the page
    heading, because the form and the button that had focus are gone.
  - **The registry link says it opens a new tab**, in words a screen reader
    reads, where the existing one opened one without warning.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Request exposing (ToMsg)
import Common.Components.Form as Form
import Common.Data.Context exposing (WithLocale, WithNavigator)
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html, p)
import Maybe.Extra as Maybe
import Regex exposing (Regex)
import String.Format as String
import Task
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.Button as Button
import Ui.Field as Field
import Ui.Layout as Layout
import Ui.Link as Link


type Model r
    = Model
        { id : String
        , submitted : Bool
        , submittedId : Maybe String
        , result : ActionResult r
        }


{-| With the id the page was opened with — the registry links to
`/knowledge-models/import?id=…` — or empty.
-}
init : Maybe String -> Model r
init id =
    Model
        { id = Maybe.withDefault "" id
        , submitted = False
        , submittedId = Nothing
        , result = Unset
        }


{-| The id of the last import that was sent, as it was sent (trimmed) — for the
success message, which `ViewConfig.done` builds from what the request returned:

    done =
        \_ ->
            { message =
                String.format (gettext "%s has been imported." appState.locale)
                    [ Maybe.withDefault "" (RegistryImport.submittedId model.registryImport) ]
            , url = …
            }

It is the id that was submitted rather than the text in the field, so editing
the field while the request is out cannot change what the message names.
`Nothing` until something has been sent.

-}
submittedId : Model r -> Maybe String
submittedId (Model model) =
    model.submittedId


type Msg r
    = IdChanged String
    | Submit
    | Completed (Result ApiError r)
    | Noop


type alias UpdateConfig r msg =
    { importRequest : String -> ToMsg r msg -> Cmd msg
    , wrapMsg : Msg r -> msg
    , logoutMsg : msg
    , getError : String
    }


fieldId : String
fieldId =
    "registry-import-id"


type IdProblem
    = Empty
    | NoVersion


{-| What is imported is one version: the id and the version, `id:1.2.0`.
-}
idProblem : String -> Maybe IdProblem
idProblem id =
    if String.isEmpty id then
        Just Empty

    else if Regex.contains reference id then
        Nothing

    else
        Just NoVersion


reference : Regex
reference =
    Regex.fromString "^[A-Za-z0-9-_.]+:[0-9]+\\.[0-9]+\\.[0-9]+$"
        |> Maybe.withDefault Regex.never


update : WithLocale b -> UpdateConfig r msg -> Msg r -> Model r -> ( Model r, Cmd msg )
update appState cfg msg (Model model) =
    case msg of
        IdChanged id ->
            ( Model { model | id = id }, Cmd.none )

        Submit ->
            if ActionResult.isLoading model.result then
                ( Model model, Cmd.none )

            else if Maybe.isJust (idProblem (String.trim model.id)) then
                ( Model { model | submitted = True }
                , Task.attempt (always (cfg.wrapMsg Noop)) (Dom.focus fieldId)
                )

            else
                ( Model { model | submitted = True, submittedId = Just (String.trim model.id), result = Loading }
                , cfg.importRequest (String.trim model.id) (cfg.wrapMsg << Completed)
                )

        Completed result ->
            case result of
                Ok imported ->
                    ( Model { model | result = Success imported }
                    , Task.attempt (always (cfg.wrapMsg Noop)) (Dom.focus AppShell.mainHeadingId)
                    )

                Err error ->
                    ( Model { model | result = ApiError.toActionResult appState cfg.getError error }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , Form.focusErrorSummary
                        ]
                    )

        Noop ->
            ( Model model, Cmd.none )


{-| Cmd/Ctrl+Enter imports, while the form is showing and nothing is in flight.
-}
subscriptions : WithNavigator a -> Model r -> Sub (Msg r)
subscriptions appState (Model model) =
    case model.result of
        Success _ ->
            Sub.none

        _ ->
            Form.submitShortcut appState { result = model.result, onSubmit = Submit }


type alias ViewConfig r msg =
    { label : String

    -- An example of what to fill in, `id:version`; the error for an id without
    -- its version names it too.
    , placeholder : String

    -- "You can find knowledge models in %s.", with `%s` replaced by the link.
    , hint : String
    , registryLink : Maybe { label : String, url : String }
    , submitLabel : String

    -- What to say and where to go once `r` has been imported.
    , done : r -> { message : String, url : String }
    , doneLabel : String
    , wrapMsg : Msg r -> msg
    }


view : WithLocale b -> ViewConfig r msg -> Model r -> Html msg
view appState cfg (Model model) =
    case model.result of
        Success imported ->
            let
                done =
                    cfg.done imported
            in
            Layout.stack
                [ Alert.toHtml (Alert.success done.message)
                , Button.toHtml (Button.linkPrimary cfg.doneLabel done.url)
                ]

        _ ->
            viewForm appState cfg (Model model)


viewForm : WithLocale b -> ViewConfig r msg -> Model r -> Html msg
viewForm appState cfg (Model model) =
    let
        error =
            if model.submitted then
                Maybe.map
                    (\problem ->
                        case problem of
                            Empty ->
                                String.format (gettext "%s cannot be empty." appState.locale) [ cfg.label ]

                            NoVersion ->
                                String.format (gettext "Fill in the ID with the version, such as %s." appState.locale) [ cfg.placeholder ]
                    )
                    (idProblem (String.trim model.id))

            else
                Nothing

        registryText =
            case cfg.registryLink of
                Just registry ->
                    p []
                        (String.formatHtml cfg.hint
                            [ Link.link registry
                                |> Link.external
                                |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                                |> Link.toHtml
                            ]
                        )

                Nothing ->
                    Html.text ""
    in
    Html.map cfg.wrapMsg <|
        Form.view
            { onSubmit = Submit
            , content =
                [ Form.errorSummary model.result
                , Field.input { id = fieldId, label = cfg.label, value = model.id, onInput = IdChanged }
                    |> Field.withPlaceholder cfg.placeholder
                    |> Field.withRequired True
                    |> Field.withError error
                    |> Field.toHtml
                , registryText
                , Form.actions appState
                    { submitLabel = cfg.submitLabel
                    , result = model.result
                    , onCancel = Nothing
                    }
                ]
            }
