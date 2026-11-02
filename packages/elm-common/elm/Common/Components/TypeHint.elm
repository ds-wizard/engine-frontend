module Common.Components.TypeHint exposing
    ( Model
    , Msg
    , UpdateConfig
    , ViewConfig
    , init
    , selected
    , setSelected
    , subscriptions
    , update
    , view
    , viewCompact
    )

{-| A form field that suggests items from the API: a knowledge model, a project
template, a user.

It is built on `Ui.Combobox`. The combobox owns
the interaction and the accessibility — the trigger that shows the value, the
search field that is the `combobox`, the listbox, the busy state and the
announced count. What this module adds is everything a combobox is not allowed
to know: the request, the debounce, the stale-response guard, the translations,
and turning a failed request into something the field says.

    TypeHint.view appState
        { id = "knowledgeModelPackageUuid"
        , label = gettext "Knowledge Model" appState.locale
        , placeholder = gettext "Select knowledge model" appState.locale
        , required = True
        , error = fieldError
        , hint = Nothing
        , clearable = True
        , viewItem =
            \package ->
                { label = package.name
                , code = Just package.id
                , badges = []
                , description = Just package.description
                , icon = Nothing
                }
        , wrapMsg = KnowledgeModelTypeHintMsg
        }
        model.knowledgeModelTypeHint


## Timing

Opening the popup asks at once — an empty list is not a useful thing to open
onto. Typing waits 300 ms, so a word typed quickly is one request. Responses are
kept only while their query is still the one in the field; one for an older
query that finishes late is dropped, rather than overwriting the answer to the
newer one.


## What is said

  - While a request is out the listbox is busy and a row says "Loading...".
  - When it comes back, a polite live region says how many results there are.
    The options change while focus stays in the search field, so nothing else
    would tell a screen reader the answer has arrived.
  - A failed request is said in the popup, where the eye is, and becomes the
    field's error, where it stays after the popup closes. The error is in the
    trigger's `aria-describedby`, so it is heard on the control, not only seen
    under it.
  - A validation error passed in by the form wins over a failed request; the
    request can be retried by reopening, the validation cannot.


## The reply

Choosing an item sends `setReply`, clearing sends `clearReply` — the same
contract `TypeHintInput` had, so a form field can be set from either. The
selection is compared by `toKey`, not by `==`, because API records can carry
functions or be large, and a record refetched with a new timestamp is still the
same item.

-}

import ActionResult
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Request exposing (ToMsg)
import Common.Components.ItemSummary as ItemSummary
import Common.Components.TypeHint.Internal as Internal exposing (Search)
import Common.Data.Context exposing (WithLocale)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext, ngettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Process
import String.Format as String
import Task
import Task.Extra as Task
import Ui.Combobox as Combobox
import Ui.Text as Text


type Model a
    = Model
        { combobox : Combobox.Model a
        , search : Search a
        }


init : Model a
init =
    Model
        { combobox = Combobox.init
        , search = Internal.init
        }


selected : Model a -> Maybe a
selected (Model model) =
    Combobox.selected model.combobox


{-| Set the value without anyone choosing it — the item a page was opened with,
or the one an edit form loaded. It does not send `setReply` and does not move
focus: the form already has the value, and the user is somewhere else.
-}
setSelected : Maybe a -> Model a -> Model a
setSelected value (Model model) =
    Model { model | combobox = Combobox.setSelected value model.combobox }


type Msg a
    = ComboboxMsg (Combobox.Msg a)
    | DebounceSettled Int
    | Loaded String (Result ApiError (Pagination a))


type alias UpdateConfig a msg =
    { wrapMsg : Msg a -> msg
    , getTypeHints : PaginationQueryString -> ToMsg (Pagination a) msg -> Cmd msg
    , getError : String
    , setReply : a -> msg
    , clearReply : Maybe msg
    , filterResults : Maybe (a -> Bool)
    , logoutMsg : msg
    , id : String
    , toKey : a -> String
    }


debounceMs : Float
debounceMs =
    300


update : WithLocale b -> UpdateConfig a msg -> Msg a -> Model a -> ( Model a, Cmd msg )
update appState cfg msg (Model model) =
    case msg of
        ComboboxMsg comboboxMsg ->
            let
                ( combobox, comboboxCmd ) =
                    Combobox.update cfg.id (List.length model.search.results) comboboxMsg model.combobox

                ( search, searchCmd ) =
                    case Combobox.resultsWanted { before = model.combobox, after = combobox } of
                        Just query ->
                            if not (Combobox.isOpen model.combobox) && Combobox.isOpen combobox then
                                ( Internal.requested model.search, fetch cfg query )

                            else
                                let
                                    ( typed, tag ) =
                                        Internal.typed model.search
                                in
                                ( typed
                                , Process.sleep debounceMs
                                    |> Task.perform (\_ -> cfg.wrapMsg (DebounceSettled tag))
                                )

                        Nothing ->
                            ( model.search, Cmd.none )
            in
            ( Model { model | combobox = combobox, search = search }
            , Cmd.batch
                [ Cmd.map (cfg.wrapMsg << ComboboxMsg) comboboxCmd
                , searchCmd
                , replyCmd cfg (Combobox.selected model.combobox) (Combobox.selected combobox)
                ]
            )

        DebounceSettled tag ->
            if Internal.isLatest tag model.search then
                ( Model model, fetch cfg (Combobox.query model.combobox) )

            else
                ( Model model, Cmd.none )

        Loaded query result ->
            if Internal.accepts { response = query, current = Combobox.query model.combobox } then
                case result of
                    Ok pagination ->
                        let
                            items =
                                case cfg.filterResults of
                                    Just keep ->
                                        List.filter keep pagination.items

                                    Nothing ->
                                        pagination.items
                        in
                        ( Model { model | search = Internal.received items model.search }, Cmd.none )

                    Err error ->
                        let
                            message =
                                case ApiError.toActionResult appState cfg.getError error of
                                    ActionResult.Error serverMessage ->
                                        serverMessage

                                    _ ->
                                        cfg.getError
                        in
                        ( Model { model | search = Internal.failed message model.search }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

            else
                ( Model model, RequestHelpers.getResultCmd cfg.logoutMsg result )


{-| The first page for a query. The size is the API's default, as the existing
input asks for; a caller that wants a different page — templates sorted by name,
fifty at a time — adjusts the query string in its `getTypeHints`.
-}
fetch : UpdateConfig a msg -> String -> Cmd msg
fetch cfg query =
    cfg.getTypeHints (PaginationQueryString.fromQ query) (cfg.wrapMsg << Loaded query)


replyCmd : UpdateConfig a msg -> Maybe a -> Maybe a -> Cmd msg
replyCmd cfg before after =
    if Maybe.map cfg.toKey before == Maybe.map cfg.toKey after then
        Cmd.none

    else
        case after of
            Just item ->
                Task.dispatch (cfg.setReply item)

            Nothing ->
                Maybe.unwrap Cmd.none Task.dispatch cfg.clearReply


{-| Nothing to subscribe to: the combobox closes on blur and on Escape, not on
a document-wide click listener as `TypeHintInput` did. Kept so the call sites
do not change when something does need a subscription.
-}
subscriptions : Model a -> Sub (Msg a)
subscriptions =
    always Sub.none


type alias ViewConfig a msg =
    { id : String
    , label : String
    , placeholder : String
    , required : Bool
    , error : Maybe String
    , hint : Maybe String
    , clearable : Bool
    , viewItem : a -> ItemSummary.Item
    , toKey : a -> String
    , wrapMsg : Msg a -> msg
    }


view : WithLocale b -> ViewConfig a msg -> Model a -> Html msg
view =
    viewField False


{-| The toolbar form of the field: the label is there for a screen reader and
not drawn, and an item is its name and nothing else.

A toolbar is one line tall. A label over every control would make it three, and
the full row — the round icon made from the name, the id, the sentence about the
thing — is a list of three-line items where the row itself is the height of the
bar it drops out of. What is being chosen in a toolbar is one of a handful of
things the reader already knows by name.

-}
viewCompact : WithLocale b -> ViewConfig a msg -> Model a -> Html msg
viewCompact =
    viewField True


viewField : Bool -> WithLocale b -> ViewConfig a msg -> Model a -> Html msg
viewField compact appState cfg (Model model) =
    let
        error =
            case cfg.error of
                Just validation ->
                    Just validation

                Nothing ->
                    model.search.error
    in
    Html.map (cfg.wrapMsg << ComboboxMsg) <|
        Combobox.toFieldWithHiddenLabel compact
            { hint = cfg.hint
            , error = error
            , required = cfg.required
            }
            { id = cfg.id
            , label = cfg.label
            , placeholder = cfg.placeholder
            , searchPlaceholder = gettext "Search..." appState.locale
            , options = model.search.results
            , viewOption = viewOption compact cfg
            , optionTestId = \item -> cfg.id ++ "_option_" ++ cfg.toKey item
            , viewSelected = viewSelected compact cfg
            , loading = model.search.loading
            , loadingLabel = gettext "Loading..." appState.locale

            -- A failed request is not "no results": say what went wrong where
            -- "no results" would otherwise be shown and announced.
            , emptyLabel =
                Maybe.withDefault
                    (gettext "No results matching your search were found." appState.locale)
                    model.search.error
            , resultsLabel = resultsLabel appState
            , clearLabel =
                if cfg.clearable then
                    Just (String.format (gettext "Clear %s" appState.locale) [ cfg.label ])

                else
                    Nothing
            }
            model.combobox


resultsLabel : WithLocale b -> Int -> String
resultsLabel appState count =
    String.format (ngettext ( "%s result", "%s results" ) count appState.locale) [ String.fromInt count ]


{-| A suggestion as a row of the listing it is choosing from:
`Common.Components.ItemSummary`, which a page also uses to show a thing that
was chosen and is now only to be read. It is drawn inside the trigger's
`button` as well, which is why it is the plain row and never the linked one.
-}
viewItemContent : Bool -> ViewConfig a msg -> a -> Html (Combobox.Msg a)
viewItemContent compact cfg item =
    if compact then
        Text.label (cfg.viewItem item).label

    else
        ItemSummary.view (cfg.viewItem item)


viewOption : Bool -> ViewConfig a msg -> a -> Html (Combobox.Msg a)
viewOption =
    viewItemContent


{-| The chosen value inside the trigger. The same row as in the list: what was
chosen and what it looked like when it was chosen are the same thing, and a
name alone leaves the reader checking the list again to be sure which version
they picked.
-}
viewSelected : Bool -> ViewConfig a msg -> a -> Html (Combobox.Msg a)
viewSelected =
    viewItemContent
