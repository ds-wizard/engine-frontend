module Common.Components.TypeHintInput exposing
    ( Model
    , Msg(..)
    , UpdateCofnig
    , ViewConfig
    , clear
    , init
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Browser.Events
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Components.FontAwesome exposing (faError, faRemove, faSearch, faSpinner)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Ports.Dom as Dom
import Debounce exposing (Debounce)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, input, li, text, ul)
import Html.Attributes exposing (class, classList, id, tabindex, type_, value)
import Html.Events exposing (onBlur, onFocus, onInput, onMouseDown, stopPropagationOn)
import Html.Extra as Html
import Json.Decode as D exposing (Decoder)
import List.Extra as List
import Maybe.Extra as Maybe
import Shortcut
import Task.Extra as Task



-- MODEL


type alias Model a =
    { typehints : Maybe (ActionResult (Pagination a))
    , typehintFocus : Maybe Int
    , q : String
    , debounce : Debounce String
    , selected : Maybe a
    , fieldId : String
    }


init : String -> Model a
init fieldId =
    { typehints = Nothing
    , typehintFocus = Nothing
    , q = ""
    , debounce = Debounce.init
    , selected = Nothing
    , fieldId = fieldId
    }


clear : Model a -> Model a
clear model =
    { model | selected = Nothing }



-- UPDATE


type Msg a
    = ShowTypeHints
    | HideTypeHints
    | TypeHintInput String
    | TypeHintsLoaded String (Result ApiError (Pagination a))
    | SetReply a
    | ClearReply
    | DebounceMsg Debounce.Msg
    | SetTypehintFocusNext
    | SetTypehintFocusPrev
    | SelectTypehintByFocus


type alias UpdateCofnig a msg =
    { wrapMsg : Msg a -> msg
    , getTypeHints : PaginationQueryString -> (Result ApiError (Pagination a) -> Msg a) -> Cmd (Msg a)
    , getError : String
    , setReply : a -> msg
    , clearReply : Maybe msg
    , filterResults : Maybe (a -> Bool)
    }


update : UpdateCofnig a msg -> Msg a -> Model a -> ( Model a, Cmd msg )
update cfg msg model =
    case msg of
        ShowTypeHints ->
            ( { model | typehints = Just Loading }
            , Cmd.batch
                [ Cmd.map cfg.wrapMsg (loadTypeHints cfg model.q)
                , Dom.focus ("#" ++ model.fieldId ++ "_search")
                ]
            )

        HideTypeHints ->
            ( { model | typehints = Nothing }, Cmd.none )

        TypeHintInput input ->
            let
                ( debounce, debounceCmd ) =
                    Debounce.push debounceConfig input model.debounce
            in
            ( { model | debounce = debounce, q = input, typehints = Just Loading }, Cmd.map cfg.wrapMsg debounceCmd )

        SetReply item ->
            ( { model | selected = Just item, typehints = Nothing, q = "" }
            , Task.dispatch (cfg.setReply item)
            )

        ClearReply ->
            ( { model | selected = Nothing }, Maybe.unwrap Cmd.none Task.dispatch cfg.clearReply )

        DebounceMsg debounceMsg ->
            let
                load q =
                    loadTypeHints cfg q

                ( debounce, debounceCmd ) =
                    Debounce.update debounceConfig (Debounce.takeLast load) debounceMsg model.debounce
            in
            ( { model | debounce = debounce }, Cmd.map cfg.wrapMsg debounceCmd )

        TypeHintsLoaded q result ->
            case model.typehints of
                Just _ ->
                    if q == model.q then
                        case result of
                            Ok typehints ->
                                let
                                    filteredTypehints =
                                        case cfg.filterResults of
                                            Just filter ->
                                                { typehints | items = List.filter filter typehints.items }

                                            Nothing ->
                                                typehints
                                in
                                ( { model
                                    | typehints = Just <| Success filteredTypehints
                                    , typehintFocus = Nothing
                                  }
                                , Cmd.none
                                )

                            Err _ ->
                                ( { model | typehints = Just <| Error cfg.getError }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetTypehintFocusNext ->
            let
                newFocus =
                    case ( model.typehints, model.typehintFocus ) of
                        ( Just (Success hints), Just focus ) ->
                            if focus + 1 < List.length hints.items then
                                Just (focus + 1)

                            else
                                Just 0

                        ( Just (Success hints), Nothing ) ->
                            if not (List.isEmpty hints.items) then
                                Just 0

                            else
                                Nothing

                        _ ->
                            Nothing
            in
            ( { model | typehintFocus = newFocus }, Cmd.none )

        SetTypehintFocusPrev ->
            let
                newFocus =
                    case ( model.typehints, model.typehintFocus ) of
                        ( Just (Success hints), Just focus ) ->
                            if focus - 1 >= 0 then
                                Just (focus - 1)

                            else
                                Just (List.length hints.items - 1)

                        ( Just (Success hints), Nothing ) ->
                            if not (List.isEmpty hints.items) then
                                Just (List.length hints.items - 1)

                            else
                                Nothing

                        _ ->
                            Nothing
            in
            ( { model | typehintFocus = newFocus }, Cmd.none )

        SelectTypehintByFocus ->
            case ( model.typehints, model.typehintFocus ) of
                ( Just (Success hints), Just focus ) ->
                    case List.getAt focus hints.items of
                        Just item ->
                            ( { model | selected = Just item, typehints = Nothing, q = "" }
                            , Task.dispatch (cfg.setReply item)
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


loadTypeHints : UpdateCofnig a msg -> String -> Cmd (Msg a)
loadTypeHints cfg q =
    cfg.getTypeHints (PaginationQueryString.fromQ q) (TypeHintsLoaded q)


debounceConfig : Debounce.Config (Msg a)
debounceConfig =
    { strategy = Debounce.later 200
    , transform = DebounceMsg
    }



-- SUBSCRIPTIONS


subscriptions : Model a -> Sub (Msg a)
subscriptions model =
    if Maybe.isJust model.typehints then
        Browser.Events.onClick <| D.map (always HideTypeHints) (succeedIfClickOutside model.fieldId)

    else
        Sub.none


succeedIfClickOutside : String -> Decoder ()
succeedIfClickOutside targetId =
    onClickDecoder targetId
        |> D.field "target"
        |> invertDecoder


onClickDecoder : String -> Decoder ()
onClickDecoder targetId =
    D.field "id" D.string
        |> D.andThen
            (\id ->
                if id == targetId then
                    D.succeed ()

                else
                    D.field "parentNode" (onClickDecoder targetId)
            )


invertDecoder : Decoder a -> Decoder ()
invertDecoder decoder =
    D.maybe decoder
        |> D.andThen
            (\maybe ->
                if maybe == Nothing then
                    D.succeed ()

                else
                    D.fail ""
            )



-- VIEW


type alias ViewConfig a msg =
    { viewItem : a -> Html msg
    , wrapMsg : Msg a -> msg
    , nothingSelectedItem : Html msg
    , clearEnabled : Bool
    , locale : Gettext.Locale
    }


view : ViewConfig a msg -> Model a -> Bool -> Html msg
view cfg model isInvalid =
    let
        isOpen =
            Maybe.isJust model.typehints

        value =
            case model.selected of
                Just item ->
                    let
                        clearButton =
                            if cfg.clearEnabled then
                                a
                                    [ stopPropagationOn "click" (D.succeed ( cfg.wrapMsg ClearReply, True ))
                                    , class "ms-2"
                                    ]
                                    [ faRemove ]

                            else
                                Html.nothing
                    in
                    [ cfg.viewItem item
                    , clearButton
                    ]

                Nothing ->
                    [ cfg.nothingSelectedItem ]

        typeHintInputTabinex =
            if isOpen then
                -1

            else
                0
    in
    div
        [ class "typehint-input"
        , classList [ ( "is-invalid", isInvalid ) ]
        , id model.fieldId
        ]
        [ div
            [ class "typehint-input-value form-control"
            , classList
                [ ( "is-invalid", isInvalid )
                , ( "focus", isOpen )
                ]
            , tabindex typeHintInputTabinex
            , onFocus (cfg.wrapMsg ShowTypeHints)
            ]
            value
        , viewTypeHints cfg model
        ]


viewTypeHints : ViewConfig a msg -> Model a -> Html msg
viewTypeHints cfg model =
    if Maybe.isJust model.typehints then
        let
            content =
                case Maybe.withDefault Unset model.typehints of
                    Success hints ->
                        if List.isEmpty hints.items then
                            div [ class "typehints-empty" ] [ text (gettext "No results matching your search were found." cfg.locale) ]

                        else
                            ul [] (List.indexedMap (viewTypeHint cfg model) hints.items)

                    Loading ->
                        div [ class "typehints-loading" ]
                            [ div [ class "loader" ]
                                [ faSpinner
                                , text (gettext "Loading..." cfg.locale)
                                ]
                            ]

                    Error err ->
                        div [ class "typehints-error" ]
                            [ faError
                            , text err
                            ]

                    Unset ->
                        Html.nothing

            shortcuts =
                [ Shortcut.simpleShortcut Shortcut.ArrowDown (cfg.wrapMsg SetTypehintFocusNext)
                , Shortcut.simpleShortcut Shortcut.ArrowUp (cfg.wrapMsg SetTypehintFocusPrev)
                , Shortcut.simpleShortcut Shortcut.Enter (cfg.wrapMsg SelectTypehintByFocus)
                , Shortcut.simpleShortcut Shortcut.Escape (cfg.wrapMsg HideTypeHints)
                ]
        in
        Shortcut.shortcutElement shortcuts
            [ class "typehints" ]
            [ div [ class "typehints-search" ]
                [ faSearch
                , input
                    [ class "form-control"
                    , type_ "text"
                    , onInput (cfg.wrapMsg << TypeHintInput)
                    , id (model.fieldId ++ "_search")
                    , value model.q
                    , onBlur (cfg.wrapMsg HideTypeHints)
                    ]
                    []
                ]
            , content
            ]

    else
        Html.nothing


viewTypeHint : ViewConfig a msg -> Model a -> Int -> a -> Html msg
viewTypeHint cfg model i item =
    li []
        [ a
            [ onMouseDown (cfg.wrapMsg <| SetReply item)
            , classList [ ( "selected", model.typehintFocus == Just i ) ]
            ]
            [ cfg.viewItem item ]
        ]
