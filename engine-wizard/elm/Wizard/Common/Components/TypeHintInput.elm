module Wizard.Common.Components.TypeHintInput exposing
    ( Model
    , Msg
    , UpdateCofnig
    , ViewConfig
    , clear
    , init
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Browser.Dom as Dom
import Browser.Events
import Debounce exposing (Debounce)
import Html exposing (Attribute, Html, a, div, input, li, text, ul)
import Html.Attributes exposing (class, classList, id, type_, value)
import Html.Events exposing (onClick, onInput, onMouseDown, stopPropagationOn)
import Json.Decode as D exposing (Decoder)
import Maybe.Extra as Maybe
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Error.ApiError exposing (ApiError)
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Locale exposing (lgx)
import Shared.Utils exposing (dispatch)
import Task
import Wizard.Common.AppState exposing (AppState)



-- MODEL


type alias Model a =
    { typehints : Maybe (ActionResult (Pagination a))
    , q : String
    , debounce : Debounce String
    , dropdownState : Dropdown.State
    , selected : Maybe a
    , fieldId : String
    }


init : String -> Model a
init fieldId =
    { typehints = Nothing
    , q = ""
    , debounce = Debounce.init
    , dropdownState = Dropdown.initialState
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
    | DropdownMsg Dropdown.State
    | NoOp


type alias UpdateCofnig a msg =
    { wrapMsg : Msg a -> msg
    , getTypeHints : PaginationQueryString -> AppState -> (Result ApiError (Pagination a) -> Msg a) -> Cmd (Msg a)
    , getError : String
    , setReply : a -> msg
    , clearReply : Maybe msg
    , filterResults : Maybe (a -> Bool)
    }


update : UpdateCofnig a msg -> Msg a -> AppState -> Model a -> ( Model a, Cmd msg )
update cfg msg appState model =
    case msg of
        ShowTypeHints ->
            ( { model | typehints = Just Loading }
            , Cmd.batch
                [ Cmd.map cfg.wrapMsg (loadTypeHints cfg appState model.q)
                , Task.attempt (\_ -> cfg.wrapMsg NoOp) (Dom.focus (model.fieldId ++ "-search"))
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
            , dispatch (cfg.setReply item)
            )

        ClearReply ->
            ( { model | selected = Nothing }, Maybe.unwrap Cmd.none dispatch cfg.clearReply )

        DebounceMsg debounceMsg ->
            let
                load q =
                    loadTypeHints cfg appState q

                ( debounce, debounceCmd ) =
                    Debounce.update debounceConfig (Debounce.takeLast load) debounceMsg model.debounce
            in
            ( { model | debounce = debounce }, Cmd.map cfg.wrapMsg debounceCmd )

        DropdownMsg state ->
            ( { model | dropdownState = state }, Cmd.none )

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
                                ( { model | typehints = Just <| Success filteredTypehints }, Cmd.none )

                            Err _ ->
                                ( { model | typehints = Just <| Error cfg.getError }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


loadTypeHints : UpdateCofnig a msg -> AppState -> String -> Cmd (Msg a)
loadTypeHints cfg appState q =
    cfg.getTypeHints (PaginationQueryString.fromQ q) appState (TypeHintsLoaded q)


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


succeedIfClickOutside : String -> Decoder ()
succeedIfClickOutside targetId =
    onClickDecoder targetId
        |> D.field "target"
        |> invertDecoder



-- VIEW


type alias ViewConfig a msg =
    { viewItem : a -> Html msg
    , wrapMsg : Msg a -> msg
    , nothingSelectedItem : Html msg
    , clearEnabled : Bool
    }


view : AppState -> ViewConfig a msg -> Model a -> Bool -> Html msg
view appState cfg model isInvalid =
    let
        value =
            case model.selected of
                Just item ->
                    let
                        clearButton =
                            if cfg.clearEnabled then
                                a
                                    [ stopPropagationOn "click" (D.succeed ( cfg.wrapMsg ClearReply, True ))
                                    ]
                                    [ faSet "_global.remove" appState ]

                            else
                                emptyNode
                    in
                    [ cfg.viewItem item
                    , clearButton
                    ]

                Nothing ->
                    [ cfg.nothingSelectedItem ]
    in
    div [ class "TypeHintInput", classList [ ( "is-invalid", isInvalid ) ], id model.fieldId ]
        [ div
            [ class "TypeHintInput__Value form-control"
            , classList [ ( "is-invalid", isInvalid ) ]
            , onClick (cfg.wrapMsg ShowTypeHints)
            ]
            value
        , viewTypeHints appState cfg model
        ]


viewTypeHints : AppState -> ViewConfig a msg -> Model a -> Html msg
viewTypeHints appState cfg model =
    let
        content =
            case Maybe.withDefault Unset model.typehints of
                Success hints ->
                    ul [] (List.map (viewTypeHint cfg) hints.items)

                Loading ->
                    div [ class "loading" ]
                        [ faSet "_global.spinner" appState
                        , lgx "loading" appState
                        ]

                Error err ->
                    div [ class "error" ]
                        [ faSet "_global.error" appState
                        , text err
                        ]

                Unset ->
                    emptyNode
    in
    if Maybe.isJust model.typehints then
        div [ class "TypeHintInput__TypeHints" ]
            [ div [ class "TypeHintInput__TypeHints__Search" ]
                [ fa "fas fa-search"
                , input
                    [ class " form-control"
                    , type_ "text"
                    , onInput (cfg.wrapMsg << TypeHintInput)
                    , id (model.fieldId ++ "-search")
                    , value model.q
                    ]
                    []
                ]
            , content
            ]

    else
        emptyNode


viewTypeHint : ViewConfig a msg -> a -> Html msg
viewTypeHint cfg item =
    li []
        [ a [ onMouseDown (cfg.wrapMsg <| SetReply item) ] [ cfg.viewItem item ] ]
