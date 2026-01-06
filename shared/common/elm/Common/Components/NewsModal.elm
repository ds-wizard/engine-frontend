module Common.Components.NewsModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , initialModel
    , open
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError exposing (ApiError)
import Common.Components.FontAwesome exposing (faNext, faPrev)
import Common.Components.Modal as Modal
import Common.Components.NewsModal.Api as NewsModalApi
import Common.Components.NewsModal.Models.New exposing (New)
import Common.Utils.Markdown as Markdown
import Gettext exposing (gettext)
import Html exposing (Html, button, div, h5, img, text)
import Html.Attributes exposing (class, disabled, src)
import Html.Events exposing (onClick)
import Html.Extra as Html
import List.Extensions as List
import List.Extra as List
import Maybe.Extra as Maybe
import Shortcut
import Task.Extra as Task


type alias Model =
    { news : ActionResult (List New)
    , currentIndex : Int
    , closed : Bool
    }


initialModel : Model
initialModel =
    { news = ActionResult.Unset
    , currentIndex = 0
    , closed = False
    }


init : Maybe String -> String -> ( Model, Cmd Msg )
init mbNewsUrl version =
    case mbNewsUrl of
        Just newsUrl ->
            ( { initialModel
                | news = ActionResult.Loading
              }
            , NewsModalApi.getNews newsUrl version GetNewsComplete
            )

        Nothing ->
            ( initialModel, Cmd.none )


type Msg
    = GetNewsComplete (Result ApiError (List New))
    | SetIndex Int
    | SetClosed Bool


open : Msg
open =
    SetClosed False


type alias UpdateConfig msg =
    { lastSeenId : Maybe String
    , setLastSeenMsg : String -> msg
    , wrapMsg : Msg -> msg
    }


update : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update cfg msg model =
    case msg of
        GetNewsComplete result ->
            case result of
                Ok newsList ->
                    let
                        wasLastNewsSeen =
                            Maybe.isJust cfg.lastSeenId && (cfg.lastSeenId == Maybe.map .id (List.head newsList))
                    in
                    ( { model | news = ActionResult.Success newsList, closed = wasLastNewsSeen }, Cmd.none )

                Err _ ->
                    ( { model | news = ActionResult.Error "" }, Cmd.none )

        SetIndex index ->
            ( { model | currentIndex = index }, Cmd.none )

        SetClosed closed ->
            let
                setSeenCmd =
                    if closed then
                        case Maybe.andThen List.head (ActionResult.toMaybe model.news) of
                            Just lastNew ->
                                if Just lastNew.id /= cfg.lastSeenId then
                                    Task.dispatch (cfg.setLastSeenMsg lastNew.id)

                                else
                                    Cmd.none

                            Nothing ->
                                Cmd.none

                    else
                        Cmd.none
            in
            ( { model | closed = closed, currentIndex = 0 }
            , setSeenCmd
            )


view : Gettext.Locale -> Model -> Html Msg
view locale model =
    let
        ( modalContent, shortcuts ) =
            case model.news of
                ActionResult.Success newsList ->
                    case List.getAt model.currentIndex newsList of
                        Nothing ->
                            ( [], [] )

                        Just new ->
                            let
                                hasPrev =
                                    model.currentIndex < List.length newsList - 1

                                hasNext =
                                    model.currentIndex > 0

                                prevShortcut =
                                    Shortcut.simpleShortcut Shortcut.ArrowLeft (SetIndex (model.currentIndex + 1))

                                nextShortcut =
                                    Shortcut.simpleShortcut Shortcut.ArrowRight (SetIndex (model.currentIndex - 1))
                            in
                            ( [ div
                                    [ class "modal-header" ]
                                    [ h5 [ class "modal-title" ] [ text new.title ] ]
                              , div
                                    [ class "modal-body" ]
                                    [ img [ class "border-bottom", src new.image ] []
                                    , Markdown.toHtml [] new.content
                                    ]
                              , div
                                    [ class "modal-footer" ]
                                    [ button
                                        [ class "btn btn-primary"
                                        , onClick (SetClosed True)
                                        ]
                                        [ text (gettext "Continue" locale)
                                        ]
                                    , div [ class "d-flex gap-2" ]
                                        [ Html.viewIf (List.length newsList > 1) <|
                                            button
                                                [ class "btn btn-outline-secondary with-icon"
                                                , onClick (SetIndex (model.currentIndex + 1))
                                                , disabled (not hasPrev)
                                                ]
                                                [ faPrev
                                                , text (gettext "Previous" locale)
                                                ]
                                        , Html.viewIf hasNext <|
                                            button
                                                [ class "btn btn-outline-secondary with-icon-after"
                                                , onClick (SetIndex (model.currentIndex - 1))
                                                ]
                                                [ text (gettext "Next" locale)
                                                , faNext
                                                ]
                                        ]
                                    ]
                              ]
                            , []
                                |> List.insertIf prevShortcut hasPrev
                                |> List.insertIf nextShortcut hasNext
                            )

                _ ->
                    ( [], [] )

        modalConfig =
            { modalContent = modalContent
            , visible = ActionResult.isSuccess model.news && not model.closed
            , enterMsg = Just (SetClosed True)
            , escMsg = Just (SetClosed True)
            , dataCy = "news_modal"
            }
    in
    Modal.simpleWithAttrsAndShortcuts [ class "modal-news" ] shortcuts modalConfig
