module Common.Components.NewsModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , initialModel
    , open
    , subscriptions
    , update
    , view
    )

{-| "What's new": the release notes, shown once after an upgrade and on demand
from the account menu.

It opens by itself only when the newest item is one this user has not seen, and
not while the dashboard tour is about to start — two things taking focus at once
is one of them lost. Closing it records the newest item as seen, through the
application, because only the application can write to the user.

The items are a sequence, newest first. Previous and Next are buttons in the
footer, where Tab reaches them after the content; the left and right arrow keys
do the same while the dialog is open, as they did before, but only as a
shortcut — nothing is reachable by the arrows alone. They stand aside while the
caret is in a text box, so they cannot take a keystroke from something that
wants it.

The picture is decorative (`alt=""`): the title and the text say what the item
is, and a description of a screenshot would repeat them. The body is markdown
under the dialog's heading, so its own headings start one level below it.

-}

import ActionResult exposing (ActionResult)
import Browser.Events
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.RolePermission exposing (RolePermission)
import Common.Api.News as NewsApi
import Common.Components.Markdown as Markdown
import Common.Data.Context exposing (WithLocale)
import Common.Data.News exposing (News)
import Gettext exposing (gettext)
import Html exposing (Html, div, img)
import Html.Attributes exposing (alt, class, src)
import Json.Decode as D
import List.Extra as List
import Maybe.Extra as Maybe
import Task
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Modal as Modal


type alias Model =
    { news : ActionResult (List News)
    , currentIndex : Int
    , closed : Bool
    }


initialModel : Model
initialModel =
    { news = ActionResult.Unset
    , currentIndex = 0
    , closed = False
    }


{-| Fetches the feed for this client version and these permissions. Without a
news URL there is nothing to fetch and the dialog never opens.
-}
init : Maybe String -> String -> List RolePermission -> ( Model, Cmd Msg )
init mbNewsUrl version permissions =
    case mbNewsUrl of
        Just newsUrl ->
            ( { initialModel | news = ActionResult.Loading }
            , NewsApi.getNews newsUrl version permissions GetNewsComplete
            )

        Nothing ->
            ( initialModel, Cmd.none )


type Msg
    = GetNewsComplete (Result ApiError (List News))
    | SetIndex Int
    | SetClosed Bool


{-| Reopen it from a menu, at the newest item.
-}
open : Msg
open =
    SetClosed False


type alias UpdateConfig msg =
    { lastSeenId : Maybe String
    , setLastSeenMsg : String -> msg
    , wrapMsg : Msg -> msg
    , dashboardTourActive : Bool
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
                    ( { model | news = ActionResult.Success newsList, closed = wasLastNewsSeen || cfg.dashboardTourActive }, Cmd.none )

                Err _ ->
                    ( { model | news = ActionResult.Error "" }, Cmd.none )

        SetIndex index ->
            ( { model | currentIndex = index }, Cmd.none )

        SetClosed closed ->
            let
                setSeenCmd =
                    if closed then
                        case Maybe.andThen List.head (ActionResult.toMaybe model.news) of
                            Just lastNews ->
                                if Just lastNews.id /= cfg.lastSeenId then
                                    Task.perform cfg.setLastSeenMsg (Task.succeed lastNews.id)

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


{-| The arrow-key shortcuts, only while the dialog is showing.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case ( isVisible model, ActionResult.toMaybe model.news ) of
        ( True, Just newsList ) ->
            Browser.Events.onKeyDown (arrowDecoder model.currentIndex (List.length newsList))

        _ ->
            Sub.none


arrowDecoder : Int -> Int -> D.Decoder Msg
arrowDecoder currentIndex count =
    D.map2 Tuple.pair
        (D.field "key" D.string)
        (D.oneOf [ D.at [ "target", "tagName" ] D.string, D.succeed "" ])
        |> D.andThen
            (\( key, tagName ) ->
                if tagName == "INPUT" || tagName == "TEXTAREA" then
                    D.fail "typing"

                else if key == "ArrowLeft" && currentIndex < count - 1 then
                    D.succeed (SetIndex (currentIndex + 1))

                else if key == "ArrowRight" && currentIndex > 0 then
                    D.succeed (SetIndex (currentIndex - 1))

                else
                    D.fail "not a shortcut"
            )


isVisible : Model -> Bool
isVisible model =
    ActionResult.isSuccess model.news && not model.closed


view : WithLocale a -> Model -> Html Msg
view appState model =
    let
        mbCurrent =
            ActionResult.toMaybe model.news
                |> Maybe.andThen (\newsList -> Maybe.map (Tuple.pair newsList) (List.getAt model.currentIndex newsList))

        ( title, body, actions ) =
            case mbCurrent of
                Just ( newsList, news ) ->
                    let
                        prevButton =
                            if List.length newsList > 1 then
                                [ Button.secondary (gettext "Previous" appState.locale) (SetIndex (model.currentIndex + 1))
                                    |> Button.withIcon (Icon.solid "fa-arrow-left")
                                    |> Button.withDisabled (model.currentIndex >= List.length newsList - 1)
                                ]

                            else
                                []

                        nextButton =
                            if model.currentIndex > 0 then
                                [ Button.secondary (gettext "Next" appState.locale) (SetIndex (model.currentIndex - 1))
                                    |> Button.withIcon (Icon.solid "fa-arrow-right")
                                ]

                            else
                                []
                    in
                    ( news.title
                    , [ img [ class "c-news-modal__image", src news.image, alt "" ] []
                      , Markdown.view { headingOffset = 2 } news.content
                      ]
                    , prevButton ++ nextButton ++ [ Button.primary (gettext "Continue" appState.locale) (SetClosed True) ]
                    )

                Nothing ->
                    ( "", [], [] )
    in
    Modal.plain
        { title = title
        , open = isVisible model && Maybe.isJust mbCurrent
        , onClose = SetClosed True
        , body = [ div [ class "c-news-modal" ] body ]
        }
        |> Modal.withWide
        |> Modal.withActions actions
        |> Modal.withTestId "news_modal"
        |> Modal.toHtml
