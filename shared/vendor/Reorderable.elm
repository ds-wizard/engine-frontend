module Reorderable exposing
    ( HtmlWrapper
    , Msg
    , State
    , ViewConfig
    , initialState
    , subscriptions
    , update
    , view
    )

import Browser.Events exposing (onMouseMove, onMouseUp)
import Html exposing (Attribute, Html, li, ul)
import Html.Attributes exposing (attribute, class, classList, style)
import Html.Events exposing (on)
import Json.Decode as D exposing (Decoder)
import List.Extra as List



{- Types -}


type Msg
    = DragStart String MouseEvent
    | DragEnd
    | DragMove MouseEvent
    | MouseOverIgnored Bool


type State
    = State
        { dragging : Maybe DraggedItem
        , mouseOverIgnored : Bool
        }


type alias DraggedItem =
    { id : String
    , position : Position
    }


type alias Position =
    { x : Int
    , y : Int
    }


type alias MouseEvent =
    { startingPosition : Position
    , movement : Position
    }


type alias ViewConfig a msg =
    { toId : a -> String
    , toMsg : Msg -> msg
    , itemView : HtmlWrapper msg -> a -> Html msg
    , placeholderView : Html msg
    , listClass : String
    , itemClass : String
    , placeholderClass : String
    , updateList : List a -> msg
    }


type alias HtmlWrapper msg =
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg



{- Model -}


initialState : State
initialState =
    State
        { dragging = Nothing
        , mouseOverIgnored = False
        }



{- Subscriptions -}


subscriptions : State -> Sub Msg
subscriptions (State state) =
    if state.dragging /= Nothing then
        Sub.batch
            [ onMouseMove (D.map DragMove mouseEventDecoder)
            , onMouseUp (D.succeed DragEnd)
            ]

    else
        Sub.none



{- Update -}


update : Msg -> State -> State
update msg (State state) =
    case msg of
        DragStart id event ->
            State
                { state
                    | dragging =
                        Just
                            { id = id
                            , position = event.startingPosition
                            }
                }

        DragEnd ->
            State { state | dragging = Nothing }

        DragMove event ->
            let
                dragging =
                    state.dragging
                        |> Maybe.map
                            (\dragged ->
                                { dragged
                                    | position = move dragged.position event.movement
                                }
                            )
            in
            State { state | dragging = dragging }

        MouseOverIgnored mouseOverIgnored ->
            State { state | mouseOverIgnored = mouseOverIgnored }



{- View -}


view : ViewConfig a msg -> State -> List a -> Html msg
view config state list =
    let
        views =
            List.concatMap (viewItem config state list) list
    in
    ul [ class config.listClass, style "position" "relative" ]
        views


viewItem : ViewConfig a msg -> State -> List a -> a -> List (Html msg)
viewItem config (State state) list item =
    let
        id =
            config.toId item

        isBeingDragged =
            state.dragging
                |> Maybe.map (.id >> (==) id)
                |> Maybe.withDefault False

        ( itemView, itemClass ) =
            if isBeingDragged then
                ( config.placeholderView, config.placeholderClass )

            else
                ( config.itemView (ignoreDrag config.toMsg) item, config.itemClass )

        draggedItemView =
            case ( isBeingDragged, state.dragging ) of
                ( True, Just draggedItem ) ->
                    [ viewDraggedItem config draggedItem item ]

                _ ->
                    []

        attrs =
            [ class itemClass
            , onDragStart state.mouseOverIgnored (config.toMsg << DragStart id)
            , onDragOver
                config.updateList
                (\() -> updateList config.toId id (Maybe.map .id state.dragging) list)
                (state.dragging /= Nothing)
            , attribute "data-cy" "reorderable_item"
            ]
    in
    li attrs [ itemView ]
        :: draggedItemView


viewDraggedItem : ViewConfig a msg -> DraggedItem -> a -> Html msg
viewDraggedItem config draggedItem item =
    li
        [ class config.itemClass
        , style "position" "absolute"
        , style "left" <| px draggedItem.position.x
        , style "top" <| px draggedItem.position.y
        , style "pointer-events" "none"
        , style "display" "block"
        , style "z-index" "10"
        ]
        [ config.itemView (ignoreDrag config.toMsg) item ]


viewPlaceholder : ViewConfig a msg -> Html msg
viewPlaceholder config =
    li [ class config.placeholderClass ] [ config.placeholderView ]


ignoreDrag : (Msg -> msg) -> HtmlWrapper msg
ignoreDrag toMsg elem attr children =
    elem
        (attr
            ++ [ on "mouseenter" <| D.succeed <| toMsg <| MouseOverIgnored True
               , on "mouseleave" <| D.succeed <| toMsg <| MouseOverIgnored False
               ]
        )
        children



{- Mouse Events -}


mouseEvent : Int -> Int -> Int -> Int -> MouseEvent
mouseEvent elemOffsetX elemOffsetY movementX movementY =
    { startingPosition = Position elemOffsetX elemOffsetY
    , movement = Position movementX movementY
    }


mouseEventDecoder : Decoder MouseEvent
mouseEventDecoder =
    D.map4 mouseEvent
        (D.at [ "srcElement", "offsetLeft" ] D.int)
        (D.at [ "srcElement", "offsetTop" ] D.int)
        (D.field "movementX" D.int)
        (D.field "movementY" D.int)


onDragStart : Bool -> (MouseEvent -> msg) -> Attribute msg
onDragStart ignored toMsg =
    mouseEventDecoder
        |> D.andThen (decodeWhen <| not ignored)
        |> D.map toMsg
        |> on "mousedown"


onDragOver : (List a -> msg) -> (() -> List a) -> Bool -> Attribute msg
onDragOver configUpdateList listThunk isDragging =
    D.succeed ()
        |> D.andThen (decodeWhen isDragging)
        |> D.andThen (D.succeed << configUpdateList << listThunk)
        |> on "mouseover"



{- Helpers -}


decodeWhen : Bool -> a -> Decoder a
decodeWhen condition x =
    if condition then
        D.succeed x

    else
        D.fail "Not this time"


move : Position -> Position -> Position
move currentPos movement =
    { x = currentPos.x + movement.x
    , y = currentPos.y + movement.y
    }


px : Int -> String
px i =
    String.fromInt i ++ "px"


moveTo : Int -> Int -> List a -> List a
moveTo startIndex destIndex list =
    let
        maybeElem =
            list
                |> List.getAt startIndex

        setElemToNewPosition xs =
            maybeElem
                |> Maybe.map (\x -> insertAt destIndex x xs)
                |> Maybe.withDefault xs
    in
    if destIndex >= List.length list then
        list

    else
        list
            |> List.removeAt startIndex
            |> setElemToNewPosition


insertAt : Int -> a -> List a -> List a
insertAt index elem list =
    if index == 0 then
        elem :: list

    else
        case list of
            [] ->
                []

            head :: tail ->
                head :: insertAt (index - 1) elem tail


updateList : (a -> String) -> String -> Maybe String -> List a -> List a
updateList toId overId dragging list =
    case dragging of
        Nothing ->
            list

        Just draggedId ->
            let
                indexEqualTo id =
                    list
                        |> List.findIndex (\a -> toId a == id)
                        |> Maybe.withDefault 0

                overIndex =
                    indexEqualTo overId

                draggedIndex =
                    indexEqualTo draggedId
            in
            list |> moveTo draggedIndex overIndex
