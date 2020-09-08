module Shared.Elemental.Components.Listing exposing (Item, Model, Msg, UpdateConfig, UpdatedTimeConfig, ViewConfig, init, setPagination, update, view)

import ActionResult exposing (ActionResult(..))
import Browser.Navigation as Nav
import Css exposing (alignItems, backgroundColor, borderBottom, borderBottom3, borderTop3, center, display, displayFlex, firstChild, flexGrow, height, hover, important, inlineBlock, justifyContent, margin, marginLeft, marginRight, minWidth, none, num, padding2, px, solid, textAlign, textDecoration, underline, width, zero)
import Css.Global as Css exposing (descendants, typeSelector)
import Debouncer.Extra as Debouncer exposing (Debouncer)
import Html.Styled exposing (Html, a, div, img, input, li, nav, span, text, ul)
import Html.Styled.Attributes exposing (class, classList, css, href, placeholder, src, type_, value)
import Html.Styled.Events exposing (onInput)
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.Pagination.Page exposing (Page)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString, SortDirection(..))
import Shared.Elemental.Atoms.FormInput as FormInput
import Shared.Elemental.Components.ActionResultWrapper as ActionResultWrapper
import Shared.Elemental.Components.Pagination as Pagination
import Shared.Elemental.Foundations.Border as Border
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (px2rem)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html.Styled exposing (emptyNode, fa)
import Shared.Utils exposing (dispatch)
import Time
import Time.Distance exposing (inWords, inWordsWithConfig)



-- MODEL


type alias Model a =
    { pagination : ActionResult (Pagination a)
    , paginationQueryString : PaginationQueryString
    , items : List (Item a)
    , qInput : String
    , qDebouncer : Debouncer (Msg a)
    }


type alias Item a =
    { item : a }


init : PaginationQueryString -> ( Model a, Cmd (Msg a) )
init paginationQueryString =
    ( { pagination = Loading
      , paginationQueryString = paginationQueryString
      , items = []
      , qInput = Maybe.withDefault "" paginationQueryString.q
      , qDebouncer = Debouncer.toDebouncer (Debouncer.debounce 500)
      }
    , dispatch Reload
    )


setPagination : Pagination a -> Model a -> Model a
setPagination pagination model =
    let
        wrap item =
            { item = item }
    in
    { model
        | pagination = Success pagination
        , items = List.map wrap pagination.items
    }



-- UPDATE


type Msg a
    = Reload
    | ReloadBackground
    | GetItemsComplete PaginationQueryString (Result ApiError (Pagination a))
    | QueryInput String
    | QueryApply String
    | DebouncerMsg (Debouncer.Msg (Msg a))


type alias UpdateConfig a appState msg =
    { getRequest : PaginationQueryString -> AbstractAppState appState -> ToMsg (Pagination a) (Msg a) -> Cmd (Msg a)
    , getError : String
    , wrapMsg : Msg a -> msg
    , updateUrlCmd : PaginationQueryString -> Cmd msg
    }


update : UpdateConfig a appState msg -> AbstractAppState appState -> Msg a -> Model a -> ( Model a, Cmd msg )
update cfg appState msg model =
    case msg of
        Reload ->
            ( { model | pagination = Loading, items = [] }
            , Cmd.map cfg.wrapMsg <| cfg.getRequest model.paginationQueryString appState (GetItemsComplete model.paginationQueryString)
            )

        ReloadBackground ->
            ( model
            , Cmd.map cfg.wrapMsg <| cfg.getRequest model.paginationQueryString appState (GetItemsComplete model.paginationQueryString)
            )

        GetItemsComplete paginationQueryString result ->
            case result of
                Ok pagination ->
                    if model.paginationQueryString == paginationQueryString then
                        ( setPagination pagination model, Cmd.none )

                    else
                        ( model, Cmd.none )

                Err error ->
                    ( { model | pagination = ApiError.toActionResult cfg.getError error }
                      -- TODO maybe logout
                    , Cmd.none
                    )

        QueryInput string ->
            ( { model | qInput = string }
            , dispatch (cfg.wrapMsg <| DebouncerMsg <| Debouncer.provideInput <| QueryApply string)
            )

        QueryApply string ->
            let
                paginationQueryString =
                    model.paginationQueryString

                newModel =
                    { model
                        | pagination = Loading
                        , items = []
                        , paginationQueryString = { paginationQueryString | q = Just string, page = Just 1 }
                    }
            in
            ( newModel
              --, cfg.cmdNavigate { paginationQueryString | q = Just string, page = Just 1 }
            , Cmd.batch
                [ Cmd.map cfg.wrapMsg <| cfg.getRequest newModel.paginationQueryString appState (GetItemsComplete newModel.paginationQueryString)
                , cfg.updateUrlCmd newModel.paginationQueryString
                ]
            )

        DebouncerMsg debounceMsg ->
            let
                updateConfig =
                    { mapMsg = cfg.wrapMsg << DebouncerMsg
                    , getDebouncer = .qDebouncer
                    , setDebouncer = \debouncer m -> { m | qDebouncer = debouncer }
                    }
            in
            Debouncer.update (update cfg appState) updateConfig debounceMsg model



-- VIEW


type alias ViewConfig a msg =
    { title : a -> Html msg
    , description : a -> Html msg

    --, dropdownItems : a -> List (ListingDropdownItem msg)
    , textTitle : a -> String
    , emptyText : String
    , updated : Maybe (UpdatedTimeConfig a)

    --, iconView : Maybe (a -> Html msg)
    , sortOptions : List ( String, String )
    , wrapMsg : Msg a -> msg
    , toRoute : PaginationQueryString -> String

    --, toolbarExtra : Maybe (Html msg)
    }


type alias UpdatedTimeConfig a =
    { getTime : a -> Time.Posix
    , currentTime : Time.Posix
    }


view : Theme -> ViewConfig a msg -> Model a -> Html msg
view theme config model =
    div []
        [ viewToolbar theme config model
        , ActionResultWrapper.page theme (viewList theme config model) model.pagination
        ]


viewToolbar : Theme -> ViewConfig a msg -> Model a -> Html msg
viewToolbar theme cfg model =
    let
        style =
            [ Spacing.stackMD
            ]
    in
    div [ css style ]
        [ viewToolbarFilter theme cfg model
        , viewToolbarSort theme cfg model
        ]


viewToolbarFilter : Theme -> ViewConfig a msg -> Model a -> Html msg
viewToolbarFilter theme cfg model =
    input
        [ css [ FormInput.inputStyle theme ]
        , type_ "text"
        , placeholder "Filter by name..."
        , onInput (cfg.wrapMsg << QueryInput)
        , value model.qInput
        ]
        []


viewToolbarSort : Theme -> ViewConfig a msg -> Model a -> Html msg
viewToolbarSort theme cfg model =
    let
        paginationQueryString =
            model.paginationQueryString

        sortOption ( name, visibleName ) =
            let
                route =
                    cfg.toRoute { paginationQueryString | sortBy = Just name, page = Just 1 }
            in
            li [] [ a [ href route ] [ text visibleName ] ]

        sortDirectionButton =
            if paginationQueryString.sortDirection == SortASC then
                a [ href (cfg.toRoute { paginationQueryString | sortDirection = SortDESC, page = Just 1 }) ]
                    [ text "ASC" ]

            else
                a [ href (cfg.toRoute { paginationQueryString | sortDirection = SortASC, page = Just 1 }) ]
                    [ text "DESC" ]
    in
    div []
        [ ul [] (List.map sortOption cfg.sortOptions)
        , sortDirectionButton
        ]


viewList : Theme -> ViewConfig a msg -> Model a -> Pagination a -> Html msg
viewList theme cfg model pagination =
    if List.length pagination.items > 0 then
        let
            paginationConfig =
                { paginationQueryString = model.paginationQueryString
                , page = pagination.page
                , toRoute = cfg.toRoute
                }
        in
        div []
            [ div [ css [ Spacing.stackLG ] ]
                (List.indexedMap (viewItem theme cfg) model.items)
            , Pagination.view theme paginationConfig
            ]

    else
        viewEmpty


viewItem : Theme -> ViewConfig a msg -> Int -> Item a -> Html msg
viewItem theme config index item =
    let
        --actions =
        --    config.dropdownItems item.item
        --
        --dropdown =
        --    if List.length actions > 0 then
        --        Dropdown.dropdown item.dropdownState
        --            { options = [ Dropdown.alignMenuRight ]
        --            , toggleMsg = config.wrapMsg << ItemDropdownMsg index
        --            , toggleButton =
        --                Dropdown.toggle [ Button.roleLink ]
        --                    [ faSet "listing.actions" appState ]
        --            , items = List.map (viewAction appState) actions
        --            }
        --
        --    else
        --        emptyNode
        --
        --icon =
        --    config.iconView
        --        |> Maybe.andMap (Just item.item)
        --        |> Maybe.withDefault (ItemIcon.view { text = config.textTitle item.item, image = Nothing })
        styles =
            [ displayFlex
            , alignItems center
            , Spacing.insetStretchSM
            , borderBottom3 (px 1) solid theme.colors.border
            , firstChild
                [ borderTop3 (px 1) solid theme.colors.border
                ]
            , descendants
                [ typeSelector "img"
                    [ Spacing.inlineSM
                    , width (px2rem 45)
                    , height (px2rem 45)
                    ]
                , Css.class "link"
                    [ Typography.heading3 theme
                    , textDecoration none
                    , hover [ textDecoration underline ]
                    ]
                , Css.class "content"
                    [ Spacing.inlineSM
                    , flexGrow (num 1)
                    ]
                , Css.class "updated"
                    [ Typography.copy1lighter theme ]
                , Css.class "fragment"
                    [ Spacing.inlineSM ]
                ]
            ]
    in
    div [ css styles ]
        [ img [ src "/img/project-icons/1.png" ] []
        , div [ class "content" ]
            [ div [ class "title-row" ]
                [ span [ class "title" ] [ config.title item.item ]
                ]
            , div [ class "extra" ]
                [ div [ class "description" ]
                    [ config.description item.item ]
                ]
            ]
        , div [ class "updated" ]
            [ viewUpdated config item.item ]

        --, div [ class "actions" ]
        --    [ dropdown ]
        ]


viewUpdated : ViewConfig a msg -> a -> Html msg
viewUpdated config item =
    case config.updated of
        Just updated ->
            span []
                [ text <| "Updated " ++ inWords (updated.getTime item) updated.currentTime ]

        Nothing ->
            emptyNode


viewEmpty : Html msg
viewEmpty =
    div [] [ text "list is empty" ]
