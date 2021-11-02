module Wizard.Common.Components.Listing exposing
    ( ListingActionConfig
    , ListingActionType(..)
    , ListingConfig
    , ListingDropdownItem
    , Model
    , Msg
    , UpdatedConfig
    , dropdownAction
    , dropdownSeparator
    , modelFromList
    , subscriptions
    , update
    , view
    , viewItem
    )

import Bootstrap.Dropdown as Dropdown
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l)
import Shared.Undraw as Undraw
import Time
import Time.Distance exposing (inWordsWithConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.ListingDropdown as ListingDropdown
import Wizard.Common.TimeDistance exposing (locale)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes
import Wizard.Routing as Routing


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Components.Listing"



-- MODEL


type alias Model a =
    { items : List (Item a) }


type alias Item a =
    { dropdownState : Dropdown.State
    , item : a
    }



-- CONFIG


type alias ListingConfig a msg =
    { title : a -> Html msg
    , description : a -> Html msg
    , dropdownItems : a -> List (ListingDropdownItem msg)
    , textTitle : a -> String
    , emptyText : String
    , updated : Maybe (UpdatedConfig a)
    , wrapMsg : Msg -> msg
    , iconView : Maybe (a -> Html msg)
    }


type alias UpdatedConfig a =
    { getTime : a -> Time.Posix
    , currentTime : Time.Posix
    }


type ListingDropdownItem msg
    = ListingDropdownAction (ListingActionConfig msg)
    | ListingDropdownSeparator


type alias ListingActionConfig msg =
    { extraClass : Maybe String
    , icon : Html msg
    , label : String
    , msg : ListingActionType msg
    }


type ListingActionType msg
    = ListingActionMsg msg
    | ListingActionLink Routes.Route
    | ListingActionExternalLink String



-- MSG


type Msg
    = DropdownMsg Int Dropdown.State



-- MODEL


modelFromList : List a -> Model a
modelFromList list =
    let
        wrap item =
            { dropdownState = Dropdown.initialState
            , item = item
            }
    in
    { items = List.map wrap list }



-- UTILS


dropdownAction : ListingActionConfig msg -> ListingDropdownItem msg
dropdownAction =
    ListingDropdownAction


dropdownSeparator : ListingDropdownItem msg
dropdownSeparator =
    ListingDropdownSeparator



-- UPDATE


update : Msg -> Model a -> Model a
update msg model =
    case msg of
        DropdownMsg index state ->
            let
                updateItem i item =
                    if i == index then
                        { item | dropdownState = state }

                    else
                        item

                newItems =
                    List.indexedMap updateItem model.items
            in
            { model | items = newItems }



-- SUBSCRIPTIONS


subscriptions : Model a -> Sub Msg
subscriptions model =
    let
        subscription index item =
            Dropdown.subscriptions item.dropdownState (DropdownMsg index)
    in
    Sub.batch <| List.indexedMap subscription model.items



-- VIEW


view : AppState -> ListingConfig a msg -> Model a -> Html msg
view appState config model =
    if List.length model.items > 0 then
        div [ class "Listing list-group list-group-flush" ]
            (List.indexedMap (viewItem appState config) model.items)

    else
        viewEmpty appState config


viewEmpty : AppState -> ListingConfig a msg -> Html msg
viewEmpty appState config =
    Page.illustratedMessage
        { image = Undraw.noData
        , heading = l_ "empty.heading" appState
        , lines = [ config.emptyText ]
        , cy = "listing-empty"
        }


viewItem : AppState -> ListingConfig a msg -> Int -> Item a -> Html msg
viewItem appState config index item =
    let
        actions =
            config.dropdownItems item.item

        dropdown =
            if List.length actions > 0 then
                ListingDropdown.dropdown appState
                    { dropdownState = item.dropdownState
                    , toggleMsg = config.wrapMsg << DropdownMsg index
                    , items = List.map (viewAction appState) actions
                    }

            else
                emptyNode

        icon =
            config.iconView
                |> Maybe.andMap (Just item.item)
                |> Maybe.withDefault (ItemIcon.view { text = config.textTitle item.item, image = Nothing })
    in
    div [ class "list-group-item" ]
        [ icon
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
            [ viewUpdated appState config item.item ]
        , div [ class "actions" ]
            [ dropdown ]
        ]


viewUpdated : AppState -> ListingConfig a msg -> a -> Html msg
viewUpdated appState config item =
    case config.updated of
        Just updated ->
            span []
                [ text <| l_ "item.updated" appState ++ inWordsWithConfig { withAffix = True } (locale appState) (updated.getTime item) updated.currentTime ]

        Nothing ->
            emptyNode


viewAction : AppState -> ListingDropdownItem msg -> Dropdown.DropdownItem msg
viewAction appState dropdownItem =
    case dropdownItem of
        ListingDropdownAction action ->
            let
                attrs =
                    case action.msg of
                        ListingActionLink route ->
                            [ href <| Routing.toUrl appState route ]

                        ListingActionExternalLink url ->
                            [ href url, target "_blank" ]

                        ListingActionMsg msg ->
                            [ onClick msg ]
            in
            Dropdown.anchorItem
                (class (Maybe.withDefault "" action.extraClass) :: attrs)
                [ action.icon, text action.label ]

        ListingDropdownSeparator ->
            Dropdown.divider
