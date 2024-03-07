module Registry2.Components.VersionList exposing (Msg, State, ViewProps, initialState, update, view)

import Html exposing (Html, a, div, i, li, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Shared.Utils exposing (flip)
import Version exposing (Version)


type State
    = State StateData


type alias StateData =
    { collapsed : Bool }


initialState : State
initialState =
    State
        { collapsed = True }


type Msg
    = ToggleCollapse


update : Msg -> State -> State
update msg (State state) =
    case msg of
        ToggleCollapse ->
            State { state | collapsed = not state.collapsed }


type alias ViewProps =
    { versions : List Version
    , currentVersion : Version
    , toUrl : Version -> String
    }


view : State -> ViewProps -> Html Msg
view (State state) props =
    let
        versions =
            List.filter ((/=) props.currentVersion) props.versions

        isCollapsed =
            List.length versions > 4 && state.collapsed

        takeCollapsed list =
            if isCollapsed then
                List.take 3 list

            else
                list

        viewAllLink =
            if isCollapsed then
                [ a [ onClick ToggleCollapse, class "btn btn-light btn-sm mt-2" ]
                    [ i [ class "fas fa-angle-down me-1" ] []
                    , text "View all"
                    ]
                ]

            else
                []
    in
    versions
        |> List.sortWith Version.compare
        |> List.reverse
        |> takeCollapsed
        |> List.map (viewVersion props.toUrl)
        |> ul [ class "mb-0 ps-4" ]
        |> List.singleton
        |> flip (++) viewAllLink
        |> div []


viewVersion : (Version -> String) -> Version -> Html Msg
viewVersion toUrl version =
    li []
        [ a [ href (toUrl version) ]
            [ text (Version.toString version) ]
        ]
