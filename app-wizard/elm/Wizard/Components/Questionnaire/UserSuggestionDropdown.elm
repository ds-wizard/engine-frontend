module Wizard.Components.Questionnaire.UserSuggestionDropdown exposing
    ( Model
    , Msg
    , ViewConfig
    , init
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Components.FontAwesome exposing (faQuestionnaireCommentsAssign)
import Common.Components.Tooltip exposing (tooltipLeft)
import Common.Ports.Dom as Dom
import Common.Utils.Setters exposing (setDebouncer)
import Debouncer.Extra as Debouncer exposing (Debouncer)
import Gettext exposing (gettext)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (class, id, placeholder, type_, value)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onClickStopPropagation)
import Task.Extra as Task
import Uuid exposing (Uuid)
import Wizard.Api.Models.User as User
import Wizard.Api.Questionnaires as QuestionnairesApi
import Wizard.Components.UserIcon as UserIcon
import Wizard.Data.AppState exposing (AppState)


type alias Model =
    { uuid : Uuid
    , threadUuid : Uuid
    , editorNote : Bool
    , dropdownState : Dropdown.State
    , searchValue : String
    , users : ActionResult (Pagination UserSuggestion)
    , debouncer : Debouncer Msg
    }


init : Uuid -> Uuid -> Bool -> Model
init uuid threadUuid editorNote =
    { uuid = uuid
    , threadUuid = threadUuid
    , editorNote = editorNote
    , dropdownState = Dropdown.initialState
    , searchValue = ""
    , users = ActionResult.Unset
    , debouncer = Debouncer.toDebouncer (Debouncer.debounce 500)
    }


type Msg
    = DropdownMsg Dropdown.State
    | SearchInput String
    | Search String
    | SearchCompleted (Result ApiError (Pagination UserSuggestion))
    | DebouncerMsg (Debouncer.Msg Msg)


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        DropdownMsg dropdownState ->
            let
                ( users, cmd ) =
                    if model.users == ActionResult.Unset then
                        ( ActionResult.Loading
                        , Cmd.batch
                            [ Task.dispatch (Search "")
                            , Dom.focus ("#user-search-" ++ Uuid.toString model.threadUuid)
                            ]
                        )

                    else
                        ( model.users, Cmd.none )
            in
            ( { model | dropdownState = dropdownState, users = users }
            , cmd
            )

        SearchInput value ->
            ( { model | searchValue = value }
            , Task.dispatch (DebouncerMsg (Debouncer.provideInput (Search value)))
            )

        Search value ->
            ( { model | users = ActionResult.Loading }
            , QuestionnairesApi.getQuestionnaireUserSuggestions appState model.uuid model.editorNote value SearchCompleted
            )

        SearchCompleted result ->
            case result of
                Ok users ->
                    ( { model | users = ActionResult.Success users }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | users = ApiError.toActionResult appState (gettext "Unable to get users." appState.locale) error }
                    , Cmd.none
                    )

        DebouncerMsg debouncerMsg ->
            let
                updateConfig =
                    { mapMsg = DebouncerMsg
                    , getDebouncer = .debouncer
                    , setDebouncer = setDebouncer
                    }
            in
            Debouncer.update (update appState) updateConfig debouncerMsg model


subscriptions : Model -> Sub Msg
subscriptions model =
    Dropdown.subscriptions model.dropdownState DropdownMsg


type alias ViewConfig msg =
    { wrapMsg : Msg -> msg
    , selectMsg : UserSuggestion -> msg
    }


view : ViewConfig msg -> AppState -> Model -> Html msg
view cfg appState model =
    let
        viewUserItem user =
            Dropdown.buttonItem
                [ onClick (cfg.selectMsg user)
                , dataCy "project_comment-assign_user-suggestion"
                ]
                [ UserIcon.viewSmall user
                , text (User.fullName user)
                ]

        foundUsers =
            case model.users of
                Success users ->
                    if List.isEmpty users.items then
                        [ Dropdown.customItem <|
                            div [ class "dropdown-item-empty" ]
                                [ text (gettext "No users found." appState.locale)
                                ]
                        ]

                    else
                        users.items
                            |> List.sortWith User.compare
                            |> List.map viewUserItem

                _ ->
                    []
    in
    Dropdown.dropdown model.dropdownState
        { options = [ Dropdown.alignMenuRight, Dropdown.attrs [ class "UserSuggestionDropdown" ] ]
        , toggleMsg = cfg.wrapMsg << DropdownMsg
        , toggleButton =
            Dropdown.toggle
                [ Button.roleLink
                , Button.attrs [ dataCy "comments_comment_assign" ]
                ]
                [ span (tooltipLeft (gettext "Assign comment thread" appState.locale))
                    [ faQuestionnaireCommentsAssign ]
                ]
        , items =
            [ Dropdown.customItem <|
                div [ class "dropdown-item-search" ]
                    [ input
                        [ type_ "text"
                        , class "form-control"
                        , placeholder (gettext "Search users..." appState.locale)
                        , onClickStopPropagation (cfg.wrapMsg <| SearchInput model.searchValue)
                        , onInput (cfg.wrapMsg << SearchInput)
                        , value model.searchValue
                        , id ("user-search-" ++ Uuid.toString model.threadUuid)
                        ]
                        []
                    ]
            , Dropdown.divider
            ]
                ++ foundUsers
        }
