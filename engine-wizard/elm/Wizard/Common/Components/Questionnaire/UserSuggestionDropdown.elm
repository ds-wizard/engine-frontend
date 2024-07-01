module Wizard.Common.Components.Questionnaire.UserSuggestionDropdown exposing
    ( Model
    , Msg
    , ViewConfig
    , init
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Debouncer.Extra as Debouncer exposing (Debouncer)
import Gettext exposing (gettext)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.User as User
import Shared.Data.UserSuggestion exposing (UserSuggestion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (faSet)
import Shared.Setters exposing (setDebouncer)
import Shared.Utils exposing (dispatch)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy, tooltipLeft)
import Wizard.Common.Html.Events exposing (alwaysStopPropagationOn)
import Wizard.Common.View.UserIcon as UserIcon


type alias Model =
    { uuid : Uuid
    , editorNote : Bool
    , dropdownState : Dropdown.State
    , searchValue : String
    , users : ActionResult (Pagination UserSuggestion)
    , debouncer : Debouncer Msg
    }


init : Uuid -> Bool -> Model
init uuid editorNote =
    { uuid = uuid
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
                        , dispatch (Search "")
                        )

                    else
                        ( model.users, Cmd.none )
            in
            ( { model | dropdownState = dropdownState, users = users }
            , cmd
            )

        SearchInput value ->
            ( { model | searchValue = value }
            , dispatch (DebouncerMsg (Debouncer.provideInput (Search value)))
            )

        Search value ->
            ( { model | users = ActionResult.Loading }
            , QuestionnairesApi.getQuestionnaireUserSuggestions model.uuid model.editorNote value appState SearchCompleted
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
            ActionResult.unwrap [] (List.sortWith User.compare << .items) model.users
    in
    Dropdown.dropdown model.dropdownState
        { options = [ Dropdown.alignMenuRight, Dropdown.attrs [ class "UserSuggestionDropdown" ] ]
        , toggleMsg = cfg.wrapMsg << DropdownMsg
        , toggleButton =
            Dropdown.toggle
                [ Button.roleLink
                ]
                [ span (tooltipLeft (gettext "Assign comment thread" appState.locale))
                    [ faSet "questionnaire.commentsAssign" appState ]
                ]
        , items =
            [ Dropdown.customItem <|
                div [ class "dropdown-item-search" ]
                    [ input
                        [ type_ "text"
                        , class "form-control"
                        , placeholder (gettext "Search users..." appState.locale)
                        , alwaysStopPropagationOn "click" (D.succeed (cfg.wrapMsg <| SearchInput model.searchValue))
                        , onInput (cfg.wrapMsg << SearchInput)
                        , value model.searchValue
                        ]
                        []
                    ]
            , Dropdown.divider
            ]
                ++ List.map viewUserItem foundUsers
        }
