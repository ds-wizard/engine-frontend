module Common.Components.Questionnaire.Modal.Version exposing
    ( Model
    , Msg
    , Open
    , UpdateConfig
    , init
    , name
    , rename
    , update
    , view
    )

{-| Naming a change, so that it can be found again: "Submitted to the funder".

The same dialog names one and renames one — the difference is whether it starts
empty and which request it sends. What comes back is handed to the panel, which
keeps its own list of versions.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.ProjectVersion exposing (ProjectVersion)
import Common.Api.Projects as ProjectsApi
import Common.Data.AppState exposing (AppState)
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import Json.Encode as E
import Json.Encode.Extra as E
import Maybe.Extra as Maybe
import Task
import Ui.Field as Field
import Ui.Modal as Modal
import Uuid exposing (Uuid)


type alias Model =
    { projectUuid : Uuid
    , open : Maybe Open
    , name_ : String
    , description : String
    , result : ActionResult ProjectVersion
    }


{-| What is being named: a change that has no version yet, or one that has.
-}
type Open
    = New Uuid
    | Existing ProjectVersion


init : Uuid -> Model
init projectUuid =
    { projectUuid = projectUuid, open = Nothing, name_ = "", description = "", result = Unset }


{-| Name the change `eventUuid`.
-}
name : Uuid -> Model -> Model
name eventUuid model =
    { model | open = Just (New eventUuid), name_ = "", description = "", result = Unset }


rename : ProjectVersion -> Model -> Model
rename version model =
    { model
        | open = Just (Existing version)
        , name_ = version.name
        , description = Maybe.withDefault "" version.description
        , result = Unset
    }


type Msg
    = NameTyped String
    | DescriptionTyped String
    | Submit
    | Saved (Result ApiError ProjectVersion)
    | Close


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , savedMsg : ProjectVersion -> msg
    }


update : AppState -> UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update appState cfg msg model =
    case msg of
        NameTyped value ->
            ( { model | name_ = value }, Cmd.none )

        DescriptionTyped value ->
            ( { model | description = value }, Cmd.none )

        Submit ->
            case model.open of
                Just open ->
                    if String.isEmpty (String.trim model.name_) then
                        ( model, Cmd.none )

                    else
                        let
                            body =
                                E.object
                                    [ ( "eventUuid", Uuid.encode (eventUuidOf open) )
                                    , ( "name", E.string (String.trim model.name_) )
                                    , ( "description", E.maybe E.string (nonEmpty model.description) )
                                    ]

                            request =
                                case open of
                                    New _ ->
                                        ProjectsApi.postVersion appState model.projectUuid body (cfg.wrapMsg << Saved)

                                    Existing version ->
                                        ProjectsApi.putVersion appState model.projectUuid version.uuid body (cfg.wrapMsg << Saved)
                        in
                        ( { model | result = Loading }, request )

                Nothing ->
                    ( model, Cmd.none )

        Saved result ->
            case result of
                Ok version ->
                    ( { model | open = Nothing, result = Success version }
                    , Task.perform (always (cfg.savedMsg version)) (Task.succeed ())
                    )

                Err error ->
                    ( { model | result = ApiError.toActionResult appState (gettext "Unable to save the version." appState.locale) error }, RequestHelpers.getResultCmd (cfg.wrapMsg Close) result )

        Close ->
            ( { model | open = Nothing }, Cmd.none )


eventUuidOf : Open -> Uuid
eventUuidOf open =
    case open of
        New eventUuid ->
            eventUuid

        Existing version ->
            version.eventUuid


nonEmpty : String -> Maybe String
nonEmpty value =
    if String.isEmpty (String.trim value) then
        Nothing

    else
        Just (String.trim value)


view : AppState -> Model -> Html Msg
view appState model =
    let
        title =
            case model.open of
                Just (Existing _) ->
                    gettext "Rename the version" appState.locale

                _ ->
                    gettext "Name this version" appState.locale
    in
    Modal.plain
        { title = title
        , open = Maybe.isJust model.open
        , onClose = Close
        , body =
            [ Field.toHtml
                (Field.input
                    { id = "project-version-name"
                    , label = gettext "Name" appState.locale
                    , value = model.name_
                    , onInput = NameTyped
                    }
                    |> Field.withRequired True
                )
            , Field.toHtml
                (Field.textarea
                    { id = "project-version-description"
                    , label = gettext "Description" appState.locale
                    , value = model.description
                    , onInput = DescriptionTyped
                    }
                )
            ]
        }
        |> Modal.withForm { onSubmit = Submit, submitLabel = gettext "Save" appState.locale }
        |> Modal.withLoading (ActionResult.isLoading model.result)
        |> Modal.withTestId "project-version"
        |> Modal.toHtml
