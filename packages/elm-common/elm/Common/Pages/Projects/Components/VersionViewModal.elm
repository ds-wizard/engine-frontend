module Common.Pages.Projects.Components.VersionViewModal exposing
    ( Model
    , Msg
    , init
    , initEmpty
    , subscriptions
    , update
    , view
    )

{-| The questionnaire as it was at one change, over the one being worked on.

Two requests make it: the project's content at that change (`fetchPreview`) and
the questionnaire itself, which carries the knowledge model the content is read
through. The two are put together into an ordinary read-only questionnaire —
the same component, `initSimple`, with its toolbar and its actions off — so
nothing about how a questionnaire is shown is written twice.

It is a cover modal rather than a page of its own: the project stays behind it,
and closing is Escape or the backdrop, both the browser's.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.ProjectCommon exposing (ProjectCommon)
import Common.Api.Models.ProjectContent exposing (ProjectContent)
import Common.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Api.Projects as ProjectsApi
import Common.Components.Questionnaire as Questionnaire
import Common.Data.AppState exposing (AppState)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.AsyncBlock as AsyncBlock
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Modal as Modal
import Ui.Tooltip as Tooltip
import Uuid exposing (Uuid)


type alias Model =
    { questionnaire : ActionResult Questionnaire.Model
    , content : ActionResult ProjectContent
    , project : ActionResult ProjectQuestionnaire
    , projectCommon : ActionResult ProjectCommon
    , title : String
    , open : Bool
    }


initEmpty : Model
initEmpty =
    { questionnaire = Unset
    , content = Unset
    , project = Unset
    , projectCommon = Unset
    , title = ""
    , open = False
    }


{-| `title` is what the change is called — when it happened, after the name it
was given if it has one. The history panel makes it, because that is where the
change and its version are.
-}
init : AppState -> Uuid -> Uuid -> String -> ( Model, Cmd Msg )
init appState projectUuid eventUuid title =
    ( { questionnaire = Loading
      , content = Loading
      , project = Loading
      , projectCommon = Loading
      , title = title
      , open = True
      }
    , Cmd.batch
        [ ProjectsApi.fetchPreview appState projectUuid eventUuid GotContent
        , ProjectsApi.getQuestionnaire appState projectUuid GotQuestionnaire
        ]
    )


type Msg
    = GotContent (Result ApiError ProjectContent)
    | GotQuestionnaire (Result ApiError (ProjectDetailWrapper ProjectQuestionnaire))
    | QuestionnaireMsg Questionnaire.Msg
    | Close


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    let
        -- Neither request is any use without the other: the content is answers
        -- by path, the questionnaire is what those paths mean.
        withQuestionnaire ( updated, cmd ) =
            case ActionResult.combine updated.content updated.project of
                Success ( content, project ) ->
                    let
                        ( questionnaire, questionnaireCmd ) =
                            Questionnaire.initSimple appState (ProjectQuestionnaire.updateContent project content)
                    in
                    ( { updated | questionnaire = Success questionnaire }
                    , Cmd.batch [ cmd, Cmd.map QuestionnaireMsg questionnaireCmd ]
                    )

                Error message ->
                    ( { updated | questionnaire = Error message }, cmd )

                _ ->
                    ( updated, cmd )
    in
    case msg of
        GotContent result ->
            withQuestionnaire
                (case result of
                    Ok content ->
                        ( { model | content = Success content }, Cmd.none )

                    Err error ->
                        ( { model | content = ApiError.toActionResult appState (gettext "Unable to get the project as it was." appState.locale) error }, Cmd.none )
                )

        GotQuestionnaire result ->
            withQuestionnaire
                (case result of
                    Ok wrapper ->
                        ( { model | project = Success wrapper.data, projectCommon = Success wrapper.common }, Cmd.none )

                    Err error ->
                        ( { model | project = ApiError.toActionResult appState (gettext "Unable to get the project as it was." appState.locale) error }, Cmd.none )
                )

        QuestionnaireMsg questionnaireMsg ->
            case ( model.projectCommon, model.questionnaire ) of
                ( Success projectCommon, Success questionnaire ) ->
                    let
                        result =
                            Questionnaire.update appState
                                { wrapMsg = QuestionnaireMsg
                                , mbKmEditorUuid = Nothing
                                , projectCommon = projectCommon
                                , previewVersionMsg = Nothing
                                , revertVersionMsg = Nothing
                                }
                                questionnaireMsg
                                questionnaire
                    in
                    ( { model | questionnaire = Success result.model }, result.cmd )

                _ ->
                    ( model, Cmd.none )

        Close ->
            ( initEmpty, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.questionnaire of
        Success questionnaire ->
            Sub.map QuestionnaireMsg (Questionnaire.subscriptions questionnaire)

        _ ->
            Sub.none


view : AppState -> Model -> Html Msg
view appState model =
    Modal.plain
        { title = model.title
        , open = model.open
        , onClose = Close
        , body =
            [ AsyncBlock.view
                { loading = ActionResult.isLoading model.questionnaire
                , error = errorOf model.questionnaire
                , loadingLabel = gettext "Loading the project as it was…" appState.locale
                , content =
                    case model.questionnaire of
                        Success questionnaire ->
                            [ Html.map QuestionnaireMsg
                                (Questionnaire.view appState
                                    { wrapMsg = identity
                                    , readonly = True
                                    , commentsReadonly = True
                                    , actionsEnabled = False
                                    , toolbarEnabled = False
                                    , canViewVersions = False
                                    , canRevert = False
                                    }
                                    questionnaire
                                )
                            ]

                        _ ->
                            []
                }
            ]
        }
        |> Modal.withCover
        -- A page's worth of questionnaire, so the room a footer would take goes
        -- to the questionnaire and the way out is in the header instead.
        |> Modal.withHeaderAction
            (Button.iconOnly (Icon.solid "fa-xmark") (gettext "Close" appState.locale) Close
                |> Button.quiet
                |> Button.withTooltipAt Tooltip.End
                |> Button.withTestId "project-version-view_close"
            )
        |> Modal.withActions []
        -- Named by its own id: the title is the change's, and while the dialog
        -- is closed there is no change and no title.
        |> Modal.withId "project-version-view"
        |> Modal.withTestId "project-version-view"
        |> Modal.toHtml


errorOf : ActionResult a -> Maybe String
errorOf result =
    case result of
        Error message ->
            Just message

        _ ->
            Nothing
