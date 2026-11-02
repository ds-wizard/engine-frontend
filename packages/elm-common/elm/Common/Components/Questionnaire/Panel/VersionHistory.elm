module Common.Components.Questionnaire.Panel.VersionHistory exposing
    ( Model
    , Msg
    , Outcome(..)
    , UpdateConfig
    , ViewConfig
    , addEvent
    , init
    , initMsg
    , removeVersion
    , setVersion
    , subscriptions
    , update
    , view
    )

{-| What has happened to the project: every change, newest first, grouped by month
and by day, with the versions people have named among them.

**"Load older" works.** The wizard drew the link and did nothing when it was
pressed; here it asks for the next page and says so while
it comes. The endpoint has always been paged.

**A day folds.** Today is open; the rest are disclosure buttons that say what they
fold and how many changes are in them, so a year of work is a list of days rather
than a scroll (§11.7).

**What a row offers** depends on who is looking: naming, renaming and deleting a
version are the owner's, viewing the questionnaire at an event and making a
document from it are the page's to allow, and so is reverting.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.KnowledgeModel.Question as Question
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue exposing (ReplyValue(..))
import Common.Api.Models.ProjectDetail.Reply.ReplyValue.IntegrationReplyType as IntegrationReplyType
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Api.Models.ProjectVersion as ProjectVersion exposing (ProjectVersion)
import Common.Api.Models.User as User
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Api.Projects as ProjectsApi
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.ProjectUtils as ProjectUtils
import Common.Routes as Routes
import Common.Utils.Markdown as Markdown
import Common.Utils.TimeUtils as TimeUtils
import Dict exposing (Dict)
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, button, div, h3, li, span, strong, text, ul)
import Html.Attributes exposing (attribute, class, type_)
import Html.Events exposing (onClick)
import List.Extra as List
import Maybe.Extra as Maybe
import String.Format as String
import Time
import Ui.AsyncBlock as AsyncBlock
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.EmptyState as EmptyState
import Ui.Field as Field
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Menu as Menu
import Ui.Text as Text
import Uuid exposing (Uuid)


type alias Model =
    { projectUuid : Uuid
    , events : ActionResult (List ProjectEvent)
    , versions : ActionResult (List ProjectVersion)
    , page : Int
    , more : Bool
    , loadingMore : Bool
    , expandedDays : List String
    , menus : Dict String Menu.Model
    }


init : Uuid -> Model
init projectUuid =
    { projectUuid = projectUuid
    , events = Unset
    , versions = Unset
    , page = 0
    , more = False
    , loadingMore = False
    , expandedDays = []
    , menus = Dict.empty
    }


{-| Sent when the panel opens: the history is fetched then, not before.
-}
initMsg : Msg
initMsg =
    Load


{-| A version just named or renamed, in the list the panel keeps.
-}
setVersion : ProjectVersion -> Model -> Model
setVersion version model =
    { model
        | versions =
            ActionResult.map
                (\versions ->
                    if List.any (\existing -> existing.uuid == version.uuid) versions then
                        List.map
                            (\existing ->
                                if existing.uuid == version.uuid then
                                    version

                                else
                                    existing
                            )
                            versions

                    else
                        version :: versions
                )
                model.versions
    }


{-| A version whose name has been taken off.
-}
removeVersion : ProjectVersion -> Model -> Model
removeVersion version model =
    { model | versions = ActionResult.map (List.filter (\existing -> existing.uuid /= version.uuid)) model.versions }


{-| A change made while the panel is open belongs in the list under today.
-}
addEvent : ProjectEvent -> Model -> Model
addEvent event model =
    { model | events = ActionResult.map (\events -> events ++ [ event ]) model.events }



-- UPDATE


type Msg
    = Load
    | GotEvents Int (Result ApiError (Pagination ProjectEvent))
    | GotVersions (Result ApiError (List ProjectVersion))
    | LoadMore
    | SetDayExpanded String Bool
    | MenuMsg String Menu.Msg
    | Chose Outcome


{-| What the panel asks for and cannot do itself.
-}
type Outcome
    = ScrollTo String
    | NameVersion Uuid
    | RenameVersion ProjectVersion
    | DeleteVersion ProjectVersion
    | ViewVersion Uuid String
    | Revert ProjectEvent
    | NamedOnlyChanged Bool


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg }


update : AppState -> UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg, Maybe Outcome )
update appState cfg msg model =
    case msg of
        Load ->
            ( { model | events = Loading, versions = Loading, page = 0, more = False }
            , Cmd.batch
                [ ProjectsApi.getEvents appState model.projectUuid 0 (cfg.wrapMsg << GotEvents 0)
                , ProjectsApi.getVersions appState model.projectUuid (cfg.wrapMsg << GotVersions)
                ]
            , Nothing
            )

        GotEvents page result ->
            case result of
                Ok pagination ->
                    -- The server answers newest first; the list is kept oldest
                    -- first, so an older page goes before what is already here.
                    ( { model
                        | events = Success (List.reverse pagination.items ++ ActionResult.withDefault [] model.events)
                        , page = page + 1
                        , more = page + 1 < pagination.page.totalPages
                        , loadingMore = False
                        , expandedDays =
                            if page == 0 then
                                [ dayKey appState appState.currentTime ]

                            else
                                model.expandedDays
                      }
                    , Cmd.none
                    , Nothing
                    )

                Err _ ->
                    ( { model | events = Error (gettext "Unable to get the version history." appState.locale), loadingMore = False }, Cmd.none, Nothing )

        GotVersions result ->
            case result of
                Ok versions ->
                    ( { model | versions = Success versions }, Cmd.none, Nothing )

                Err _ ->
                    ( { model | versions = Error (gettext "Unable to get the versions." appState.locale) }, Cmd.none, Nothing )

        LoadMore ->
            ( { model | loadingMore = True }
            , ProjectsApi.getEvents appState model.projectUuid model.page (cfg.wrapMsg << GotEvents model.page)
            , Nothing
            )

        SetDayExpanded key expanded ->
            ( { model
                | expandedDays =
                    if expanded then
                        key :: model.expandedDays

                    else
                        List.filter ((/=) key) model.expandedDays
              }
            , Cmd.none
            , Nothing
            )

        MenuMsg key menuMsg ->
            let
                ( menu, menuCmd ) =
                    Menu.update key 6 menuMsg (Maybe.withDefault Menu.init (Dict.get key model.menus))
            in
            ( { model | menus = Dict.insert key menu model.menus }, Cmd.map (cfg.wrapMsg << MenuMsg key) menuCmd, Nothing )

        Chose outcome ->
            ( model, Cmd.none, Just outcome )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch (List.map (\( key, menu ) -> Sub.map (MenuMsg key) (Menu.subscriptions key menu)) (Dict.toList model.menus))


{-| The day an event belongs to, in the reader's own timezone.
-}
dayKey : AppState -> Time.Posix -> String
dayKey appState moment =
    String.join "-"
        [ String.fromInt (Time.toYear appState.timeZone moment)
        , String.fromInt (monthNumber (Time.toMonth appState.timeZone moment))
        , String.fromInt (Time.toDay appState.timeZone moment)
        ]


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12



-- VIEW


type alias ViewConfig =
    { questionnaire : ProjectQuestionnaire
    , namedOnly : Bool
    , canView : Bool
    , canRevert : Bool
    }


view : AppState -> ViewConfig -> Model -> Html Msg
view appState cfg model =
    let
        events =
            ActionResult.withDefault [] model.events
                |> List.filterNot ProjectEvent.isInvisible
                |> List.filter
                    (\event ->
                        not cfg.namedOnly
                            || Maybe.isJust (ProjectVersion.getVersionByEventUuid (ActionResult.withDefault [] model.versions) (ProjectEvent.getUuid event))
                    )
                |> List.reverse

        days =
            groupByDay appState events
    in
    div [ class "c-questionnaire-history" ]
        [ div [ class "c-questionnaire-history__filter" ]
            [ Field.toHtml
                (Field.toggle
                    { id = "questionnaire-history-named-only"
                    , label = gettext "Named versions only" appState.locale
                    , checked = cfg.namedOnly
                    , onCheck = namedOnlyMsg
                    }
                )
            ]
        , AsyncBlock.view
            { loading = ActionResult.isLoading model.events || ActionResult.isLoading model.versions
            , error = errorOf model.events
            , loadingLabel = gettext "Loading the version history…" appState.locale
            , content =
                viewEmpty appState cfg model days
                    ++ List.map (viewDay appState cfg model) days
                    ++ viewLoadMore appState model
            }
        ]


{-| Named versions only, and none among the changes shown. The versions are all
loaded at once and the changes a page at a time, so a project whose named
versions are all among the older changes is told to load them rather than that
it has none.
-}
viewEmpty : AppState -> ViewConfig -> Model -> List ( String, List ProjectEvent ) -> List (Html Msg)
viewEmpty appState cfg model days =
    if cfg.namedOnly && List.isEmpty days then
        [ EmptyState.toHtml
            (EmptyState.emptyState
                (if model.more && not (List.isEmpty (ActionResult.withDefault [] model.versions)) then
                    { title = gettext "No named versions among these changes" appState.locale
                    , description = gettext "Load older changes to find them." appState.locale
                    }

                 else
                    { title = gettext "No named versions" appState.locale
                    , description = gettext "Turn off “Named versions only” to see every change." appState.locale
                    }
                )
                |> EmptyState.withIllustration (Icon.toHtml (Icon.solid "fa-tag"))
                |> EmptyState.withHeadingLevel 3
            )
        ]

    else
        []


{-| Whether only named versions are shown belongs to the questionnaire, which
remembers it between visits; the panel passes the change on.
-}
namedOnlyMsg : Bool -> Msg
namedOnlyMsg value =
    Chose (NamedOnlyChanged value)


errorOf : ActionResult a -> Maybe String
errorOf result =
    case result of
        Error message ->
            Just message

        _ ->
            Nothing


viewLoadMore : AppState -> Model -> List (Html Msg)
viewLoadMore appState model =
    if model.more then
        [ div [ class "c-questionnaire-history__more" ]
            [ Button.toHtml
                (Button.ghost (gettext "Load older changes" appState.locale) LoadMore
                    |> Button.small
                    |> Button.withLoading model.loadingMore
                )
            ]
        ]

    else
        []


{-| The changes, newest first, cut into the days they happened on — in the same
order, so today is the first fold and its newest change the first in it.

`foldr` reaches the last change first and builds the list backwards, which is
what keeps the order the caller gave.

-}
groupByDay : AppState -> List ProjectEvent -> List ( String, List ProjectEvent )
groupByDay appState events =
    List.foldr
        (\event acc ->
            let
                key =
                    dayKey appState (ProjectEvent.getCreatedAt event)
            in
            case acc of
                ( previousKey, group ) :: rest ->
                    if previousKey == key then
                        ( previousKey, event :: group ) :: rest

                    else
                        ( key, [ event ] ) :: acc

                [] ->
                    [ ( key, [ event ] ) ]
        )
        []
        events


viewDay : AppState -> ViewConfig -> Model -> ( String, List ProjectEvent ) -> Html Msg
viewDay appState cfg model ( key, events ) =
    let
        expanded =
            List.member key model.expandedDays

        label =
            case List.head events of
                Just event ->
                    TimeUtils.toReadableDate appState.timeZone (ProjectEvent.getCreatedAt event)

                Nothing ->
                    key

        summary =
            String.format (ngettext ( "%s change", "%s changes" ) (List.length events) appState.locale)
                [ String.fromInt (List.length events) ]
    in
    div [ class "c-questionnaire-history__day" ]
        (button
            [ type_ "button"
            , class "c-questionnaire-history__toggle"
            , attribute "aria-expanded"
                (if expanded then
                    "true"

                 else
                    "false"
                )
            , onClick (SetDayExpanded key (not expanded))
            ]
            [ Icon.toHtml
                (if expanded then
                    Icon.solid "fa-chevron-down"

                 else
                    Icon.solid "fa-chevron-right"
                )
            , h3 [ class "c-questionnaire-history__date" ] [ text label ]
            , Text.bodySmall summary
            ]
            :: (if expanded then
                    [ ul [ class "c-questionnaire-history__events" ] (List.map (viewEvent appState cfg model) events) ]

                else
                    []
               )
        )


viewEvent : AppState -> ViewConfig -> Model -> ProjectEvent -> Html Msg
viewEvent appState cfg model event =
    let
        eventUuid =
            ProjectEvent.getUuid event

        version =
            ProjectVersion.getVersionByEventUuid (ActionResult.withDefault [] model.versions) eventUuid

        isCurrent =
            -- The newest visible change is where the questionnaire stands now.
            ProjectQuestionnaire.isCurrentVersion (ActionResult.withDefault [] model.events) eventUuid

        who =
            Maybe.map User.fullName (ProjectEvent.getCreatedBy event)
                |> Maybe.withDefault (gettext "Anonymous user" appState.locale)

        badges =
            Maybe.values
                [ Maybe.map (\v -> Badge.toHtml (Badge.info v.name)) version
                , if isCurrent then
                    Just (Badge.toHtml (Badge.success (gettext "Current" appState.locale)))

                  else
                    Nothing
                ]

        what =
            viewWhat appState cfg event

        owner =
            ProjectUtils.isOwner appState cfg.questionnaire

        -- Three things can be done with a change, and they are not the same
        -- kind of thing: name it, go and look at it, or go back to it. The
        -- wizard's menu was grouped that way and so is this one.
        naming =
            case ( owner, version ) of
                ( True, Just aVersion ) ->
                    [ Menu.item (gettext "Rename this version" appState.locale) (Chose (RenameVersion aVersion))
                        |> Menu.withIcon (Icon.solid "fa-pen")
                    , Menu.dangerItem (gettext "Delete this version" appState.locale) (Chose (DeleteVersion aVersion))
                        |> Menu.withIcon (Icon.solid "fa-trash")
                    ]

                ( True, Nothing ) ->
                    [ Menu.item (gettext "Name this version" appState.locale) (Chose (NameVersion eventUuid))
                        |> Menu.withIcon (Icon.solid "fa-tag")
                    ]

                _ ->
                    []

        looking =
            Maybe.values
                [ if cfg.canView && not isCurrent then
                    Just
                        (Menu.item (gettext "View questionnaire" appState.locale) (Chose (ViewVersion eventUuid (viewTitle appState event version)))
                            |> Menu.withIcon (Icon.regular "fa-rectangle-list")
                        )

                  else
                    Nothing
                , if cfg.canView then
                    Just
                        (Menu.linkItem (gettext "Create document" appState.locale) (Routes.toUrl appState (Routes.projectsDetailDocumentsNew cfg.questionnaire.uuid (Just eventUuid)))
                            |> Menu.withIcon (Icon.regular "fa-file")
                        )

                  else
                    Nothing
                ]

        reverting =
            if cfg.canRevert && owner && not isCurrent then
                [ Menu.dangerItem (gettext "Revert to this version" appState.locale) (Chose (Revert event))
                    |> Menu.withIcon (Icon.solid "fa-clock-rotate-left")
                ]

            else
                []

        items =
            List.filterNot List.isEmpty [ naming, looking, reverting ]
                |> List.intersperse [ Menu.separator ]
                |> List.concat

        menu =
            if List.isEmpty items then
                []

            else
                let
                    menuId =
                        "history-" ++ Uuid.toString eventUuid
                in
                [ Menu.toHtml
                    { id = menuId
                    , trigger = Menu.kebab (gettext "What to do with this change" appState.locale)
                    , alignment = Menu.End
                    , items = items
                    , toMsg = MenuMsg menuId
                    }
                    (Maybe.withDefault Menu.init (Dict.get menuId model.menus))
                ]
    in
    li [ class "c-questionnaire-history__event" ]
        [ div [ class "c-questionnaire-history__line" ]
            (Timestamp.view appState (ProjectEvent.getCreatedAt event) :: badges ++ menu)
        , div [ class "c-questionnaire-history__what" ] what
        , div [ class "c-questionnaire-history__who" ]
            [ ItemIcon.toHtml (ItemIcon.withSize ItemIcon.Small (avatarOf (ProjectEvent.getCreatedBy event)))
            , Text.bodySmall who
            ]
        ]


{-| Somebody's face, or the one an anonymous change gets.
-}
avatarOf : Maybe UserSuggestion -> ItemIcon.ItemIcon
avatarOf mbUser =
    case mbUser of
        Just user ->
            UserAvatar.user
                { firstName = user.firstName
                , lastName = user.lastName
                , imageUrl = user.imageUrl
                , gravatarHash = Just user.gravatarHash
                }

        Nothing ->
            UserAvatar.anonymous


{-| What a change did, in as much detail as the change carries: the question it
was to and the answer it left there, or the phase the project was moved to.

Without it, a history of "Data description"
eleven times over says only that somebody kept changing their mind.

-}
viewWhat : AppState -> ViewConfig -> ProjectEvent -> List (Html Msg)
viewWhat appState cfg event =
    let
        question path =
            button
                [ type_ "button"
                , class "c-questionnaire-history__question"
                , onClick (Chose (ScrollTo path))
                ]
                [ text (questionTitle cfg.questionnaire path) ]
    in
    case event of
        ProjectEvent.SetReply data ->
            question data.path :: viewReplyValue appState cfg (ProjectEvent.getQuestionUuid event) data.value

        ProjectEvent.ClearReply data ->
            [ question data.path
            , viewValue (Icon.solid "fa-eraser") [ text (gettext "The answer was cleared." appState.locale) ]
            ]

        ProjectEvent.SetPhase data ->
            case Maybe.andThen (\phaseUuid -> Dict.get (Uuid.toString phaseUuid) cfg.questionnaire.knowledgeModel.entities.phases) data.phaseUuid of
                Just phase ->
                    -- The phase is the whole of what this change was, so it is
                    -- the word the eye should land on.
                    [ viewValue (Icon.solid "fa-flag")
                        (String.formatHtml (gettext "Phase set to %s" appState.locale)
                            [ strong [] [ text phase.title ] ]
                        )
                    ]

                Nothing ->
                    [ viewValue (Icon.solid "fa-flag") [ text (gettext "Phase cleared" appState.locale) ] ]

        _ ->
            []


{-| A line of what a change was, under a mark saying what kind of thing it is.
-}
viewValue : Icon.Icon -> List (Html Msg) -> Html Msg
viewValue icon content =
    div [ class "c-questionnaire-history__value" ]
        [ span [ class "c-questionnaire-history__value-icon", attribute "aria-hidden" "true" ] [ Icon.toHtml icon ]
        , span [ class "c-questionnaire-history__value-text" ] content
        ]


{-| What an item picked from a list is called, which only the question that
picked it can say.
-}
itemSelectLabel : AppState -> ViewConfig -> String -> String -> String
itemSelectLabel appState cfg questionUuid itemUuid =
    ProjectQuestionnaire.getItemSelectQuestionValueLabel appState cfg.questionnaire questionUuid itemUuid


{-| The answer itself, with a mark saying what kind of answer it is — the
wizard's icons: a written one, a chosen one, each of several chosen ones, a
file, an item picked from a list.
-}
viewReplyValue : AppState -> ViewConfig -> Maybe String -> ReplyValue -> List (Html Msg)
viewReplyValue appState cfg mbQuestionUuid replyValue =
    let
        value icon content =
            viewValue icon [ text content ]

        answerLabel answerUuid =
            Dict.get answerUuid cfg.questionnaire.knowledgeModel.entities.answers
                |> Maybe.map .label
                |> Maybe.withDefault (gettext "Deleted answer" appState.locale)

        choiceLabel choiceUuid =
            Dict.get choiceUuid cfg.questionnaire.knowledgeModel.entities.choices
                |> Maybe.map .label
                |> Maybe.withDefault (gettext "Deleted choice" appState.locale)
    in
    case replyValue of
        StringReply reply ->
            [ value (Icon.regular "fa-pen-to-square") reply ]

        AnswerReply answerUuid ->
            [ value (Icon.regular "fa-circle-dot") (answerLabel answerUuid) ]

        MultiChoiceReply choiceUuids ->
            List.map (value (Icon.regular "fa-square-check") << choiceLabel) choiceUuids

        IntegrationReply integrationReply ->
            case integrationReply of
                IntegrationReplyType.PlainType reply ->
                    [ value (Icon.regular "fa-pen-to-square") reply ]

                IntegrationReplyType.IntegrationType reply _ ->
                    [ value (Icon.solid "fa-link") (Markdown.toString reply) ]

        ItemSelectReply itemUuid ->
            case mbQuestionUuid of
                Just questionUuid ->
                    [ value (Icon.regular "fa-square-caret-down") (itemSelectLabel appState cfg questionUuid itemUuid) ]

                Nothing ->
                    [ value (Icon.regular "fa-square-caret-down") (gettext "Item" appState.locale) ]

        FileReply fileUuid ->
            case ProjectQuestionnaire.getFile cfg.questionnaire fileUuid of
                Just file ->
                    [ value (Icon.solid "fa-file") file.fileName ]

                Nothing ->
                    [ value (Icon.solid "fa-file") (gettext "Deleted file" appState.locale) ]

        ItemListReply _ ->
            []


{-| What the dialog showing this change is called: when it happened, after the
name it was given if it has one. The panel makes it because the panel is where
the change and its version are; the page only shows what it is handed.
-}
viewTitle : AppState -> ProjectEvent -> Maybe ProjectVersion -> String
viewTitle appState event version =
    let
        moment =
            TimeUtils.toReadableDateTime appState.timeZone (ProjectEvent.getCreatedAt event)
    in
    case version of
        Just aVersion ->
            aVersion.name ++ " — " ++ moment

        Nothing ->
            moment


{-| What a path's question is called, or the path when the knowledge model no
longer has it.
-}
questionTitle : ProjectQuestionnaire -> String -> String
questionTitle questionnaire path =
    String.split "." path
        |> List.last
        |> Maybe.andThen (\uuid -> Dict.get uuid questionnaire.knowledgeModel.entities.questions)
        |> Maybe.map Question.getTitle
        |> Maybe.withDefault path
