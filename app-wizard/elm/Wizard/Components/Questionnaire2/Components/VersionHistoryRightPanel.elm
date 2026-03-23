module Wizard.Components.Questionnaire2.Components.VersionHistoryRightPanel exposing
    ( Model
    , Msg
    , ViewConfig
    , addEvent
    , init
    , initMsg
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Bootstrap.Dropdown as Dropdown
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Components.ActionResultBlock as ActionResultBlock
import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (fa, faDelete, faDetailShowAll, faEdit, faKmAnswer, faKmChoice, faQuestionnaire, faQuestionnaireHistoryCreateDocument, faQuestionnaireHistoryRevert)
import Common.Utils.FileIcon as FileIcon
import Common.Utils.Markdown as Markdown
import Common.Utils.TimeUtils as TimeUtils
import Dict exposing (Dict)
import Flip exposing (flip)
import Gettext exposing (gettext)
import Html exposing (Html, a, br, div, em, h5, img, input, label, li, span, strong, text, ul)
import Html.Attributes exposing (checked, class, src, type_)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onCheck, onClick)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import String.Format as String
import Task.Extra as Task
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.ClearReplyData exposing (ClearReplyData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.SetPhaseData exposing (SetPhaseData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.SetReplyData exposing (SetReplyData)
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue.IntegrationReplyType as IntegrationReplyType
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Api.Models.ProjectVersion as ProjectVersion exposing (ProjectVersion)
import Wizard.Api.Models.User as User
import Wizard.Api.Projects as ProjectsApi
import Wizard.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..))
import Wizard.Components.Questionnaire2.Components.DeleteVersionModal as DeleteVersionModal
import Wizard.Components.Questionnaire2.Components.VersionModal as VersionModal
import Wizard.Components.QuestionnaireVersionTag as QuestionnaireVersionTag
import Wizard.Data.AppState exposing (AppState)
import Wizard.Data.Session as Session
import Wizard.Routes as Routes
import Wizard.Utils.ProjectUtils as ProjectUtils


type alias Model =
    { projectUuid : Uuid
    , projectEvents : ActionResult (List ProjectEvent)
    , projectEventsExtraLoading : ActionResult ()
    , projectEventsPage : Int
    , projectEventsLoadMore : Bool
    , projectVersions : ActionResult (List ProjectVersion)
    , expandedDays : List String
    , dropdownStates : Dict String Dropdown.State
    , versionModalModel : VersionModal.Model
    , deleteVersionModalModel : DeleteVersionModal.Model
    }


init : AppState -> Uuid -> Model
init appState projectUuid =
    let
        yearString =
            String.fromInt (Time.toYear appState.timeZone appState.currentTime)

        monthString =
            String.fromInt (TimeUtils.monthToInt (Time.toMonth appState.timeZone appState.currentTime))

        dayString =
            String.fromInt (Time.toDay appState.timeZone appState.currentTime)

        identifier =
            yearString ++ "-" ++ monthString ++ "-" ++ dayString
    in
    { projectUuid = projectUuid
    , projectEvents = ActionResult.Unset
    , projectEventsExtraLoading = ActionResult.Unset
    , projectEventsPage = 0
    , projectEventsLoadMore = False
    , projectVersions = ActionResult.Unset
    , expandedDays = [ identifier ]
    , dropdownStates = Dict.empty
    , versionModalModel = VersionModal.init
    , deleteVersionModalModel = DeleteVersionModal.init
    }


addEvent : ProjectEvent -> Model -> Model
addEvent event model =
    case model.projectEvents of
        ActionResult.Success events ->
            { model | projectEvents = ActionResult.Success (events ++ [ event ]) }

        _ ->
            model


type Msg
    = Init
    | GetQuestionnaireEventsCompleted Int (Result ApiError (Pagination ProjectEvent))
    | GetQuestionnaireVersionsCompleted (Result ApiError (List ProjectVersion))
    | LoadMore
    | SetVersionDateExpanded String
    | SetVersionDateCollapsed String
    | DropdownMsg String Dropdown.State
    | VersionModalMsg VersionModal.Msg
    | DeleteVersionModalMsg DeleteVersionModal.Msg
    | OpenAddVersionModal Uuid
    | OpenRenameVersionModal ProjectVersion
    | OpenDeleteVersionModal ProjectVersion
    | AddVersion ProjectVersion
    | RenameVersion ProjectVersion
    | DeleteVersion ProjectVersion


initMsg : Msg
initMsg =
    Init


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    let
        withNoCmd newModel =
            ( newModel, Cmd.none )
    in
    case msg of
        Init ->
            ( { model
                | projectEvents = ActionResult.Loading
                , projectEventsExtraLoading = ActionResult.Unset
                , projectEventsPage = 0
                , projectEventsLoadMore = False
                , projectVersions = ActionResult.Loading
              }
            , Cmd.batch
                [ ProjectsApi.getEvents appState model.projectUuid 0 (GetQuestionnaireEventsCompleted 0)
                , ProjectsApi.getVersions appState model.projectUuid GetQuestionnaireVersionsCompleted
                ]
            )

        GetQuestionnaireEventsCompleted page result ->
            withNoCmd <|
                case result of
                    Ok questionnaireEvents ->
                        if page == questionnaireEvents.page.number then
                            let
                                currentItems =
                                    ActionResult.withDefault [] model.projectEvents
                            in
                            { model
                                | projectEvents = ActionResult.Success (List.reverse questionnaireEvents.items ++ currentItems)
                                , projectEventsExtraLoading = ActionResult.Unset
                                , projectEventsPage = page + 1
                                , projectEventsLoadMore = page + 1 < questionnaireEvents.page.totalPages
                            }

                        else
                            model

                    Err _ ->
                        { model | projectEvents = ActionResult.Error (gettext "Unable to get version history." appState.locale) }

        GetQuestionnaireVersionsCompleted result ->
            withNoCmd <|
                case result of
                    Ok questionnaireVersions ->
                        { model | projectVersions = ActionResult.Success questionnaireVersions }

                    Err _ ->
                        { model | projectVersions = ActionResult.Error (gettext "Unable to get version history." appState.locale) }

        LoadMore ->
            withNoCmd model

        SetVersionDateExpanded date ->
            withNoCmd { model | expandedDays = date :: model.expandedDays }

        SetVersionDateCollapsed date ->
            withNoCmd { model | expandedDays = List.filter ((/=) date) model.expandedDays }

        DropdownMsg uuid state ->
            withNoCmd { model | dropdownStates = Dict.insert uuid state model.dropdownStates }

        VersionModalMsg versionModalMsg ->
            let
                ( newVersionModalModel, versionModalCmd ) =
                    VersionModal.update
                        { wrapMsg = VersionModalMsg
                        , projectUuid = model.projectUuid
                        , addVersionCmd = Task.dispatch << AddVersion
                        , renameVersionCmd = Task.dispatch << RenameVersion
                        }
                        appState
                        versionModalMsg
                        model.versionModalModel
            in
            ( { model | versionModalModel = newVersionModalModel }, versionModalCmd )

        DeleteVersionModalMsg deleteVersionModalMsg ->
            let
                ( newDeleteVersionModalModel, deleteVersionModalCmd ) =
                    DeleteVersionModal.update
                        { wrapMsg = DeleteVersionModalMsg
                        , projectUuid = model.projectUuid
                        , deleteVersionCmd = Task.dispatch << DeleteVersion
                        }
                        appState
                        deleteVersionModalMsg
                        model.deleteVersionModalModel
            in
            ( { model | deleteVersionModalModel = newDeleteVersionModalModel }, deleteVersionModalCmd )

        OpenAddVersionModal eventUuid ->
            withNoCmd { model | versionModalModel = VersionModal.setEventUuid eventUuid model.versionModalModel }

        OpenRenameVersionModal version ->
            withNoCmd { model | versionModalModel = VersionModal.setVersion version model.versionModalModel }

        OpenDeleteVersionModal version ->
            withNoCmd { model | deleteVersionModalModel = DeleteVersionModal.setVersion version model.deleteVersionModalModel }

        AddVersion questionnaireVersion ->
            withNoCmd { model | projectVersions = ActionResult.map ((::) questionnaireVersion) model.projectVersions }

        RenameVersion questionnaireVersion ->
            let
                updateVersion version =
                    if version.uuid == questionnaireVersion.uuid then
                        { version | name = questionnaireVersion.name, description = questionnaireVersion.description }

                    else
                        version

                questionnaireVersions =
                    ActionResult.map (List.map updateVersion) model.projectVersions
            in
            withNoCmd { model | projectVersions = questionnaireVersions }

        DeleteVersion questionnaireVersion ->
            let
                questionnaireVersions =
                    ActionResult.map (List.filter (not << (==) questionnaireVersion.uuid << .uuid)) model.projectVersions
            in
            withNoCmd { model | projectVersions = questionnaireVersions }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        toSubscription ( uuid, state ) =
            Dropdown.subscriptions state (DropdownMsg uuid)
    in
    model.dropdownStates
        |> Dict.toList
        |> List.map toSubscription
        |> Sub.batch


type alias ViewConfig msg =
    { namedOnly : Bool
    , onToggleNamedOnly : Bool -> msg
    , previewQuestionnaireEventMsg : Maybe (Uuid -> msg)
    , questionnaire : ProjectQuestionnaire
    , revertQuestionnaireMsg : Maybe (ProjectEvent -> msg)
    , scrollMsg : String -> msg
    , wrapMsg : Msg -> msg
    }


view : AppState -> ViewConfig msg -> Model -> Html msg
view appState cfg model =
    let
        versionsAndEvents =
            ActionResult.combine model.projectVersions model.projectEvents
    in
    ActionResultBlock.view
        { viewContent = viewVersionHistory appState cfg model
        , actionResult = versionsAndEvents
        , locale = appState.locale
        }


viewVersionHistory : AppState -> ViewConfig msg -> Model -> ( List ProjectVersion, List ProjectEvent ) -> Html msg
viewVersionHistory appState cfg model ( versions, events ) =
    let
        filterVersions =
            if cfg.namedOnly then
                List.filter (isVersion versions)

            else
                identity

        filterEvents event =
            not (ProjectEvent.isInvisible event)

        eventGroups =
            events
                |> List.filter filterEvents
                |> filterVersions
                |> List.foldl (groupEvents appState) []
                |> List.reverse
                |> List.map (viewEventsMonthGroup appState cfg model versions events)

        namedOnlySelect =
            div [ class "bg-light border-bottom px-3 py-2" ]
                [ label [ class "form-check-label form-check-toggle" ]
                    [ input
                        [ type_ "checkbox"
                        , class "form-check-input"
                        , checked cfg.namedOnly
                        , onCheck cfg.onToggleNamedOnly
                        ]
                        []
                    , span [] [ text (gettext "Named versions only" appState.locale) ]
                    ]
                ]

        viewAllButton =
            if ActionResult.isLoading model.projectEventsExtraLoading then
                Flash.loader appState.locale

            else if model.projectEventsLoadMore then
                div []
                    [ a [ onClick (cfg.wrapMsg LoadMore), class "with-icon" ]
                        [ faDetailShowAll
                        , text (gettext "Load older" appState.locale)
                        ]
                    ]

            else
                Html.nothing
    in
    div [ class "questionnaireRightPanelList questionnaireRightPanelList--noPadding questionnaireHistory" ]
        (namedOnlySelect
            :: eventGroups
            ++ [ viewAllButton
               , Html.map (cfg.wrapMsg << VersionModalMsg) <| VersionModal.view appState model.versionModalModel
               , Html.map (cfg.wrapMsg << DeleteVersionModalMsg) <| DeleteVersionModal.view appState model.deleteVersionModalModel
               ]
        )


viewEventsMonthGroup : AppState -> ViewConfig msg -> Model -> List ProjectVersion -> List ProjectEvent -> EventsMonthGroup -> Html msg
viewEventsMonthGroup appState cfg model versions events group =
    let
        yearString =
            String.fromInt group.year

        monthString =
            TimeUtils.monthToString appState group.month

        dayGroups =
            List.map (viewEventsDayGroup appState cfg model versions events group.month (createEventsDayGroupIdentifier group.year group.month)) (List.reverse group.days)
    in
    div [ class "questionnaireHistory__month" ]
        [ h5 [] [ text <| monthString ++ " " ++ yearString ]
        , div [] dayGroups
        ]


viewEventsDayGroup : AppState -> ViewConfig msg -> Model -> List ProjectVersion -> List ProjectEvent -> Time.Month -> (EventsDayGroup -> String) -> EventsDayGroup -> Html msg
viewEventsDayGroup appState cfg model versions events month getIdentifier group =
    let
        monthString =
            String.fromInt (TimeUtils.monthToInt month)

        dayString =
            String.fromInt group.day

        dateString =
            dayString ++ ". " ++ monthString ++ "."

        eventElements =
            List.map (viewEvent appState cfg model versions events) (List.reverse (filterDayEvents versions group.events))

        content =
            if cfg.namedOnly then
                [ a [ class "questionnaireHistory__date questionnaireHistory__date--namedOnlyOpen" ]
                    [ strong [] [ text dateString ]
                    ]
                , div [] eventElements
                ]

            else if List.member (getIdentifier group) model.expandedDays then
                [ a
                    [ onClick (cfg.wrapMsg <| SetVersionDateCollapsed (getIdentifier group))
                    , class "questionnaireHistory__date questionnaireHistory__date--open"
                    ]
                    [ fa "fas fa-caret-down"
                    , strong [] [ text dateString ]
                    ]
                , div [] eventElements
                ]

            else
                let
                    users =
                        group.events
                            |> List.map ProjectEvent.getCreatedBy
                            |> List.sortBy (Maybe.unwrap "{" User.fullName)
                            |> List.uniqueBy (Maybe.unwrap "" User.fullName)
                            |> List.map (viewEventUser appState)
                in
                [ a
                    [ onClick (cfg.wrapMsg <| SetVersionDateExpanded (getIdentifier group))
                    , class "questionnaireHistory__date questionnaireHistory__date--closed"
                    ]
                    [ fa "fas fa-caret-right"
                    , strong [] [ text dateString ]
                    ]
                , div [ class "questionnaireHistory__dayUsers" ] users
                ]
    in
    div [ class "questionnaireHistory__day" ] content


viewEvent : AppState -> ViewConfig msg -> Model -> List ProjectVersion -> List ProjectEvent -> ProjectEvent -> Html msg
viewEvent appState cfg model versions events event =
    div [ class "questionnaireHistory__event" ]
        [ viewEventHeader appState cfg model versions events event
        , viewEventBadges appState versions events event
        , viewEventDetail appState cfg event
        , viewEventUser appState (ProjectEvent.getCreatedBy event)
        ]


viewEventHeader : AppState -> ViewConfig msg -> Model -> List ProjectVersion -> List ProjectEvent -> ProjectEvent -> Html msg
viewEventHeader appState cfg model versions events event =
    let
        dropdown =
            viewEventHeaderDropdown appState cfg model versions events event

        readableTime =
            TimeUtils.toReadableTime appState.timeZone (ProjectEvent.getCreatedAt event)
    in
    div [ class "questionnaireHistory__eventHeader" ]
        [ text readableTime
        , dropdown
        ]


viewEventHeaderDropdown : AppState -> ViewConfig msg -> Model -> List ProjectVersion -> List ProjectEvent -> ProjectEvent -> Html msg
viewEventHeaderDropdown appState cfg model versions events event =
    let
        eventUuid =
            ProjectEvent.getUuid event

        isOwner =
            ProjectUtils.isOwner appState cfg.questionnaire

        mbVersion =
            ProjectVersion.getVersionByEventUuid versions eventUuid

        versionGroup =
            case mbVersion of
                Just version ->
                    [ ( ListingDropdown.dropdownAction
                            { extraClass = Nothing
                            , icon = faEdit
                            , label = gettext "Rename this version" appState.locale
                            , msg = ListingActionMsg (cfg.wrapMsg (OpenRenameVersionModal version))
                            , dataCy = "rename"
                            }
                      , isOwner
                      )
                    , ( ListingDropdown.dropdownAction
                            { extraClass = Just "text-danger"
                            , icon = faDelete
                            , label = gettext "Delete this version" appState.locale
                            , msg = ListingActionMsg (cfg.wrapMsg (OpenDeleteVersionModal version))
                            , dataCy = "delete"
                            }
                      , isOwner
                      )
                    ]

                Nothing ->
                    [ ( ListingDropdown.dropdownAction
                            { extraClass = Nothing
                            , icon = faEdit
                            , label = gettext "Name this version" appState.locale
                            , msg = ListingActionMsg (cfg.wrapMsg (OpenAddVersionModal eventUuid))
                            , dataCy = "view"
                            }
                      , isOwner
                      )
                    ]

        previewGroup =
            case ( cfg.previewQuestionnaireEventMsg, ProjectQuestionnaire.isCurrentVersion events eventUuid ) of
                ( Just viewMsg, False ) ->
                    let
                        viewQuestionnaireAction =
                            ListingDropdown.dropdownAction
                                { extraClass = Nothing
                                , icon = faQuestionnaire
                                , label = gettext "View questionnaire" appState.locale
                                , msg = ListingActionMsg (viewMsg eventUuid)
                                , dataCy = "view-questionnaire"
                                }

                        createDocumentAction =
                            ListingDropdown.dropdownAction
                                { extraClass = Nothing
                                , icon = faQuestionnaireHistoryCreateDocument
                                , label = gettext "Create document" appState.locale
                                , msg = ListingActionLink (Routes.projectsDetailDocumentsNew cfg.questionnaire.uuid (Just eventUuid))
                                , dataCy = "create-document"
                                }
                    in
                    [ ( viewQuestionnaireAction, True )
                    , ( createDocumentAction, Session.exists appState.session )
                    ]

                _ ->
                    []

        revertGroup =
            case cfg.revertQuestionnaireMsg of
                Just revertMsg ->
                    [ ( ListingDropdown.dropdownAction
                            { extraClass = Just "text-danger"
                            , icon = faQuestionnaireHistoryRevert
                            , label = gettext "Revert to this version" appState.locale
                            , msg = ListingActionMsg (revertMsg event)
                            , dataCy = "revert"
                            }
                      , not (ProjectQuestionnaire.isCurrentVersion events eventUuid) && isOwner
                      )
                    ]

                Nothing ->
                    []

        groups =
            [ versionGroup
            , previewGroup
            , revertGroup
            ]

        items =
            ListingDropdown.itemsFromGroups groups

        eventUuidString =
            Uuid.toString eventUuid

        dropdownState =
            Maybe.withDefault Dropdown.initialState <|
                Dict.get eventUuidString model.dropdownStates
    in
    Html.viewIf (not (List.isEmpty items)) <|
        ListingDropdown.dropdown
            { dropdownState = dropdownState
            , toggleMsg = cfg.wrapMsg << DropdownMsg eventUuidString
            , items = items
            }


viewEventBadges : AppState -> List ProjectVersion -> List ProjectEvent -> ProjectEvent -> Html msg
viewEventBadges appState versions events event =
    let
        eventUuid =
            ProjectEvent.getUuid event

        currentVersionBadge =
            if ProjectQuestionnaire.isCurrentVersion events eventUuid then
                QuestionnaireVersionTag.current appState

            else
                Html.nothing

        versionNameBadge =
            case ProjectVersion.getVersionByEventUuid versions eventUuid of
                Just version ->
                    QuestionnaireVersionTag.version version

                Nothing ->
                    Html.nothing
    in
    div [ class "questionnaireHistory__eventBadges" ] [ currentVersionBadge, versionNameBadge ]


viewEventDetail : AppState -> ViewConfig msg -> ProjectEvent -> Html msg
viewEventDetail appState cfg event =
    let
        mbQuestion =
            Maybe.unwrap Nothing
                (flip KnowledgeModel.getQuestion cfg.questionnaire.knowledgeModel)
                (ProjectEvent.getQuestionUuid event)
    in
    case ( event, mbQuestion ) of
        ( ProjectEvent.SetReply data, Just question ) ->
            viewEventDetailSetReply appState cfg data question

        ( ProjectEvent.ClearReply data, Just question ) ->
            viewEventDetailClearReply appState cfg data question

        ( ProjectEvent.SetPhase data, _ ) ->
            viewEventDetailSetLevel appState cfg data

        _ ->
            Html.nothing


viewEventDetailSetReply : AppState -> ViewConfig msg -> SetReplyData -> Question -> Html msg
viewEventDetailSetReply appState cfg data question =
    let
        replyView ( icon, replyText ) =
            li []
                [ span [ class "fa-li" ] [ icon ]
                , span [ class "fa-li-content" ] [ text replyText ]
                ]

        eventView replies =
            div [ class "questionnaireHistory__eventDetail" ]
                [ em [] [ linkToQuestion cfg question data.path ]
                , ul [ class "fa-ul" ] (List.map replyView replies)
                ]
    in
    case data.value of
        ReplyValue.StringReply reply ->
            eventView [ ( fa "far fa-edit", reply ) ]

        ReplyValue.AnswerReply answerUuid ->
            let
                answerText =
                    Maybe.unwrap "" .label (KnowledgeModel.getAnswer answerUuid cfg.questionnaire.knowledgeModel)
            in
            eventView [ ( faKmAnswer, answerText ) ]

        ReplyValue.MultiChoiceReply choiceUuids ->
            let
                choices =
                    KnowledgeModel.getQuestionChoices (Question.getUuid question) cfg.questionnaire.knowledgeModel
                        |> List.filter (.uuid >> flip List.member choiceUuids)
                        |> List.map (\choice -> ( faKmChoice, choice.label ))
            in
            eventView choices

        ReplyValue.IntegrationReply replyType ->
            case replyType of
                IntegrationReplyType.PlainType reply ->
                    eventView [ ( fa "far fa-edit", reply ) ]

                IntegrationReplyType.IntegrationType reply _ ->
                    eventView [ ( fa "fas fa-link", Markdown.toString reply ) ]

        ReplyValue.ItemSelectReply itemUuid ->
            let
                itemLabel =
                    ProjectQuestionnaire.getItemSelectQuestionValueLabel appState cfg.questionnaire (Question.getUuid question) itemUuid
            in
            eventView [ ( fa "far fa-square-caret-down", itemLabel ) ]

        ReplyValue.FileReply fileUuid ->
            case ProjectQuestionnaire.getFile cfg.questionnaire fileUuid of
                Just file ->
                    eventView
                        [ ( fa (FileIcon.getFileIcon file.fileName file.contentType)
                          , file.fileName
                          )
                        ]

                Nothing ->
                    eventView
                        [ ( fa FileIcon.defaultIcon, gettext "Deleted file" appState.locale ) ]

        _ ->
            Html.nothing


viewEventDetailClearReply : AppState -> ViewConfig msg -> ClearReplyData -> Question -> Html msg
viewEventDetailClearReply appState cfg data question =
    div [ class "questionnaireHistory__eventDetail" ]
        [ em []
            [ text (gettext "Cleared reply of" appState.locale)
            , br [] []
            , linkToQuestion cfg question data.path
            ]
        ]


viewEventDetailSetLevel : AppState -> ViewConfig msg -> SetPhaseData -> Html msg
viewEventDetailSetLevel appState cfg data =
    let
        phaseEventDescription =
            case data.phaseUuid of
                Nothing ->
                    [ text (gettext "Unset phase" appState.locale) ]

                Just phaseUuid ->
                    let
                        mbPhase =
                            List.find (.uuid >> (==) (Uuid.toString phaseUuid)) (KnowledgeModel.getPhases cfg.questionnaire.knowledgeModel)

                        phaseName =
                            Maybe.unwrap (gettext "Unknown phase" appState.locale) .title mbPhase
                    in
                    String.formatHtml (gettext "Set phase to %s" appState.locale) [ strong [] [ text phaseName ] ]
    in
    div [ class "questionnaireHistory__eventDetail" ]
        [ em [] phaseEventDescription ]


viewEventUser : AppState -> Maybe UserSuggestion -> Html msg
viewEventUser appState mbUser =
    let
        ( imageUrl, userName ) =
            case mbUser of
                Just user ->
                    ( User.imageUrlOrGravatar user, User.fullName user )

                Nothing ->
                    ( User.defaultGravatar, gettext "Anonymous user" appState.locale )
    in
    div
        [ class "text-secondary d-flex align-items-center"
        , dataCy "questionnaire_history-event_user"
        ]
        [ img [ src imageUrl, class "user-icon user-icon-small me-1" ] []
        , text userName
        ]



-- UTILS


type alias EventsMonthGroup =
    { days : List EventsDayGroup
    , month : Time.Month
    , year : Int
    }


type alias EventsDayGroup =
    { day : Int
    , events : List ProjectEvent
    }


createEventsDayGroupIdentifier : Int -> Time.Month -> EventsDayGroup -> String
createEventsDayGroupIdentifier year month group =
    let
        yearString =
            String.fromInt year

        monthString =
            String.fromInt (TimeUtils.monthToInt month)

        dayString =
            String.fromInt group.day
    in
    yearString ++ "-" ++ monthString ++ "-" ++ dayString


groupEvents : AppState -> ProjectEvent -> List EventsMonthGroup -> List EventsMonthGroup
groupEvents appState event groups =
    let
        eventDay =
            Time.toDay appState.timeZone <| ProjectEvent.getCreatedAt event

        eventMonth =
            Time.toMonth appState.timeZone <| ProjectEvent.getCreatedAt event

        eventYear =
            Time.toYear appState.timeZone <| ProjectEvent.getCreatedAt event

        newMonthGroup =
            { days = []
            , month = eventMonth
            , year = eventYear
            }

        ( monthGroups, currentMonthGroup ) =
            case List.last groups of
                Just monthGroup ->
                    if monthGroup.month == eventMonth && monthGroup.year == eventYear then
                        ( List.take (List.length groups - 1) groups, monthGroup )

                    else
                        ( groups, newMonthGroup )

                Nothing ->
                    ( groups, newMonthGroup )

        newDayGroup =
            { day = eventDay
            , events = []
            }

        ( dayGroups, currentDayGroup ) =
            case List.last currentMonthGroup.days of
                Just dayGroup ->
                    if dayGroup.day == eventDay then
                        ( List.take (List.length currentMonthGroup.days - 1) currentMonthGroup.days
                        , dayGroup
                        )

                    else
                        ( currentMonthGroup.days, newDayGroup )

                Nothing ->
                    ( currentMonthGroup.days, newDayGroup )

        currentDayGroupWithEvent =
            { currentDayGroup | events = currentDayGroup.events ++ [ event ] }

        currentMonthGroupWithEvent =
            { currentMonthGroup | days = dayGroups ++ [ currentDayGroupWithEvent ] }
    in
    monthGroups ++ [ currentMonthGroupWithEvent ]


filterDayEvents : List ProjectVersion -> List ProjectEvent -> List ProjectEvent
filterDayEvents versions events =
    let
        defaultAcc =
            { questions = Dict.empty
            , events = []
            }

        fold event acc =
            if ProjectEvent.isInvisible event then
                acc

            else
                case ProjectEvent.getPath event of
                    Just eventPath ->
                        let
                            createdBy =
                                ProjectEvent.getCreatedBy event
                        in
                        if not (isVersion versions event) && Maybe.unwrap False ((==) createdBy) (Dict.get eventPath acc.questions) then
                            acc

                        else
                            { questions = Dict.insert eventPath createdBy acc.questions
                            , events = event :: acc.events
                            }

                    Nothing ->
                        { acc | events = event :: acc.events }
    in
    (List.foldr fold defaultAcc events).events


linkToQuestion : ViewConfig msg -> Question -> String -> Html msg
linkToQuestion cfg question path =
    a [ onClick <| cfg.scrollMsg path ] [ text (Question.getTitle question) ]


isVersion : List ProjectVersion -> ProjectEvent -> Bool
isVersion questionnaireVersions event =
    List.any (.eventUuid >> (==) (ProjectEvent.getUuid event)) questionnaireVersions
