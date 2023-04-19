module Wizard.Common.Components.Questionnaire.History exposing
    ( Model
    , Msg
    , ViewConfig
    , init
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Bootstrap.Dropdown as Dropdown
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, a, br, div, em, h5, img, input, label, li, span, strong, text, ul)
import Html.Attributes exposing (class, src, type_)
import Html.Events exposing (onCheck, onClick)
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Auth.Session as Session
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.KnowledgeModel.Question as Question exposing (Question)
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.ClearReplyData exposing (ClearReplyData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetPhaseData exposing (SetPhaseData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetReplyData exposing (SetReplyData)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue exposing (ReplyValue(..))
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue.IntegrationReplyType exposing (IntegrationReplyType(..))
import Shared.Data.QuestionnaireVersion exposing (QuestionnaireVersion)
import Shared.Data.User as User
import Shared.Data.UserSuggestion exposing (UserSuggestion)
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Utils exposing (flip)
import String.Format as String
import Time
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..))
import Wizard.Common.Components.QuestionnaireVersionTag as QuestionnaireVersionTag
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes



-- MODEL


type alias Model =
    { expandedDays : List String
    , dropdownStates : Dict String Dropdown.State
    , namedOnly : Bool
    }


init : AppState -> Model
init appState =
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
    { expandedDays = [ identifier ]
    , dropdownStates = Dict.empty
    , namedOnly = False
    }



-- UPDATE


type Msg
    = SetVersionDateExpanded String
    | SetVersionDateCollapsed String
    | DropdownMsg String Dropdown.State
    | SetNamedOnly Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetVersionDateExpanded date ->
            { model | expandedDays = date :: model.expandedDays }

        SetVersionDateCollapsed date ->
            { model | expandedDays = List.filter ((/=) date) model.expandedDays }

        DropdownMsg uuid state ->
            { model | dropdownStates = Dict.insert uuid state model.dropdownStates }

        SetNamedOnly namedOnly ->
            { model | namedOnly = namedOnly }



-- SUBSCRIPTIONS


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



-- VIEW


type alias ViewConfig msg =
    { questionnaire : QuestionnaireDetail
    , wrapMsg : Msg -> msg
    , scrollMsg : String -> msg
    , createVersionMsg : Uuid -> msg
    , renameVersionMsg : QuestionnaireVersion -> msg
    , deleteVersionMsg : QuestionnaireVersion -> msg
    , previewQuestionnaireEventMsg : Maybe (Uuid -> msg)
    , revertQuestionnaireMsg : Maybe (QuestionnaireEvent -> msg)
    }


view : AppState -> ViewConfig msg -> Model -> ActionResult (List QuestionnaireEvent) -> Html msg
view appState cfg model questionnaireEvents =
    Page.actionResultView appState (viewHistory appState cfg model) questionnaireEvents


viewHistory : AppState -> ViewConfig msg -> Model -> List QuestionnaireEvent -> Html msg
viewHistory appState cfg model questionnaireEvents =
    let
        filterVersions =
            if model.namedOnly then
                List.filter (QuestionnaireDetail.isVersion cfg.questionnaire)

            else
                identity

        filterEvents event =
            not (QuestionnaireEvent.isInvisible event)

        eventGroups =
            questionnaireEvents
                |> List.filter filterEvents
                |> filterVersions
                |> List.foldl (groupEvents appState) []
                |> List.reverse
                |> List.map (viewEventsMonthGroup appState cfg model questionnaireEvents)

        namedOnlySelect =
            div [ class "form-check" ]
                [ label [ class "form-check-label form-check-toggle" ]
                    [ input [ type_ "checkbox", class "form-check-input", onCheck (cfg.wrapMsg << SetNamedOnly) ] []
                    , span [] [ text (gettext "Named versions only" appState.locale) ]
                    ]
                ]
    in
    div [ class "history" ] (namedOnlySelect :: eventGroups)


viewEventsMonthGroup : AppState -> ViewConfig msg -> Model -> List QuestionnaireEvent -> EventsMonthGroup -> Html msg
viewEventsMonthGroup appState cfg model questionnaireEvents group =
    let
        yearString =
            String.fromInt group.year

        monthString =
            TimeUtils.monthToString appState group.month

        dayGroups =
            List.map (viewEventsDayGroup appState cfg model questionnaireEvents group.month (createEventsDayGroupIdentifier group.year group.month)) (List.reverse group.days)
    in
    div [ class "history-month" ]
        [ h5 [] [ text <| monthString ++ " " ++ yearString ]
        , div [] dayGroups
        ]


viewEventsDayGroup : AppState -> ViewConfig msg -> Model -> List QuestionnaireEvent -> Time.Month -> (EventsDayGroup -> String) -> EventsDayGroup -> Html msg
viewEventsDayGroup appState cfg model questionnaireEvents month getIdentifier group =
    let
        monthString =
            String.fromInt (TimeUtils.monthToInt month)

        dayString =
            String.fromInt group.day

        dateString =
            dayString ++ ". " ++ monthString ++ "."

        events =
            List.map (viewEvent appState cfg model questionnaireEvents) (List.reverse (filterDayEvents cfg.questionnaire group.events))

        content =
            if model.namedOnly then
                [ a [ class "date named-only-open" ]
                    [ strong [] [ text dateString ]
                    ]
                , div [] events
                ]

            else if List.member (getIdentifier group) model.expandedDays then
                [ a [ onClick (cfg.wrapMsg <| SetVersionDateCollapsed (getIdentifier group)), class "date open" ]
                    [ fa "fas fa-caret-down"
                    , strong [] [ text dateString ]
                    ]
                , div [] events
                ]

            else
                let
                    users =
                        group.events
                            |> List.map QuestionnaireEvent.getCreatedBy
                            |> List.sortBy (Maybe.unwrap "{" User.fullName)
                            |> List.uniqueBy (Maybe.unwrap "" User.fullName)
                            |> List.map (viewEventUser appState)
                in
                [ a [ onClick (cfg.wrapMsg <| SetVersionDateExpanded (getIdentifier group)), class "date closed" ]
                    [ fa "fas fa-caret-right"
                    , strong [] [ text dateString ]
                    ]
                , div [ class "history-day-users" ] users
                ]
    in
    div [ class "history-day" ] content


viewEvent : AppState -> ViewConfig msg -> Model -> List QuestionnaireEvent -> QuestionnaireEvent -> Html msg
viewEvent appState cfg model questionnaireEvents event =
    div [ class "history-event" ]
        [ viewEventHeader appState cfg model questionnaireEvents event
        , viewEventBadges appState cfg questionnaireEvents event
        , viewEventDetail appState cfg event
        , viewEventUser appState (QuestionnaireEvent.getCreatedBy event)
        ]


viewEventHeader : AppState -> ViewConfig msg -> Model -> List QuestionnaireEvent -> QuestionnaireEvent -> Html msg
viewEventHeader appState cfg model questionnaireEvents event =
    let
        dropdown =
            viewEventHeaderDropdown appState cfg model questionnaireEvents event

        readableTime =
            TimeUtils.toReadableTime appState.timeZone (QuestionnaireEvent.getCreatedAt event)
    in
    div [ class "event-header" ]
        [ text readableTime
        , dropdown
        ]


viewEventHeaderDropdown : AppState -> ViewConfig msg -> Model -> List QuestionnaireEvent -> QuestionnaireEvent -> Html msg
viewEventHeaderDropdown appState cfg model questionnaireEvents event =
    let
        eventUuid =
            QuestionnaireEvent.getUuid event

        isOwner =
            QuestionnaireDetail.isOwner appState cfg.questionnaire

        mbVersion =
            QuestionnaireDetail.getVersionByEventUuid cfg.questionnaire eventUuid

        versionGroup =
            case mbVersion of
                Just version ->
                    [ ( ListingDropdown.dropdownAction
                            { extraClass = Nothing
                            , icon = faSet "_global.edit" appState
                            , label = gettext "Rename this version" appState.locale
                            , msg = ListingActionMsg (cfg.renameVersionMsg version)
                            , dataCy = "rename"
                            }
                      , isOwner
                      )
                    , ( ListingDropdown.dropdownAction
                            { extraClass = Just "text-danger"
                            , icon = faSet "_global.delete" appState
                            , label = gettext "Delete this version" appState.locale
                            , msg = ListingActionMsg (cfg.deleteVersionMsg version)
                            , dataCy = "delete"
                            }
                      , isOwner
                      )
                    ]

                Nothing ->
                    [ ( ListingDropdown.dropdownAction
                            { extraClass = Nothing
                            , icon = faSet "_global.edit" appState
                            , label = gettext "Name this version" appState.locale
                            , msg = ListingActionMsg (cfg.createVersionMsg eventUuid)
                            , dataCy = "view"
                            }
                      , isOwner
                      )
                    ]

        previewGroup =
            case ( cfg.previewQuestionnaireEventMsg, QuestionnaireDetail.isCurrentVersion questionnaireEvents eventUuid ) of
                ( Just viewMsg, False ) ->
                    let
                        viewQuestionnaireAction =
                            ListingDropdown.dropdownAction
                                { extraClass = Nothing
                                , icon = faSet "_global.questionnaire" appState
                                , label = gettext "View questionnaire" appState.locale
                                , msg = ListingActionMsg (viewMsg eventUuid)
                                , dataCy = "view-questionnaire"
                                }

                        createDocumentAction =
                            ListingDropdown.dropdownAction
                                { extraClass = Nothing
                                , icon = faSet "questionnaire.history.createDocument" appState
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
                            , icon = faSet "questionnaire.history.revert" appState
                            , label = gettext "Revert to this version" appState.locale
                            , msg = ListingActionMsg (revertMsg event)
                            , dataCy = "revert"
                            }
                      , not (QuestionnaireDetail.isCurrentVersion questionnaireEvents eventUuid) && isOwner
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
    in
    if List.length items > 0 then
        let
            eventUuidString =
                Uuid.toString eventUuid

            dropdownState =
                Maybe.withDefault Dropdown.initialState <|
                    Dict.get eventUuidString model.dropdownStates
        in
        ListingDropdown.dropdown appState
            { dropdownState = dropdownState
            , toggleMsg = cfg.wrapMsg << DropdownMsg eventUuidString
            , items = items
            }

    else
        emptyNode


viewEventBadges : AppState -> ViewConfig msg -> List QuestionnaireEvent -> QuestionnaireEvent -> Html msg
viewEventBadges appState cfg questionnaireEvents event =
    let
        eventUuid =
            QuestionnaireEvent.getUuid event

        currentVersionBadge =
            if QuestionnaireDetail.isCurrentVersion questionnaireEvents eventUuid then
                QuestionnaireVersionTag.current appState

            else
                emptyNode

        versionNameBadge =
            case QuestionnaireDetail.getVersionByEventUuid cfg.questionnaire eventUuid of
                Just version ->
                    QuestionnaireVersionTag.version version

                Nothing ->
                    emptyNode
    in
    div [ class "event-badges" ] [ currentVersionBadge, versionNameBadge ]


viewEventDetail : AppState -> ViewConfig msg -> QuestionnaireEvent -> Html msg
viewEventDetail appState cfg event =
    let
        mbQuestion =
            Maybe.unwrap Nothing
                (flip KnowledgeModel.getQuestion cfg.questionnaire.knowledgeModel)
                (QuestionnaireEvent.getQuestionUuid event)
    in
    case ( event, mbQuestion ) of
        ( QuestionnaireEvent.SetReply data, Just question ) ->
            viewEventDetailSetReply appState cfg data question

        ( QuestionnaireEvent.ClearReply data, Just question ) ->
            viewEventDetailClearReply appState cfg data question

        ( QuestionnaireEvent.SetPhase data, _ ) ->
            viewEventDetailSetLevel appState cfg data

        _ ->
            emptyNode


viewEventDetailSetReply : AppState -> ViewConfig msg -> SetReplyData -> Question -> Html msg
viewEventDetailSetReply appState cfg data question =
    let
        replyView ( icon, replyText ) =
            li []
                [ span [ class "fa-li" ] [ icon ]
                , span [ class "fa-li-content" ] [ text replyText ]
                ]

        eventView replies =
            div [ class "event-detail" ]
                [ em [] [ linkToQuestion cfg question data.path ]
                , ul [ class "fa-ul" ] (List.map replyView replies)
                ]
    in
    case data.value of
        StringReply reply ->
            eventView [ ( fa "far fa-edit", reply ) ]

        AnswerReply answerUuid ->
            let
                answerText =
                    Maybe.unwrap "" .label (KnowledgeModel.getAnswer answerUuid cfg.questionnaire.knowledgeModel)
            in
            eventView [ ( faSet "km.answer" appState, answerText ) ]

        MultiChoiceReply choiceUuids ->
            let
                choices =
                    KnowledgeModel.getQuestionChoices (Question.getUuid question) cfg.questionnaire.knowledgeModel
                        |> List.filter (.uuid >> flip List.member choiceUuids)
                        |> List.map (\choice -> ( faSet "km.choice" appState, choice.label ))
            in
            eventView choices

        IntegrationReply replyType ->
            case replyType of
                PlainType reply ->
                    eventView [ ( fa "far fa-edit", reply ) ]

                IntegrationType _ reply ->
                    eventView [ ( fa "fas fa-link", reply ) ]

        _ ->
            emptyNode


viewEventDetailClearReply : AppState -> ViewConfig msg -> ClearReplyData -> Question -> Html msg
viewEventDetailClearReply appState cfg data question =
    div [ class "event-detail" ]
        [ em [] [ text (gettext "Cleared reply of" appState.locale), br [] [], linkToQuestion cfg question data.path ] ]


viewEventDetailSetLevel : AppState -> ViewConfig msg -> SetPhaseData -> Html msg
viewEventDetailSetLevel appState cfg data =
    let
        mbLevel =
            List.find (.uuid >> Just >> (==) (Maybe.map Uuid.toString data.phaseUuid)) (KnowledgeModel.getPhases cfg.questionnaire.knowledgeModel)

        levelName =
            Maybe.unwrap (gettext "Unknown phase" appState.locale) .title mbLevel
    in
    div [ class "event-detail" ]
        [ em [] (String.formatHtml (gettext "Set phase to %s" appState.locale) [ strong [] [ text levelName ] ]) ]


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
    div [ class "user" ]
        [ img [ src imageUrl, class "user-icon user-icon-small" ] []
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
    , events : List QuestionnaireEvent
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


groupEvents : AppState -> QuestionnaireEvent -> List EventsMonthGroup -> List EventsMonthGroup
groupEvents appState event groups =
    let
        eventDay =
            Time.toDay appState.timeZone <| QuestionnaireEvent.getCreatedAt event

        eventMonth =
            Time.toMonth appState.timeZone <| QuestionnaireEvent.getCreatedAt event

        eventYear =
            Time.toYear appState.timeZone <| QuestionnaireEvent.getCreatedAt event

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


filterDayEvents : QuestionnaireDetail -> List QuestionnaireEvent -> List QuestionnaireEvent
filterDayEvents questionnaire events =
    let
        defaultAcc =
            { questions = Dict.empty
            , events = []
            }

        fold event acc =
            if QuestionnaireEvent.isInvisible event then
                acc

            else
                case QuestionnaireEvent.getPath event of
                    Just eventPath ->
                        let
                            createdBy =
                                QuestionnaireEvent.getCreatedBy event
                        in
                        if not (QuestionnaireDetail.isVersion questionnaire event) && Maybe.unwrap False ((==) createdBy) (Dict.get eventPath acc.questions) then
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
