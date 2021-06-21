module Wizard.Common.Components.Questionnaire.History exposing
    ( Model
    , Msg
    , ViewConfig
    , init
    , subscriptions
    , update
    , view
    )

import Bootstrap.Dropdown as Dropdown
import Dict exposing (Dict)
import Html exposing (Html, a, br, div, em, h5, img, input, label, li, span, strong, text, ul)
import Html.Attributes exposing (class, src, type_)
import Html.Events exposing (onCheck, onClick)
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question)
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent(..))
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.ClearReplyData exposing (ClearReplyData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetPhaseData exposing (SetPhaseData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetReplyData exposing (SetReplyData)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue exposing (ReplyValue(..))
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue.IntegrationReplyType exposing (IntegrationReplyType(..))
import Shared.Data.QuestionnaireVersion exposing (QuestionnaireVersion)
import Shared.Data.User as User
import Shared.Data.UserSuggestion exposing (UserSuggestion)
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Locale exposing (l, lh, lx)
import Shared.Utils exposing (flip)
import Time
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.ListingDropdown as ListingDropdown
import Wizard.Common.Components.QuestionnaireVersionTag as QuestionnaireVersionTag
import Wizard.Common.Html.Attribute exposing (linkToAttributes)
import Wizard.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute
import Wizard.Projects.Routes as ProjectRoute
import Wizard.Routes as Route


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Components.Questionnaire.History"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Common.Components.Questionnaire.History"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Components.Questionnaire.History"



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


view : AppState -> ViewConfig msg -> Model -> Html msg
view appState cfg model =
    let
        filterVersions =
            if model.namedOnly then
                List.filter (QuestionnaireDetail.isVersion cfg.questionnaire)

            else
                identity

        filterEvents event =
            not (QuestionnaireEvent.isSetLabels event) && not (QuestionnaireEvent.isSetReplyList event)

        eventGroups =
            cfg.questionnaire.events
                |> List.filter filterEvents
                |> filterVersions
                |> List.foldl (groupEvents appState) []
                |> List.reverse
                |> List.map (viewEventsMonthGroup appState cfg model)

        namedOnlySelect =
            div [ class "form-check" ]
                [ label [ class "form-check-label form-check-toggle" ]
                    [ input [ type_ "checkbox", class "form-check-input", onCheck (cfg.wrapMsg << SetNamedOnly) ] []
                    , span [] [ lx_ "nameOnly.label" appState ]
                    ]
                ]
    in
    div [ class "history" ] (namedOnlySelect :: eventGroups)


viewEventsMonthGroup : AppState -> ViewConfig msg -> Model -> EventsMonthGroup -> Html msg
viewEventsMonthGroup appState cfg model group =
    let
        yearString =
            String.fromInt group.year

        monthString =
            TimeUtils.monthToString appState group.month

        dayGroups =
            List.map (viewEventsDayGroup appState cfg model group.year group.month) (List.reverse group.days)
    in
    div [ class "history-month" ]
        [ h5 [] [ text <| monthString ++ " " ++ yearString ]
        , div [] dayGroups
        ]


viewEventsDayGroup : AppState -> ViewConfig msg -> Model -> Int -> Time.Month -> EventsDayGroup -> Html msg
viewEventsDayGroup appState cfg model year month group =
    let
        yearString =
            String.fromInt year

        monthString =
            String.fromInt (TimeUtils.monthToInt month)

        dayString =
            String.fromInt group.day

        dateString =
            dayString ++ ". " ++ monthString ++ "."

        identifier =
            yearString ++ "-" ++ monthString ++ "-" ++ dayString

        isExpanded =
            List.member identifier model.expandedDays

        events =
            List.map (viewEvent appState cfg model) (List.reverse (filterDayEvents cfg.questionnaire group.events))

        users =
            group.events
                |> List.map QuestionnaireEvent.getCreatedBy
                |> List.sortBy (Maybe.unwrap "{" User.fullName)
                |> List.uniqueBy (Maybe.unwrap "" User.fullName)
                |> List.map (viewEventUser appState)

        content =
            if model.namedOnly then
                [ a [ class "date named-only-open" ]
                    [ strong [] [ text dateString ]
                    ]
                , div [] events
                ]

            else if isExpanded then
                [ a [ onClick (cfg.wrapMsg <| SetVersionDateCollapsed identifier), class "date open" ]
                    [ fa "fas fa-caret-down"
                    , strong [] [ text dateString ]
                    ]
                , div [] events
                ]

            else
                [ a [ onClick (cfg.wrapMsg <| SetVersionDateExpanded identifier), class "date closed" ]
                    [ fa "fas fa-caret-right"
                    , strong [] [ text dateString ]
                    ]
                , div [ class "history-day-users" ] users
                ]
    in
    div [ class "history-day" ] content


viewEvent : AppState -> ViewConfig msg -> Model -> QuestionnaireEvent -> Html msg
viewEvent appState cfg model event =
    div [ class "history-event" ]
        [ viewEventHeader appState cfg model event
        , viewEventBadges appState cfg event
        , viewEventDetail appState cfg event
        , viewEventUser appState (QuestionnaireEvent.getCreatedBy event)
        ]


viewEventHeader : AppState -> ViewConfig msg -> Model -> QuestionnaireEvent -> Html msg
viewEventHeader appState cfg model event =
    let
        dropdown =
            viewEventHeaderDropdown appState cfg model event

        readableTime =
            TimeUtils.toReadableTime appState.timeZone (QuestionnaireEvent.getCreatedAt event)
    in
    div [ class "event-header" ]
        [ text readableTime
        , dropdown
        ]


viewEventHeaderDropdown : AppState -> ViewConfig msg -> Model -> QuestionnaireEvent -> Html msg
viewEventHeaderDropdown appState cfg model event =
    let
        eventUuid =
            QuestionnaireEvent.getUuid event

        eventUuidString =
            Uuid.toString eventUuid

        mbVersion =
            QuestionnaireDetail.getVersionByEventUuid cfg.questionnaire eventUuid

        versionActions =
            case mbVersion of
                Just version ->
                    [ Dropdown.anchorItem [ onClick (cfg.renameVersionMsg version) ]
                        [ faSet "_global.edit" appState
                        , lx_ "action.rename" appState
                        ]
                    , Dropdown.anchorItem [ onClick (cfg.deleteVersionMsg version), class "text-danger" ]
                        [ faSet "_global.delete" appState
                        , lx_ "action.delete" appState
                        ]
                    ]

                Nothing ->
                    [ Dropdown.anchorItem [ onClick (cfg.createVersionMsg eventUuid) ]
                        [ faSet "_global.edit" appState
                        , lx_ "action.name" appState
                        ]
                    ]

        previewAction =
            case ( cfg.previewQuestionnaireEventMsg, QuestionnaireDetail.isCurrentVersion cfg.questionnaire eventUuid ) of
                ( Just viewMsg, False ) ->
                    let
                        newDocumentRoute =
                            Route.ProjectsRoute <|
                                ProjectRoute.DetailRoute cfg.questionnaire.uuid <|
                                    ProjectDetailRoute.NewDocument (Just eventUuidString)

                        createDocumentAttributes =
                            linkToAttributes appState newDocumentRoute
                    in
                    [ Dropdown.divider
                    , Dropdown.anchorItem [ onClick (viewMsg eventUuid) ]
                        [ faSet "_global.questionnaire" appState
                        , lx_ "action.viewQuestionnaire" appState
                        ]
                    , Dropdown.anchorItem createDocumentAttributes
                        [ faSet "questionnaire.history.createDocument" appState
                        , lx_ "action.createDocument" appState
                        ]
                    ]

                _ ->
                    []

        revertAction =
            case ( cfg.revertQuestionnaireMsg, QuestionnaireDetail.isCurrentVersion cfg.questionnaire eventUuid ) of
                ( Just revertMsg, False ) ->
                    [ Dropdown.divider
                    , Dropdown.anchorItem [ onClick (revertMsg event), class "text-danger" ]
                        [ faSet "questionnaire.history.revert" appState
                        , lx_ "action.revert" appState
                        ]
                    ]

                _ ->
                    []

        dropdownState =
            Maybe.withDefault Dropdown.initialState <|
                Dict.get eventUuidString model.dropdownStates
    in
    ListingDropdown.dropdown appState
        { dropdownState = dropdownState
        , toggleMsg = cfg.wrapMsg << DropdownMsg eventUuidString
        , items = versionActions ++ previewAction ++ revertAction
        }


viewEventBadges : AppState -> ViewConfig msg -> QuestionnaireEvent -> Html msg
viewEventBadges appState cfg event =
    let
        eventUuid =
            QuestionnaireEvent.getUuid event

        currentVersionBadge =
            if QuestionnaireDetail.isCurrentVersion cfg.questionnaire eventUuid then
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
        [ em [] [ lx_ "event.cleared" appState, br [] [], linkToQuestion cfg question data.path ] ]


viewEventDetailSetLevel : AppState -> ViewConfig msg -> SetPhaseData -> Html msg
viewEventDetailSetLevel appState cfg data =
    let
        mbLevel =
            List.find (.uuid >> (==) (Uuid.toString data.uuid)) (KnowledgeModel.getPhases cfg.questionnaire.knowledgeModel)

        levelName =
            Maybe.unwrap "" .title mbLevel
    in
    div [ class "event-detail" ]
        [ em [] (lh_ "event.phase" [ strong [] [ text levelName ] ] appState) ]


viewEventUser : AppState -> Maybe UserSuggestion -> Html msg
viewEventUser appState mbUser =
    let
        ( imageUrl, userName ) =
            case mbUser of
                Just user ->
                    ( User.imageUrlOrGravatar user, User.fullName user )

                Nothing ->
                    ( User.defaultGravatar, l_ "user.anonymous" appState )
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
            let
                createdBy =
                    QuestionnaireEvent.getCreatedBy event
            in
            if QuestionnaireEvent.isInvisible event then
                acc

            else
                case QuestionnaireEvent.getPath event of
                    Just eventPath ->
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
