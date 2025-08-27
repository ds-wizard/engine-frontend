module Wizard.Common.Components.Questionnaire.History exposing
    ( Model
    , Msg(..)
    , ViewConfig
    , init
    , setNamedOnly
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Bootstrap.Dropdown as Dropdown
import Dict exposing (Dict)
import Flip exposing (flip)
import Gettext exposing (gettext)
import Html exposing (Html, a, br, div, em, h5, img, input, label, li, span, strong, text, ul)
import Html.Attributes exposing (checked, class, src, type_)
import Html.Events exposing (onCheck, onClick)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Components.FontAwesome exposing (fa, faDelete, faEdit, faKmAnswer, faKmChoice, faQuestionnaire, faQuestionnaireHistoryCreateDocument, faQuestionnaireHistoryRevert)
import Shared.Utils.Markdown as Markdown
import Shared.Utils.TimeUtils as TimeUtils
import String.Format as String
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent.ClearReplyData exposing (ClearReplyData)
import Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent.SetPhaseData exposing (SetPhaseData)
import Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent.SetReplyData exposing (SetReplyData)
import Wizard.Api.Models.QuestionnaireDetail.Reply.ReplyValue exposing (ReplyValue(..))
import Wizard.Api.Models.QuestionnaireDetail.Reply.ReplyValue.IntegrationReplyType exposing (IntegrationReplyType(..))
import Wizard.Api.Models.QuestionnaireQuestionnaire as QuestionnaireQuestionnaire exposing (QuestionnaireQuestionnaire)
import Wizard.Api.Models.QuestionnaireVersion as QuestionnaireVersion exposing (QuestionnaireVersion)
import Wizard.Api.Models.User as User
import Wizard.Api.Models.UserSuggestion exposing (UserSuggestion)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..))
import Wizard.Common.Components.QuestionnaireVersionTag as QuestionnaireVersionTag
import Wizard.Common.FileIcon as FileIcon
import Wizard.Common.QuestionnaireUtils as QuestionnaireUtils
import Wizard.Common.View.Page as Page
import Wizard.Data.Session as Session
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


setNamedOnly : Bool -> Model -> Model
setNamedOnly namedOnly model =
    { model | namedOnly = namedOnly }



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
    { questionnaire : QuestionnaireQuestionnaire
    , wrapMsg : Msg -> msg
    , scrollMsg : String -> msg
    , createVersionMsg : Uuid -> msg
    , renameVersionMsg : QuestionnaireVersion -> msg
    , deleteVersionMsg : QuestionnaireVersion -> msg
    , previewQuestionnaireEventMsg : Maybe (Uuid -> msg)
    , revertQuestionnaireMsg : Maybe (QuestionnaireEvent -> msg)
    }


view : AppState -> ViewConfig msg -> Model -> ActionResult ( List QuestionnaireVersion, List QuestionnaireEvent ) -> Html msg
view appState cfg model versionsAndEvents =
    Page.actionResultView appState (viewHistory appState cfg model) versionsAndEvents


viewHistory : AppState -> ViewConfig msg -> Model -> ( List QuestionnaireVersion, List QuestionnaireEvent ) -> Html msg
viewHistory appState cfg model ( versions, events ) =
    let
        filterVersions =
            if model.namedOnly then
                List.filter (isVersion versions)

            else
                identity

        filterEvents event =
            not (QuestionnaireEvent.isInvisible event)

        eventGroups =
            events
                |> List.filter filterEvents
                |> filterVersions
                |> List.foldl (groupEvents appState) []
                |> List.reverse
                |> List.map (viewEventsMonthGroup appState cfg model versions events)

        namedOnlySelect =
            div [ class "form-check" ]
                [ label [ class "form-check-label form-check-toggle" ]
                    [ input
                        [ type_ "checkbox"
                        , class "form-check-input"
                        , checked model.namedOnly
                        , onCheck (cfg.wrapMsg << SetNamedOnly)
                        ]
                        []
                    , span [] [ text (gettext "Named versions only" appState.locale) ]
                    ]
                ]
    in
    div [ class "history" ] (namedOnlySelect :: eventGroups)


viewEventsMonthGroup : AppState -> ViewConfig msg -> Model -> List QuestionnaireVersion -> List QuestionnaireEvent -> EventsMonthGroup -> Html msg
viewEventsMonthGroup appState cfg model versions events group =
    let
        yearString =
            String.fromInt group.year

        monthString =
            TimeUtils.monthToString appState group.month

        dayGroups =
            List.map (viewEventsDayGroup appState cfg model versions events group.month (createEventsDayGroupIdentifier group.year group.month)) (List.reverse group.days)
    in
    div [ class "history-month" ]
        [ h5 [] [ text <| monthString ++ " " ++ yearString ]
        , div [] dayGroups
        ]


viewEventsDayGroup : AppState -> ViewConfig msg -> Model -> List QuestionnaireVersion -> List QuestionnaireEvent -> Time.Month -> (EventsDayGroup -> String) -> EventsDayGroup -> Html msg
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
            if model.namedOnly then
                [ a [ class "date named-only-open" ]
                    [ strong [] [ text dateString ]
                    ]
                , div [] eventElements
                ]

            else if List.member (getIdentifier group) model.expandedDays then
                [ a [ onClick (cfg.wrapMsg <| SetVersionDateCollapsed (getIdentifier group)), class "date open" ]
                    [ fa "fas fa-caret-down"
                    , strong [] [ text dateString ]
                    ]
                , div [] eventElements
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


viewEvent : AppState -> ViewConfig msg -> Model -> List QuestionnaireVersion -> List QuestionnaireEvent -> QuestionnaireEvent -> Html msg
viewEvent appState cfg model versions events event =
    div [ class "history-event" ]
        [ viewEventHeader appState cfg model versions events event
        , viewEventBadges appState versions events event
        , viewEventDetail appState cfg event
        , viewEventUser appState (QuestionnaireEvent.getCreatedBy event)
        ]


viewEventHeader : AppState -> ViewConfig msg -> Model -> List QuestionnaireVersion -> List QuestionnaireEvent -> QuestionnaireEvent -> Html msg
viewEventHeader appState cfg model versions events event =
    let
        dropdown =
            viewEventHeaderDropdown appState cfg model versions events event

        readableTime =
            TimeUtils.toReadableTime appState.timeZone (QuestionnaireEvent.getCreatedAt event)
    in
    div [ class "event-header" ]
        [ text readableTime
        , dropdown
        ]


viewEventHeaderDropdown : AppState -> ViewConfig msg -> Model -> List QuestionnaireVersion -> List QuestionnaireEvent -> QuestionnaireEvent -> Html msg
viewEventHeaderDropdown appState cfg model versions events event =
    let
        eventUuid =
            QuestionnaireEvent.getUuid event

        isOwner =
            QuestionnaireUtils.isOwner appState cfg.questionnaire

        mbVersion =
            QuestionnaireVersion.getVersionByEventUuid versions eventUuid

        versionGroup =
            case mbVersion of
                Just version ->
                    [ ( ListingDropdown.dropdownAction
                            { extraClass = Nothing
                            , icon = faEdit
                            , label = gettext "Rename this version" appState.locale
                            , msg = ListingActionMsg (cfg.renameVersionMsg version)
                            , dataCy = "rename"
                            }
                      , isOwner
                      )
                    , ( ListingDropdown.dropdownAction
                            { extraClass = Just "text-danger"
                            , icon = faDelete
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
                            , icon = faEdit
                            , label = gettext "Name this version" appState.locale
                            , msg = ListingActionMsg (cfg.createVersionMsg eventUuid)
                            , dataCy = "view"
                            }
                      , isOwner
                      )
                    ]

        previewGroup =
            case ( cfg.previewQuestionnaireEventMsg, QuestionnaireQuestionnaire.isCurrentVersion events eventUuid ) of
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
                      , not (QuestionnaireQuestionnaire.isCurrentVersion events eventUuid) && isOwner
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
        ListingDropdown.dropdown
            { dropdownState = dropdownState
            , toggleMsg = cfg.wrapMsg << DropdownMsg eventUuidString
            , items = items
            }

    else
        Html.nothing


viewEventBadges : AppState -> List QuestionnaireVersion -> List QuestionnaireEvent -> QuestionnaireEvent -> Html msg
viewEventBadges appState versions events event =
    let
        eventUuid =
            QuestionnaireEvent.getUuid event

        currentVersionBadge =
            if QuestionnaireQuestionnaire.isCurrentVersion events eventUuid then
                QuestionnaireVersionTag.current appState

            else
                Html.nothing

        versionNameBadge =
            case QuestionnaireVersion.getVersionByEventUuid versions eventUuid of
                Just version ->
                    QuestionnaireVersionTag.version version

                Nothing ->
                    Html.nothing
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
            eventView [ ( faKmAnswer, answerText ) ]

        MultiChoiceReply choiceUuids ->
            let
                choices =
                    KnowledgeModel.getQuestionChoices (Question.getUuid question) cfg.questionnaire.knowledgeModel
                        |> List.filter (.uuid >> flip List.member choiceUuids)
                        |> List.map (\choice -> ( faKmChoice, choice.label ))
            in
            eventView choices

        IntegrationReply replyType ->
            case replyType of
                PlainType reply ->
                    eventView [ ( fa "far fa-edit", reply ) ]

                IntegrationType reply _ ->
                    eventView [ ( fa "fas fa-link", Markdown.toString reply ) ]

                IntegrationLegacyType _ reply ->
                    eventView [ ( fa "fas fa-link", Markdown.toString reply ) ]

        ItemSelectReply itemUuid ->
            let
                itemLabel =
                    QuestionnaireQuestionnaire.getItemSelectQuestionValueLabel appState cfg.questionnaire (Question.getUuid question) itemUuid
            in
            eventView [ ( fa "far fa-square-caret-down", itemLabel ) ]

        FileReply fileUuid ->
            case QuestionnaireQuestionnaire.getFile cfg.questionnaire fileUuid of
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
    div [ class "event-detail" ]
        [ em [] [ text (gettext "Cleared reply of" appState.locale), br [] [], linkToQuestion cfg question data.path ] ]


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
    div [ class "event-detail" ]
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


filterDayEvents : List QuestionnaireVersion -> List QuestionnaireEvent -> List QuestionnaireEvent
filterDayEvents versions events =
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


isVersion : List QuestionnaireVersion -> QuestionnaireEvent -> Bool
isVersion questionnaireVersions event =
    List.any (.eventUuid >> (==) (QuestionnaireEvent.getUuid event)) questionnaireVersions
