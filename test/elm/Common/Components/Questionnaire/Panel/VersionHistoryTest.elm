module Common.Components.Questionnaire.Panel.VersionHistoryTest exposing (collapseTypingTest)

import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue exposing (ReplyValue(..))
import Common.Api.Models.ProjectVersion exposing (ProjectVersion)
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Components.Questionnaire.Panel.VersionHistory as VersionHistory
import Expect
import Test exposing (Test, describe, test)
import Time
import Uuid exposing (Uuid)


uuid : Int -> Uuid
uuid n =
    Uuid.fromUuidString ("00000000-0000-4000-8000-" ++ String.padLeft 12 '0' (String.fromInt n))


user : Int -> UserSuggestion
user n =
    { uuid = uuid (100 + n), firstName = "User", lastName = String.fromInt n, gravatarHash = "", imageUrl = Nothing, affiliation = Nothing }


setReply : Int -> String -> Int -> ProjectEvent
setReply n path author =
    ProjectEvent.SetReply { uuid = uuid n, path = path, value = StringReply (String.fromInt n), createdAt = Time.millisToPosix n, createdBy = Just (user author) }


setPhase : Int -> ProjectEvent
setPhase n =
    ProjectEvent.SetPhase { uuid = uuid n, phaseUuid = Nothing, createdAt = Time.millisToPosix n, createdBy = Nothing }


version : Int -> ProjectVersion
version eventN =
    { uuid = uuid (200 + eventN), name = "v", description = Nothing, eventUuid = uuid eventN, createdAt = Time.millisToPosix 0, createdBy = Nothing }


uuids : List ProjectEvent -> List Uuid
uuids =
    List.map ProjectEvent.getUuid


collapseTypingTest : Test
collapseTypingTest =
    describe "VersionHistory.collapseTyping"
        [ test "keeps only the newest change of a question typed by one person" <|
            \_ ->
                VersionHistory.collapseTyping [] [ setReply 3 "q" 1, setReply 2 "q" 1, setReply 1 "q" 1 ]
                    |> uuids
                    |> Expect.equal [ uuid 3 ]
        , test "keeps a change made by someone else" <|
            \_ ->
                VersionHistory.collapseTyping [] [ setReply 3 "q" 1, setReply 2 "q" 2, setReply 1 "q" 2 ]
                    |> uuids
                    |> Expect.equal [ uuid 3, uuid 2 ]
        , test "keeps changes of other questions and phases" <|
            \_ ->
                VersionHistory.collapseTyping [] [ setReply 4 "a" 1, setPhase 3, setReply 2 "b" 1, setReply 1 "a" 1 ]
                    |> uuids
                    |> Expect.equal [ uuid 4, uuid 3, uuid 2 ]
        , test "keeps a named version" <|
            \_ ->
                VersionHistory.collapseTyping [ version 2 ] [ setReply 3 "q" 1, setReply 2 "q" 1, setReply 1 "q" 1 ]
                    |> uuids
                    |> Expect.equal [ uuid 3, uuid 2 ]
        ]
