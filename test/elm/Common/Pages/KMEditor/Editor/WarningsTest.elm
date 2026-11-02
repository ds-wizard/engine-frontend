module Common.Pages.KMEditor.Editor.WarningsTest exposing (suite)

{-| What each warning is about.

A warning names the entity it belongs to and, where it is about one field, that
field — which is what lets following a warning put the caret on the thing that
has to change rather than at the top of a form. The names here are the form's
own (`Entity.fieldId`), so a field renamed in the form and not here would send
the reader nowhere; that is what these tests are for.

A warning about the shape of an entity rather than one of its values — an
options question with no answers — is about the button that makes the first one,
because that is the only thing on the form that can be done about it.

-}

import Common.Api.Models.Event exposing (Event)
import Common.Api.Models.Event.AddQuestionEventData as AddQuestionEventData
import Common.Api.Models.Event.AddReferenceEventData as AddReferenceEventData
import Common.Api.Models.Event.EventContent exposing (EventContent(..))
import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState exposing (KnowledgeModelEditorState(..))
import Common.Pages.KMEditor.Editor.EditorContext as EditorContext
import Common.Pages.KMEditor.Editor.Warnings as Warnings
import Dict
import Expect
import Gettext
import List.Extra as List
import Test exposing (Test, describe, test)
import Time
import Uuid
import Version


uuidOf : String -> String
uuidOf suffix =
    "00000000-0000-4000-8000-00000000000" ++ suffix


emptyKm : KnowledgeModel
emptyKm =
    { uuid = Uuid.fromUuidString (uuidOf "0")
    , chapterUuids = []
    , tagUuids = []
    , integrationUuids = []
    , metricUuids = []
    , phaseUuids = []
    , resourceCollectionUuids = []
    , entities = KnowledgeModel.empty.entities
    , annotations = []
    }


event : String -> String -> EventContent -> Event
event parentUuid entityUuid content =
    { uuid = "event-" ++ entityUuid
    , parentUuid = parentUuid
    , entityUuid = entityUuid
    , content = content
    , createdAt = Time.millisToPosix 0
    }


{-| A chapter with four things wrong under it: an options question with no
answers, an expert whose address is not one, a reference with no URL and an
integration question with no integration chosen.
-}
events : List Event
events =
    [ event (uuidOf "0") (uuidOf "c") (AddChapterEvent { title = "Chapter", text = Nothing, annotations = [] })
    , event (uuidOf "c")
        (uuidOf "q")
        (AddQuestionEvent
            (AddQuestionEventData.AddQuestionOptionsEvent
                { title = "Question", text = Nothing, requiredPhaseUuid = Nothing, tagUuids = [], annotations = [] }
            )
        )
    , event (uuidOf "q") (uuidOf "e") (AddExpertEvent { name = "Expert", email = "not-an-address", annotations = [] })
    , event (uuidOf "q")
        (uuidOf "r")
        (AddReferenceEvent
            (AddReferenceEventData.AddReferenceURLEvent { url = "", label = "A reference", annotations = [] })
        )
    , event (uuidOf "c")
        (uuidOf "i")
        (AddQuestionEvent
            (AddQuestionEventData.AddQuestionIntegrationEvent
                { title = "Integration question", text = Nothing, requiredPhaseUuid = Nothing, tagUuids = [], integrationUuid = "", variables = Dict.empty, annotations = [] }
            )
        )
    , event (uuidOf "0") (uuidOf "m") (AddMetricEvent { title = "", abbreviation = Nothing, description = Nothing, annotations = [] })
    ]


warnings : List Warnings.Warning
warnings =
    let
        editorContext =
            EditorContext.init
                { uuid = Uuid.fromUuidString "11111111-1111-4111-8111-111111111111"
                , name = "Editor"
                , description = ""
                , id = "editor"
                , language = "en"
                , license = ""
                , readme = ""
                , version = Version.create 1 0 0
                , knowledgeModel = emptyKm
                , forkOfPackage = Nothing
                , previousPackage = Nothing
                , events = events
                , state = Edited
                , replies = Dict.empty
                , workspaceUuid = Nothing
                }
                Nothing
    in
    Warnings.compute
        { locale = Gettext.defaultLocale
        , builtIns = []
        , secrets = []
        , isDeleted = always False
        }
        (EditorContext.getFilteredKM editorContext)


{-| The field named by the warning about this entity.
-}
fieldFor : String -> Maybe (Maybe String)
fieldFor entityUuid =
    warnings
        |> List.find (\warning -> warning.editorUuid == entityUuid)
        |> Maybe.map .field


suite : Test
suite =
    describe "Common.Pages.KMEditor.Editor.Warnings"
        [ describe "what a warning is about"
            [ test "an options question with no answers is about the button that adds one" <|
                \_ -> Expect.equal (Just (Just "answers-add")) (fieldFor (uuidOf "q"))
            , test "an address that is not one is about the address" <|
                \_ -> Expect.equal (Just (Just "email")) (fieldFor (uuidOf "e"))
            , test "a reference with no URL is about the URL" <|
                \_ -> Expect.equal (Just (Just "url")) (fieldFor (uuidOf "r"))
            , test "an integration question with no integration is about the integration" <|
                \_ -> Expect.equal (Just (Just "integration")) (fieldFor (uuidOf "i"))
            , test "an unnamed metric is about its title" <|
                \_ -> Expect.equal (Just (Just "title")) (fieldFor (uuidOf "m"))
            ]
        , describe "and every one of them"
            [ test "belongs to an entity" <|
                \_ ->
                    warnings
                        |> List.filter (\warning -> String.isEmpty warning.editorUuid)
                        |> Expect.equalLists []
            , test "says something" <|
                \_ ->
                    warnings
                        |> List.filter (\warning -> String.isEmpty warning.message)
                        |> Expect.equalLists []
            , test "and there is one for each thing wrong, and no more" <|
                \_ -> Expect.equal 5 (List.length warnings)
            ]
        ]
