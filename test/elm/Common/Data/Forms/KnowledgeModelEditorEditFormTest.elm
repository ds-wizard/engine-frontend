module Common.Data.Forms.KnowledgeModelEditorEditFormTest exposing (suite)

{-| What a rename from the title sends: the editor's settings as they are,
with the new name, whatever the settings form would say of them.
-}

import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState exposing (KnowledgeModelEditorState(..))
import Common.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Common.Data.Forms.KnowledgeModelEditorEditForm as KnowledgeModelEditorEditForm
import Dict
import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)
import Uuid
import Version


suite : Test
suite =
    describe "KnowledgeModelEditorEditForm.encodeEditor"
        [ test "a new editor, with no description, license or readme, is sent as it is" <|
            \_ ->
                -- The settings form does not accept these empty; a rename of
                -- an editor just created was dropped because of it.
                KnowledgeModelEditorEditForm.encodeEditor { newEditor | name = "Renamed" }
                    |> D.decodeValue
                        (D.map7 (\name description id version language license readme -> [ name, description, id, version, language, license, readme ])
                            (D.field "name" D.string)
                            (D.field "description" D.string)
                            (D.field "id" D.string)
                            (D.field "version" D.string)
                            (D.field "language" D.string)
                            (D.field "license" D.string)
                            (D.field "readme" D.string)
                        )
                    |> Expect.equal (Ok [ "Renamed", "", "my-km", "1.2.3", "en", "", "" ])
        ]


newEditor : KnowledgeModelEditorDetail
newEditor =
    { uuid = Uuid.fromUuidString "11111111-1111-4111-8111-111111111111"
    , name = "My knowledge model"
    , description = ""
    , id = "my-km"
    , language = "en"
    , license = ""
    , readme = ""
    , version = Version.create 1 2 3
    , knowledgeModel = KnowledgeModel.empty
    , forkOfPackage = Nothing
    , previousPackage = Nothing
    , events = []
    , state = Edited
    , replies = Dict.empty
    , workspaceUuid = Nothing
    }
