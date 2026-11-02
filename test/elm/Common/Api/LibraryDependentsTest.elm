module Common.Api.LibraryDependentsTest exposing (libraryDependentsTests)

import Common.Api.Models.LibraryDependents as LibraryDependents
import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)


knowledgeModelJson : String
knowledgeModelJson =
    """
    { "knowledgeModelPackages": [ { "uuid": "22222222-2222-4222-8222-222222222222", "id": "org.derived", "name": "Derived", "version": "1.0.0" } ]
    , "editors": [ { "uuid": "66666666-6666-4666-8666-666666666661", "name": "Editor" } ]
    , "projects": []
    , "documents": []
    , "hidden": { "knowledgeModelPackages": 1, "editors": 0, "projects": 418, "documents": 0, "workspaces": 31 }
    , "deleteAllowed": false
    }
    """


libraryDependentsTests : Test
libraryDependentsTests =
    describe "LibraryDependents"
        [ test "decodes the preview with the hidden counts" <|
            \_ ->
                D.decodeString LibraryDependents.decoder knowledgeModelJson
                    |> Result.map (\d -> ( List.map .id d.knowledgeModelPackages, d.hidden.workspaces, d.deleteAllowed ))
                    |> Expect.equal (Ok ( [ "org.derived" ], 31, False ))
        , test "the hidden total leaves the workspaces out" <|
            \_ ->
                D.decodeString LibraryDependents.decoder knowledgeModelJson
                    |> Result.map (.hidden >> LibraryDependents.hiddenTotal)
                    |> Expect.equal (Ok 419)
        , test "a missing list is an error, not an empty one" <|
            \_ ->
                D.decodeString LibraryDependents.decoder "{ \"editors\": [], \"projects\": [], \"documents\": [], \"hidden\": {}, \"deleteAllowed\": true }"
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]
