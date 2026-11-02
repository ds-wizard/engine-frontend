module Common.Pages.DocumentTemplateEditors.Editor.FilesTest exposing (suite)

{-| Which path is which, and where a file or a folder may be moved to.

A folder moved into itself is a folder that stops existing, and a move into the
folder something is already in is a move that does nothing. The dialog used to
offer both and let the server refuse them; these are the rules that keep them
off the list.

-}

import Common.Pages.DocumentTemplateEditors.Editor.Files as Files exposing (Selection(..))
import Expect
import Gettext
import Set
import Test exposing (Test, describe, test)
import Time
import Uuid


{-| A template with a folder inside a folder, and a file in each.
-}
config : Files.Config ()
config =
    { locale = Gettext.defaultLocale
    , name = "Template"
    , files =
        [ file "1" "default.html.j2"
        , file "2" "parts/header.j2"
        , file "3" "parts/deep/footer.j2"
        ]
    , assets = [ asset "4" "images/logo.png" ]
    , newFolders = [ "empty" ]
    , collapsed = Set.empty
    , selected = NoSelection
    , dirty = []
    , onOpen = always ()
    , onSelectRoot = ()
    , onSelectFolder = always ()
    , onToggleFolder = \_ _ -> ()
    }


{-| An `Entry` names a file by its uuid, not by its path, so the fixture has to
give each one a uuid the tests can select by.
-}
uuidOf : String -> Uuid.Uuid
uuidOf suffix =
    Uuid.fromUuidString ("00000000-0000-4000-8000-00000000000" ++ suffix)


file : String -> String -> { uuid : Uuid.Uuid, fileName : String }
file suffix fileName =
    { uuid = uuidOf suffix, fileName = fileName }


asset : String -> String -> { uuid : Uuid.Uuid, fileName : String, contentType : String, url : String, urlExpiration : Time.Posix }
asset suffix fileName =
    { uuid = uuidOf suffix
    , fileName = fileName
    , contentType = "image/png"
    , url = ""
    , urlExpiration = Time.millisToPosix 0
    }


targets : Selection -> List String
targets selection =
    Files.moveTargets config selection


suite : Test
suite =
    describe "Common.Pages.DocumentTemplateEditors.Editor.Files"
        [ describe "selectedPath and pathOf are not the same path"
            [ test "a file's own path is the whole of its name" <|
                \_ ->
                    Files.selectedPath config (SelectedEntry (Files.File (Uuid.toString (uuidOf "2"))))
                        |> Expect.equal "parts/header.j2"
            , test "while the path of the selection is the folder it is in — where a new file would go" <|
                \_ ->
                    Files.pathOf config (SelectedEntry (Files.File (Uuid.toString (uuidOf "2"))))
                        |> Expect.equal "parts"
            , test "a folder is its own path either way" <|
                \_ ->
                    ( Files.selectedPath config (SelectedFolder "parts/deep")
                    , Files.pathOf config (SelectedFolder "parts/deep")
                    )
                        |> Expect.equal ( "parts/deep", "parts/deep" )
            , test "and renaming a file keeps it in the folder it is in" <|
                \_ ->
                    -- What the editor does with the two: the new path is the
                    -- parent of the thing itself plus the name that was typed.
                    Files.parentOf (Files.selectedPath config (SelectedEntry (Files.File (Uuid.toString (uuidOf "3")))))
                        |> Expect.equal "parts/deep"
            ]
        , describe "the folders the client is holding"
            [ test "a folder with a file in it is one the server has" <|
                \_ ->
                    Files.hasContent config "parts"
                        |> Expect.equal True
            , test "while an empty one is the client's alone — there is nothing to ask the server to move" <|
                \_ ->
                    Files.hasContent config "empty"
                        |> Expect.equal False
            , test "renaming a folder renames what is under it" <|
                \_ ->
                    List.map (Files.renamePath "parts" "pieces") [ "parts", "parts/deep", "images" ]
                        |> Expect.equalLists [ "pieces", "pieces/deep", "images" ]
            , test "and a folder whose name merely starts the same is left alone" <|
                \_ ->
                    Files.renamePath "parts" "pieces" "partsx"
                        |> Expect.equal "partsx"
            , test "deleting a folder takes what is under it, and nothing beside it" <|
                \_ ->
                    List.filter (Files.isUnder "parts") [ "parts", "parts/deep", "partsx", "images" ]
                        |> Expect.equalLists [ "parts", "parts/deep" ]
            ]
        , describe "moveTargets"
            [ test "every folder is a target for something at the top" <|
                \_ ->
                    targets (SelectedEntry (Files.File (Uuid.toString (uuidOf "1"))))
                        |> List.sort
                        |> Expect.equalLists [ "empty", "images", "parts", "parts/deep" ]
            , test "the folder a file is already in is not offered" <|
                \_ ->
                    targets (SelectedEntry (Files.File (Uuid.toString (uuidOf "2"))))
                        |> List.member "parts"
                        |> Expect.equal False
            , test "a folder is not offered itself" <|
                \_ ->
                    targets (SelectedFolder "parts")
                        |> List.member "parts"
                        |> Expect.equal False
            , test "nor anything inside it" <|
                \_ ->
                    targets (SelectedFolder "parts")
                        |> List.member "parts/deep"
                        |> Expect.equal False
            , test "but a folder beside it is" <|
                \_ ->
                    targets (SelectedFolder "parts")
                        |> List.member "images"
                        |> Expect.equal True
            ]
        ]
