module Common.Pages.DocumentTemplateEditors.Editor.Files exposing
    ( Config
    , Entry(..)
    , Selection(..)
    , hasContent
    , iconOf
    , isUnder
    , language
    , moveTargets
    , moveTree
    , nameOf
    , nothingOpen
    , parentOf
    , pathOf
    , renamePath
    , selectedPath
    , tree
    , view
    )

{-| The Files tab: the template's files down the start, whatever is open in the
middle.

**Folders are a fiction.** The server has no folder: a file is
`layouts/default.html.j2`, and the folders exist because the name says so. The
tree is built fresh from the flat list of names on every render, which is also
why a folder that holds nothing cannot be saved.

**A file's language comes from its extension.** The extension names a content
type (`Common.Utils.ContentType`) and the content type names a language the
code editor knows. That second map lives only in `packages/elm-ui/js/code-editor.js`;
the same five entries are here, because the Elm side has to choose the language
before the element is built.

**Dirty is said, not starred.** The wizard appends `" *"` to a changed file's
name, which a screen reader reads as "star" if it reads it at all. Here a
changed file carries a dot and the word, in the tree and on its tab.

-}

import Common.Api.Models.DocumentTemplate.DocumentTemplateAsset exposing (DocumentTemplateAsset)
import Common.Api.Models.DocumentTemplate.DocumentTemplateFile exposing (DocumentTemplateFile)
import Common.Utils.ContentType as ContentType
import Common.Utils.FileIcon as FileIcon
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class, classList)
import List.Extra as List
import Set exposing (Set)
import Ui.Badge as Badge
import Ui.Icon as Icon exposing (Icon)
import Ui.Illustration as Illustration
import Ui.Page.Message as Message
import Ui.Tree as Tree
import Uuid


{-| What can be open in the middle: a file, which is text and editable, or an
asset, which is not.
-}
type Entry
    = File String
    | Asset String


{-| What the toolbar acts on. Selecting and opening are two different things: a
folder can be selected without being opened, and opening a file selects it.
-}
type Selection
    = NoSelection
    | SelectedFolder String
    | SelectedEntry Entry


type alias Config msg =
    { locale : Gettext.Locale

    -- What the template is called, which is what the top of its tree is called.
    , name : String
    , files : List DocumentTemplateFile
    , assets : List DocumentTemplateAsset
    , dirty : List String
    , selected : Selection
    , collapsed : Set String

    -- Folders made here and not yet saved: the server has no folder, so an
    -- empty one lives only in the client until a file lands in it.
    , newFolders : List String
    , onOpen : Entry -> msg
    , onSelectRoot : msg
    , onSelectFolder : String -> msg
    , onToggleFolder : String -> Bool -> msg
    }



-- THE TREE


{-| The tree of what is in the template, built from the flat list of names.

The template itself is at the top, as it is in the move dialog: it is the
folder everything at the top is in, and selecting it is how something is added
there once a folder or a file has been selected. Its contents sit beside it
rather than inside it, because the top of a tree is not something to fold away.

-}
tree : Config msg -> Html msg
tree config =
    Tree.toHtml
        { label = gettext "Files" config.locale
        , words =
            { expand = \label -> gettext "Show what is in " config.locale ++ label
            , collapse = \label -> gettext "Hide what is in " config.locale ++ label
            }
        }
        ((Tree.node { key = "", label = config.name, onSelect = config.onSelectRoot }
            |> Tree.withIcon (Icon.solid "fa-folder-tree")
            |> Tree.withCurrent (config.selected == NoSelection)
            |> Tree.withRoot
            |> Tree.withTestId "dt-editor_file-tree_root"
         )
            :: nodesOf config "" (rootOf config)
        )


{-| The files and assets as a tree of names: every path split on `/`, folders
made from the parts before the last one.
-}
type Node
    = Folder (Dict String Node)
    | Leaf Entry


rootOf : Config msg -> Dict String Node
rootOf config =
    let
        add path entry nodes =
            case String.split "/" path of
                [] ->
                    nodes

                [ name ] ->
                    Dict.insert name (Leaf entry) nodes

                folder :: rest ->
                    let
                        inside =
                            case Dict.get folder nodes of
                                Just (Folder children) ->
                                    children

                                _ ->
                                    Dict.empty
                    in
                    Dict.insert folder (Folder (add (String.join "/" rest) entry inside)) nodes

        withFolders =
            List.foldl addFolder Dict.empty config.newFolders

        addFolder path nodes =
            case String.split "/" path of
                [] ->
                    nodes

                [ name ] ->
                    Dict.insert name (Folder (folderOf name nodes)) nodes

                folder :: rest ->
                    Dict.insert folder (Folder (addFolder (String.join "/" rest) (folderOf folder nodes))) nodes

        folderOf name nodes =
            case Dict.get name nodes of
                Just (Folder children) ->
                    children

                _ ->
                    Dict.empty
    in
    List.foldl (\file nodes -> add file.fileName (File (Uuid.toString file.uuid)) nodes)
        (List.foldl (\asset nodes -> add asset.fileName (Asset (Uuid.toString asset.uuid)) nodes) withFolders config.assets)
        config.files


nodesOf : Config msg -> String -> Dict String Node -> List (Tree.Node msg)
nodesOf config prefix nodes =
    let
        -- Folders first, then files, each alphabetically: the order the eye
        -- expects of a file tree, and the order the wizard uses.
        ( inner, leaves ) =
            List.partition
                (\( _, node ) ->
                    case node of
                        Folder _ ->
                            True

                        Leaf _ ->
                            False
                )
                (Dict.toList nodes)
    in
    List.map (nodeOf config prefix) (inner ++ leaves)


nodeOf : Config msg -> String -> ( String, Node ) -> Tree.Node msg
nodeOf config prefix ( name, node ) =
    let
        path =
            if String.isEmpty prefix then
                name

            else
                prefix ++ "/" ++ name
    in
    case node of
        Folder children ->
            let
                inside =
                    nodesOf config path children

                -- A folder made here and not filled in yet is empty, and a
                -- caret that opens onto nothing is a promise the tree cannot
                -- keep.
                branch =
                    if List.isEmpty inside then
                        identity

                    else
                        let
                            expanded =
                                not (Set.member path config.collapsed)
                        in
                        Tree.withChildren
                            { expanded = expanded, onToggle = config.onToggleFolder path (not expanded) }
                            inside
            in
            Tree.node { key = path, label = name, onSelect = config.onSelectFolder path }
                |> Tree.withIcon (Icon.solid "fa-folder")
                |> Tree.withCurrent (config.selected == SelectedFolder path)
                |> Tree.withTestId "dt-editor_file-tree_folder"
                |> branch

        Leaf entry ->
            Tree.node { key = path, label = name, onSelect = config.onOpen entry }
                |> Tree.withIcon (iconOf config entry name)
                |> Tree.withCurrent (config.selected == SelectedEntry entry)
                |> Tree.withEnd (dirtyMark config entry)
                |> Tree.withTestId (testIdOf entry)


{-| The icon a file wears in the tree.

A file the code editor knows a language for is code, whatever its extension
says it is — a template is `default.html.j2`, which no general map has heard of.
Everything else is what `Common.Utils.FileIcon` makes of its name and, for an
asset, of the content type the server gave it: a picture, a PDF, an archive, and
an empty sheet of paper for anything with nothing better to say about it.

-}
iconOf : Config msg -> Entry -> String -> Icon
iconOf config entry name =
    if String.isEmpty (language name) then
        Icon.fromClass (FileIcon.getFileIcon name (contentTypeOf config entry))

    else
        Icon.regular "fa-file-code"


{-| What the server says an asset is. A file is text and says nothing, so its
icon is chosen by its name alone.
-}
contentTypeOf : Config msg -> Entry -> String
contentTypeOf config entry =
    case entry of
        Asset uuid ->
            config.assets
                |> List.find (\asset -> Uuid.toString asset.uuid == uuid)
                |> Maybe.map .contentType
                |> Maybe.withDefault ""

        File _ ->
            ""


testIdOf : Entry -> String
testIdOf entry =
    case entry of
        File _ ->
            "dt-editor_file-tree_file"

        Asset _ ->
            "dt-editor_file-tree_asset"


{-| A file with unsaved changes says so, rather than wearing a star.
-}
dirtyMark : Config msg -> Entry -> List (Html msg)
dirtyMark config entry =
    case entry of
        File uuid ->
            if List.member uuid config.dirty then
                [ Badge.toHtml (Badge.warning (gettext "Unsaved" config.locale)) ]

            else
                []

        Asset _ ->
            []



-- WHAT IS OPEN


{-| What is open in the middle: one editor, two side by side, or, with nothing
open, a message saying where files come from.
-}
view : Config msg -> List (Html msg) -> Html msg
view config sides =
    div
        [ classList
            [ ( "c-dt-editor-files", True )
            , ( "c-dt-editor-files--split", List.length sides > 1 )
            ]
        ]
        (if List.isEmpty sides then
            [ nothingOpen config.locale ]

         else
            sides
        )


{-| A pane with no file in it, which says where files come from.
-}
nothingOpen : Gettext.Locale -> Html msg
nothingOpen locale =
    div [ class "c-dt-editor-files__empty" ]
        [ Message.message
            { title = gettext "No file open" locale
            , description = gettext "Open a file from the list to edit it." locale
            }
            |> Message.withIllustration (Illustration.toHtml Illustration.placeholder)
            |> Message.withinPage
            |> Message.toHtml
        ]


{-| The language the code editor highlights a file in.

The extension names a content type, and the content type names a language. The
second map is in `packages/elm-ui/js/code-editor.js` as well, where the languages are
loaded; anything not in it is plain text, which is what an unknown file should
look like rather than an error.

-}
language : String -> String
language fileName =
    case ContentType.getContentTypeText fileName of
        "application/xml" ->
            "xml"

        "text/css" ->
            "css"

        "text/html" ->
            "html"

        "text/html+jinja2" ->
            "jinja2"

        "text/x-python" ->
            "python"

        _ ->
            ""


{-| The name a path ends in, which is what a tab is called.
-}
nameOf : String -> String
nameOf path =
    Maybe.withDefault path (List.last (String.split "/" path))


{-| Every folder in the template, as paths, which `moveTargets` narrows to the
ones something may actually be moved into.
-}
folders : Config msg -> List String
folders config =
    let
        pathsOf path =
            case List.init (String.split "/" path) of
                Just parts ->
                    List.indexedMap (\index _ -> String.join "/" (List.take (index + 1) parts)) parts

                Nothing ->
                    []
    in
    (config.newFolders ++ List.concatMap (pathsOf << .fileName) config.files ++ List.concatMap (pathsOf << .fileName) config.assets)
        |> List.filterNot String.isEmpty
        |> List.unique
        |> List.sort


{-| The path of whatever is selected: a folder's own, or the folder a file is
in.
-}
pathOf : Config msg -> Selection -> String
pathOf config selection =
    case selection of
        SelectedFolder path ->
            path

        SelectedEntry entry ->
            parentOf (nameOfEntry config entry)

        NoSelection ->
            ""


{-| The path of the selected thing itself: a folder's own path, or the whole
name of a file or an asset.

Not the same as `pathOf`, which is the folder something is _in_ — where a new
file would go. This is the thing being renamed, moved or deleted, and telling
the two apart is the difference between renaming a file and renaming the folder
it happens to be in.

-}
selectedPath : Config msg -> Selection -> String
selectedPath config selection =
    case selection of
        SelectedFolder path ->
            path

        SelectedEntry entry ->
            nameOfEntry config entry

        NoSelection ->
            ""


{-| The folders something may be moved into: every folder except the one it is
already in, and — when a folder is being moved — itself and everything under it.

A folder moved into itself is a folder that stops existing, and a move into the
folder something is already in is a move that does nothing, so neither is
offered rather than accepted and then refused.

-}
moveTargets : Config msg -> Selection -> List String
moveTargets config selection =
    let
        here =
            pathOf config selection

        itself =
            case selection of
                SelectedFolder path ->
                    \folder -> folder == path || String.startsWith (path ++ "/") folder

                _ ->
                    always False
    in
    folders config
        |> List.filter (\folder -> folder /= here && not (itself folder))


{-| Where a move may go, as the tree it lives in.

A dropdown of paths asks somebody to read `layouts/partials/header` and picture
where that is; the tree is the same picture the file list already gives, so the
folder is recognized rather than parsed. Everything is in it, including the
folders a move may not go into — `Ui.Tree`'s disabled entries — because a folder
is found by the folder it is in, and a target with its surroundings taken away
is a target nobody can place.

The top of the template is the tree's root, and it is a target like any other
for something that is not already there.

-}
moveTree :
    Config msg
    ->
        { selection : Selection
        , target : Maybe String
        , expanded : Set String
        , onSelect : String -> msg
        , onToggle : String -> Bool -> msg
        }
    -> Html msg
moveTree config state =
    let
        allowed =
            Set.fromList (moveTargets config state.selection)

        rootAllowed =
            pathOf config state.selection /= ""
    in
    Tree.toHtml
        { label = gettext "Where it goes" config.locale
        , words =
            { expand = \label -> gettext "Show what is in " config.locale ++ label
            , collapse = \label -> gettext "Hide what is in " config.locale ++ label
            }
        }
        -- The template itself, and then its folders beside it rather than
        -- inside it: the top of a tree is not something to fold away, and a
        -- toggle drawn on it would be a control that does nothing. The same
        -- shape as the knowledge model editor's move.
        ((Tree.node
            { key = ""
            , label = config.name
            , onSelect = state.onSelect ""
            }
            |> Tree.withIcon (Icon.solid "fa-folder-tree")
            |> Tree.withCurrent (state.target == Just "")
            |> Tree.withDisabled (not rootAllowed)
            |> Tree.withRoot
            |> Tree.withTestId "dt-editor_move-modal_item"
         )
            :: moveNodes config state allowed ""
        )


{-| The folders directly inside `parent`, each with whatever is inside it.
-}
moveNodes : Config msg -> { a | target : Maybe String, expanded : Set String, onSelect : String -> msg, onToggle : String -> Bool -> msg } -> Set String -> String -> List (Tree.Node msg)
moveNodes config state allowed parent =
    let
        node path =
            let
                inside =
                    moveNodes config state allowed path

                branch =
                    if List.isEmpty inside then
                        identity

                    else
                        let
                            expanded =
                                Set.member path state.expanded
                        in
                        Tree.withChildren
                            { expanded = expanded, onToggle = state.onToggle path (not expanded) }
                            inside
            in
            Tree.node
                { key = path
                , label = nameOf path
                , onSelect = state.onSelect path
                }
                |> Tree.withIcon (Icon.solid "fa-folder")
                |> Tree.withCurrent (state.target == Just path)
                |> Tree.withDisabled (not (Set.member path allowed))
                |> Tree.withTestId "dt-editor_move-modal_item"
                |> branch
    in
    folders config
        |> List.filter (\folder -> parentOf folder == parent)
        |> List.map node


{-| Whether a path is that folder or something inside it.
-}
isUnder : String -> String -> Bool
isUnder folder path =
    path == folder || String.startsWith (folder ++ "/") path


{-| A path with `from` at the start of it replaced by `to`: what renaming or
moving a folder does to the folder itself and to everything under it.

A folder is a name and nothing else, so this is the whole of the operation for
the folders the client is holding — and `docsx` is not inside `docs`, which is
why the prefix is matched with the separator after it.

-}
renamePath : String -> String -> String -> String
renamePath from to path =
    if path == from then
        to

    else if String.startsWith (from ++ "/") path then
        to ++ String.dropLeft (String.length from) path

    else
        path


{-| Whether the server has anything in this folder.

The server has no folders: a folder exists because a file in it says so. One
made here with nothing in it lives only in `newFolders`, and asking the server
to move a folder it has never heard of is a request that can only fail.

-}
hasContent : Config msg -> String -> Bool
hasContent config folder =
    let
        inside name =
            String.startsWith (folder ++ "/") name
    in
    List.any (inside << .fileName) config.files || List.any (inside << .fileName) config.assets


{-| The folder a path is in, which is "" for something at the top.
-}
parentOf : String -> String
parentOf path =
    case List.init (String.split "/" path) of
        Just [] ->
            ""

        Just parts ->
            String.join "/" parts

        Nothing ->
            ""


nameOfEntry : Config msg -> Entry -> String
nameOfEntry config entry =
    case entry of
        File uuid ->
            config.files
                |> List.find (\file -> Uuid.toString file.uuid == uuid)
                |> Maybe.map .fileName
                |> Maybe.withDefault ""

        Asset uuid ->
            config.assets
                |> List.find (\asset -> Uuid.toString asset.uuid == uuid)
                |> Maybe.map .fileName
                |> Maybe.withDefault ""
