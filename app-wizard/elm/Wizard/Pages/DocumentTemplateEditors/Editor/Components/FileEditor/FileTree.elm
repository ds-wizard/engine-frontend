module Wizard.Pages.DocumentTemplateEditors.Editor.Components.FileEditor.FileTree exposing
    ( AssetData
    , FileData
    , FileTree(..)
    , FolderData
    , addAsset
    , addFile
    , addFolder
    , compare
    , getName
    , getPath
    , root
    )

import List.Extra as List
import Uuid exposing (Uuid)


type FileTree
    = Folder FolderData
    | File FileData
    | Asset AssetData


type alias FolderData =
    { name : String
    , isRoot : Bool
    , children : List FileTree
    , path : String
    }


type alias FileData =
    { name : String
    , path : String
    , uuid : Uuid
    }


type alias AssetData =
    { name : String
    , path : String
    , uuid : Uuid
    }


getName : FileTree -> String
getName fileTree =
    case fileTree of
        Folder folderData ->
            folderData.name

        File fileData ->
            fileData.name

        Asset assetData ->
            assetData.name


root : String -> FileTree
root name =
    new name "" True


getPath : (String -> a) -> (String -> a) -> (String -> a) -> FileTree -> a
getPath toFolder toFile toAsset fileTree =
    case fileTree of
        Folder folderData ->
            toFolder folderData.path

        File fileData ->
            toFile fileData.path

        Asset assetData ->
            toAsset assetData.path


compare : FileTree -> FileTree -> Order
compare tree1 tree2 =
    case ( tree1, tree2 ) of
        ( Folder data1, Folder data2 ) ->
            Basics.compare data1.name data2.name

        ( Folder _, _ ) ->
            LT

        ( _, Folder _ ) ->
            GT

        ( t1, t2 ) ->
            Basics.compare (getName t1) (getName t2)


new : String -> String -> Bool -> FileTree
new name path isRoot =
    Folder
        { name = name
        , isRoot = isRoot
        , children = []
        , path = path
        }


newAsset : Uuid -> String -> String -> FileTree
newAsset uuid name path =
    Asset
        { uuid = uuid
        , name = name
        , path = path
        }


newFile : Uuid -> String -> String -> FileTree
newFile uuid name path =
    File
        { uuid = uuid
        , name = name
        , path = path
        }


newFolder : String -> String -> FileTree
newFolder name path =
    Folder
        { name = name
        , isRoot = False
        , children = []
        , path = path
        }


addAsset : ( Uuid, String ) -> FileTree -> FileTree
addAsset ( uuid, path ) =
    addRecursive (newAsset uuid) [] (String.split "/" path)


addFile : ( Uuid, String ) -> FileTree -> FileTree
addFile ( uuid, path ) =
    addRecursive (newFile uuid) [] (String.split "/" path)


addFolder : String -> FileTree -> FileTree
addFolder path =
    addRecursive newFolder [] (String.split "/" path)


addRecursive : (String -> String -> FileTree) -> List String -> List String -> FileTree -> FileTree
addRecursive constructor currentPath parts fileTree =
    case fileTree of
        Folder folderData ->
            let
                pathToString =
                    String.join "/"
            in
            case parts of
                [] ->
                    fileTree

                file :: [] ->
                    let
                        newPath =
                            pathToString (currentPath ++ [ file ])
                    in
                    -- Check if a folder with the same path already exists
                    if List.any (\child -> getPath identity (always "") (always "") child == newPath) folderData.children then
                        fileTree

                    else
                        Folder { folderData | children = folderData.children ++ [ constructor file newPath ] }

                folder :: rest ->
                    let
                        newCurrentPath =
                            currentPath ++ [ folder ]
                    in
                    case getFolder folder folderData.children of
                        Just subfolderFileTree ->
                            Folder { folderData | children = updateFolder folder (addRecursive constructor newCurrentPath rest subfolderFileTree) folderData.children }

                        Nothing ->
                            let
                                path =
                                    pathToString newCurrentPath
                            in
                            Folder { folderData | children = folderData.children ++ [ addRecursive constructor newCurrentPath rest (new folder path False) ] }

        _ ->
            fileTree


getFolder : String -> List FileTree -> Maybe FileTree
getFolder name trees =
    let
        predicate fileTree =
            case fileTree of
                Folder folderData ->
                    name == folderData.name

                _ ->
                    False
    in
    List.find predicate trees


updateFolder : String -> FileTree -> List FileTree -> List FileTree
updateFolder name folder fileTrees =
    let
        map fileTree =
            case fileTree of
                Folder folderData ->
                    if folderData.name == name then
                        folder

                    else
                        fileTree

                _ ->
                    fileTree
    in
    List.map map fileTrees
