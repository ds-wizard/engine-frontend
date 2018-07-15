module KMEditor.Editor2.Models.Children exposing (..)

import List.Extra as List


type alias Children =
    { list : List String
    , dirty : Bool
    , deleted : List String
    }


init : List String -> Children
init children =
    { list = children
    , dirty = False
    , deleted = []
    }


addChild : String -> Children -> Children
addChild child children =
    { children | list = children.list ++ [ child ] }


deleteChild : String -> Children -> Children
deleteChild child children =
    removeChild child { children | deleted = children.deleted ++ [ child ] }


cleanDirty : Children -> Children
cleanDirty children =
    { children | dirty = False }


removeChild : String -> Children -> Children
removeChild child children =
    { children | list = List.remove child children.list }


updateList : List String -> Children -> Children
updateList newList children =
    { children | list = newList, dirty = True }
