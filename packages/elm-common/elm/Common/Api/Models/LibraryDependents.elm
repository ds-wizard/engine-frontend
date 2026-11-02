module Common.Api.Models.LibraryDependents exposing
    ( Hidden
    , LibraryDependents
    , NamedItem
    , PackageItem
    , decoder
    , hiddenTotal
    )

{-| What deleting a knowledge model package or a document template takes with
it, as far as the person asking may see. Both entities answer with the same
shape; the lists that do not apply to one of them are empty.

  - `knowledgeModelPackages` and `editors`: deleted with a knowledge model.
  - `projects`: deleted with a knowledge model; with a document template they
    stay and lose the template.
  - `documents`: deleted with a document template.
  - `hidden`: what the person may not see, as counts only.
  - `deleteAllowed`: the server deletes only when nothing is hidden.

-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias LibraryDependents =
    { knowledgeModelPackages : List PackageItem
    , editors : List NamedItem
    , projects : List NamedItem
    , documents : List NamedItem
    , hidden : Hidden
    , deleteAllowed : Bool
    }


type alias PackageItem =
    { uuid : Uuid
    , id : String
    , name : String
    , version : Version
    }


type alias NamedItem =
    { uuid : Uuid
    , name : String
    }


type alias Hidden =
    { knowledgeModelPackages : Int
    , editors : Int
    , projects : Int
    , documents : Int
    , workspaces : Int
    }


decoder : Decoder LibraryDependents
decoder =
    D.succeed LibraryDependents
        |> D.required "knowledgeModelPackages" (D.list packageItemDecoder)
        |> D.required "editors" (D.list namedItemDecoder)
        |> D.required "projects" (D.list namedItemDecoder)
        |> D.required "documents" (D.list namedItemDecoder)
        |> D.required "hidden" hiddenDecoder
        |> D.required "deleteAllowed" D.bool


packageItemDecoder : Decoder PackageItem
packageItemDecoder =
    D.succeed PackageItem
        |> D.required "uuid" Uuid.decoder
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "version" Version.decoder


namedItemDecoder : Decoder NamedItem
namedItemDecoder =
    D.succeed NamedItem
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string


hiddenDecoder : Decoder Hidden
hiddenDecoder =
    D.succeed Hidden
        |> D.required "knowledgeModelPackages" D.int
        |> D.required "editors" D.int
        |> D.required "projects" D.int
        |> D.required "documents" D.int
        |> D.required "workspaces" D.int


{-| How many affected resources the person may not see, the workspaces they are
in left out.
-}
hiddenTotal : Hidden -> Int
hiddenTotal hidden =
    hidden.knowledgeModelPackages + hidden.editors + hidden.projects + hidden.documents
