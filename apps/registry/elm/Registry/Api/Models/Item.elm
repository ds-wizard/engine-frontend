module Registry.Api.Models.Item exposing
    ( Item
    , ItemDetail
    , Kind(..)
    , decoder
    , detailDecoder
    , reference
    )

{-| What the registry publishes. Knowledge models and document templates have
one shape (one `id` and a version, no organization); translations of
the user interface and of the mails are only a language and a version
(`Registry.Api.Models.Translation`).

`reference` is the `id:version` an item is addressed by, in the registry's
pages and in a wizard's import; it is never in what the server sends.

The detail has what each kind adds: a knowledge model's metamodel version as a
number and the knowledge model it was forked from, a template's metamodel
version as a version.

-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)


type Kind
    = KnowledgeModel
    | DocumentTemplate
    | Locale
    | MailTemplateLocale


type alias Item =
    { uuid : Uuid
    , id : String
    , name : String
    , version : Version
    , description : String
    , createdAt : Time.Posix
    }


type alias ItemDetail =
    { uuid : Uuid
    , id : String
    , name : String
    , version : Version
    , description : String
    , readme : String
    , license : String
    , versions : List Version
    , createdAt : Time.Posix
    , metamodelVersion : Maybe String
    , forkOf : Maybe { id : String, version : Version }
    }


reference : { a | id : String, version : Version } -> String
reference item =
    item.id ++ ":" ++ Version.toString item.version


decoder : Decoder Item
decoder =
    D.succeed Item
        |> D.required "uuid" Uuid.decoder
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "createdAt" D.datetime


detailDecoder : Kind -> Decoder ItemDetail
detailDecoder kind =
    let
        metamodelVersion =
            case kind of
                KnowledgeModel ->
                    D.map (Just << String.fromInt) (D.field "metamodelVersion" D.int)

                DocumentTemplate ->
                    D.map Just (D.field "metamodelVersion" D.string)

                _ ->
                    D.succeed Nothing

        forkOf =
            case kind of
                KnowledgeModel ->
                    D.map2 (Maybe.map2 (\id version -> { id = id, version = version }))
                        (D.field "forkOfPackageId" (D.nullable D.string))
                        (D.field "forkOfPackageVersion" (D.nullable Version.decoder))

                _ ->
                    D.succeed Nothing
    in
    D.succeed ItemDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "readme" D.string
        |> D.required "license" D.string
        |> D.required "versions" (D.list Version.decoder)
        |> D.required "createdAt" D.datetime
        |> D.custom metamodelVersion
        |> D.custom forkOf
