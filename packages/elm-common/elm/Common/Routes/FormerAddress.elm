module Common.Routes.FormerAddress exposing (toCurrent)

{-| Addresses the client used to have, still in bookmarks and in emails sent
before they changed: every page was under `/wizard/…`, and the editors were
under paths of their own. An address like that is turned into the one the page
has now; the client replaces it before parsing anything.

    /wizard/projects/<uuid>                          -> /projects/<uuid>
    /wizard/knowledge-model-editors/editor/<uuid>/…  -> /knowledge-models/editors/<uuid>/…
    /wizard/knowledge-model-editors/migration/<uuid> -> /knowledge-models/editors/<uuid>/migration
    /wizard/knowledge-model-editors/publish/<uuid>   -> /knowledge-models/editors/<uuid>/publish
    /wizard/knowledge-model-editors[/…]              -> /knowledge-models/editors[/…]
    /wizard/document-template-editors[/…]            -> /document-templates/editors[/…]

The query and the fragment are kept. A page that is gone or has moved
elsewhere is not followed: its address, without the prefix, is not found.

-}

import Url exposing (Url)


{-| The current address of a former one; `Nothing` for an address that is
current already.
-}
toCurrent : Url -> Maybe Url
toCurrent url =
    case String.split "/" (String.dropLeft 1 url.path) of
        "wizard" :: rest ->
            Just { url | path = "/" ++ String.join "/" (currentSegments rest) }

        _ ->
            Nothing


currentSegments : List String -> List String
currentSegments segments =
    case segments of
        "knowledge-model-editors" :: "editor" :: uuid :: rest ->
            "knowledge-models" :: "editors" :: uuid :: rest

        [ "knowledge-model-editors", "migration", uuid ] ->
            [ "knowledge-models", "editors", uuid, "migration" ]

        [ "knowledge-model-editors", "publish", uuid ] ->
            [ "knowledge-models", "editors", uuid, "publish" ]

        "knowledge-model-editors" :: rest ->
            "knowledge-models" :: "editors" :: rest

        "document-template-editors" :: rest ->
            "document-templates" :: "editors" :: rest

        _ ->
            segments
