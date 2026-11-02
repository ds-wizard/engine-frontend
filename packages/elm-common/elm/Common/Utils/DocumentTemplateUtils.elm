module Common.Utils.DocumentTemplateUtils exposing (getId, getIdWithoutVersion)

import Version exposing (Version)


getId : { a | id : String, version : Version } -> String
getId template =
    template.id ++ ":" ++ Version.toString template.version


{-| The template's identity without the version: what the template is, rather
than which release of it. Use it where the version is shown on its own already,
so it isn't said twice.
-}
getIdWithoutVersion : { a | id : String } -> String
getIdWithoutVersion template =
    template.id
