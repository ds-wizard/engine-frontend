module KMEditor.Routing exposing (Route(..), isAllowed, moduleRoot, parsers, toUrl)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Url.Parser exposing (..)
import Url.Parser.Query as Query


type Route
    = CreateRoute (Maybe String)
    | EditorRoute String
    | Editor2Route String
    | IndexRoute
    | MigrationRoute String
    | PublishRoute String
    | PreviewRoute String
    | TagEditorRoute String


moduleRoot : String
moduleRoot =
    "km-editor"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (wrapRoute << CreateRoute) (s moduleRoot </> s "create" <?> Query.string "selected")
    , map (wrapRoute << EditorRoute) (s moduleRoot </> s "edit" </> string)
    , map (wrapRoute << Editor2Route) (s moduleRoot </> s "edit2" </> string)
    , map (wrapRoute <| IndexRoute) (s moduleRoot)
    , map (wrapRoute << MigrationRoute) (s moduleRoot </> s "migration" </> string)
    , map (wrapRoute << PreviewRoute) (s moduleRoot </> s "preview" </> string)
    , map (wrapRoute << PublishRoute) (s moduleRoot </> s "publish" </> string)
    , map (wrapRoute << TagEditorRoute) (s moduleRoot </> s "edit-tags" </> string)
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        CreateRoute selected ->
            case selected of
                Just id ->
                    [ moduleRoot, "create", "?selected=" ++ id ]

                Nothing ->
                    [ moduleRoot, "create" ]

        EditorRoute uuid ->
            [ moduleRoot, "edit", uuid ]

        Editor2Route uuid ->
            [ moduleRoot, "edit2", uuid ]

        IndexRoute ->
            [ moduleRoot ]

        MigrationRoute uuid ->
            [ moduleRoot, "migration", uuid ]

        PreviewRoute uuid ->
            [ moduleRoot, "preview", uuid ]

        PublishRoute uuid ->
            [ moduleRoot, "publish", uuid ]

        TagEditorRoute uuid ->
            [ moduleRoot, "edit-tags", uuid ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        CreateRoute _ ->
            hasPerm maybeJwt Perm.knowledgeModel

        EditorRoute uuid ->
            hasPerm maybeJwt Perm.knowledgeModel

        Editor2Route uuid ->
            hasPerm maybeJwt Perm.knowledgeModel

        IndexRoute ->
            hasPerm maybeJwt Perm.knowledgeModel

        MigrationRoute uuid ->
            hasPerm maybeJwt Perm.knowledgeModelUpgrade

        PreviewRoute uuid ->
            hasPerm maybeJwt Perm.knowledgeModel

        PublishRoute uuid ->
            hasPerm maybeJwt Perm.knowledgeModelPublish

        TagEditorRoute uuid ->
            hasPerm maybeJwt Perm.knowledgeModel
