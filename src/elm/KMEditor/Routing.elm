module KMEditor.Routing exposing (Route(..), isAllowed, moduleRoot, parsers, toUrl)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Url.Parser exposing (..)
import Url.Parser.Query as Query


type Route
    = CreateRoute (Maybe String)
    | EditorRoute String
    | TagEditorRoute String
    | IndexRoute
    | MigrationRoute String
    | PublishRoute String


moduleRoot : String
moduleRoot =
    "km-editor"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (wrapRoute << CreateRoute) (s moduleRoot </> s "create" <?> Query.string "selected")
    , map (wrapRoute << EditorRoute) (s moduleRoot </> s "edit" </> string)
    , map (wrapRoute << TagEditorRoute) (s moduleRoot </> s "edit-tags" </> string)
    , map (wrapRoute <| IndexRoute) (s moduleRoot)
    , map (wrapRoute << MigrationRoute) (s moduleRoot </> s "migration" </> string)
    , map (wrapRoute << PublishRoute) (s moduleRoot </> s "publish" </> string)
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

        TagEditorRoute uuid ->
            [ moduleRoot, "edit-tags", uuid ]

        IndexRoute ->
            [ moduleRoot ]

        MigrationRoute uuid ->
            [ moduleRoot, "migration", uuid ]

        PublishRoute uuid ->
            [ moduleRoot, "publish", uuid ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        CreateRoute _ ->
            hasPerm maybeJwt Perm.knowledgeModel

        EditorRoute uuid ->
            hasPerm maybeJwt Perm.knowledgeModel

        TagEditorRoute uuid ->
            hasPerm maybeJwt Perm.knowledgeModel

        IndexRoute ->
            hasPerm maybeJwt Perm.knowledgeModel

        MigrationRoute uuid ->
            hasPerm maybeJwt Perm.knowledgeModelUpgrade

        PublishRoute uuid ->
            hasPerm maybeJwt Perm.knowledgeModelPublish
