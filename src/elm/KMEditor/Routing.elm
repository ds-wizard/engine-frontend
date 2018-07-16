module KMEditor.Routing exposing (..)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import UrlParser exposing (..)


type Route
    = Create (Maybe String)
    | Editor String
    | Index
    | Migration String
    | Publish String


moduleRoot : String
moduleRoot =
    "km-editor"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (wrapRoute << Create) (s moduleRoot </> s "create" <?> stringParam "selected")
    , map (wrapRoute << Editor) (s moduleRoot </> s "edit2" </> string)
    , map (wrapRoute <| Index) (s moduleRoot)
    , map (wrapRoute << Migration) (s moduleRoot </> s "migration" </> string)
    , map (wrapRoute << Publish) (s moduleRoot </> s "publish" </> string)
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        Create selected ->
            case selected of
                Just id ->
                    [ moduleRoot, "create", "?selected=" ++ id ]

                Nothing ->
                    [ moduleRoot, "create" ]

        Editor uuid ->
            [ moduleRoot, "edit2", uuid ]

        Index ->
            [ moduleRoot ]

        Migration uuid ->
            [ moduleRoot, "migration", uuid ]

        Publish uuid ->
            [ moduleRoot, "publish", uuid ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        Create _ ->
            hasPerm maybeJwt Perm.knowledgeModel

        Editor uuid ->
            hasPerm maybeJwt Perm.knowledgeModel

        Index ->
            hasPerm maybeJwt Perm.knowledgeModel

        Migration uuid ->
            hasPerm maybeJwt Perm.knowledgeModelUpgrade

        Publish uuid ->
            hasPerm maybeJwt Perm.knowledgeModelPublish
