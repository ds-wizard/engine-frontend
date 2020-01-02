module Wizard.KMEditor.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Locale exposing (lr)
import Url.Parser exposing (..)
import Url.Parser.Query as Query
import Wizard.Auth.Permission as Perm exposing (hasPerm)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.JwtToken exposing (JwtToken)
import Wizard.KMEditor.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "kmEditor" appState
    in
    [ map (wrapRoute << CreateRoute) (s moduleRoot </> s (lr "kmEditor.create" appState) <?> Query.string (lr "kmEditor.create.selected" appState))
    , map (wrapRoute << EditorRoute) (s moduleRoot </> s (lr "kmEditor.edit" appState) </> string)
    , map (wrapRoute <| IndexRoute) (s moduleRoot)
    , map (wrapRoute << MigrationRoute) (s moduleRoot </> s (lr "kmEditor.migration" appState) </> string)
    , map (wrapRoute << PublishRoute) (s moduleRoot </> s (lr "kmEditor.publish" appState) </> string)
    ]


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "kmEditor" appState
    in
    case route of
        CreateRoute selected ->
            case selected of
                Just id ->
                    [ moduleRoot, lr "kmEditor.create" appState, "?" ++ lr "kmEditor.create.selected" appState ++ "=" ++ id ]

                Nothing ->
                    [ moduleRoot, lr "kmEditor.create" appState ]

        EditorRoute uuid ->
            [ moduleRoot, lr "kmEditor.edit" appState, uuid ]

        IndexRoute ->
            [ moduleRoot ]

        MigrationRoute uuid ->
            [ moduleRoot, lr "kmEditor.migration" appState, uuid ]

        PublishRoute uuid ->
            [ moduleRoot, lr "kmEditor.publish" appState, uuid ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        CreateRoute _ ->
            hasPerm maybeJwt Perm.knowledgeModel

        EditorRoute _ ->
            hasPerm maybeJwt Perm.knowledgeModel

        IndexRoute ->
            hasPerm maybeJwt Perm.knowledgeModel

        MigrationRoute _ ->
            hasPerm maybeJwt Perm.knowledgeModelUpgrade

        PublishRoute _ ->
            hasPerm maybeJwt Perm.knowledgeModelPublish
