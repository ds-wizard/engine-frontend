module Wizard.KMEditor.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Auth.Permission as Perm
import Shared.Auth.Session exposing (Session)
import Shared.Locale exposing (lr)
import Url.Parser exposing (..)
import Url.Parser.Extra exposing (uuid)
import Url.Parser.Query as Query
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "kmEditor" appState
    in
    [ map (wrapRoute << CreateRoute) (s moduleRoot </> s (lr "kmEditor.create" appState) <?> Query.string (lr "kmEditor.create.selected" appState))
    , map (wrapRoute << EditorRoute) (s moduleRoot </> s (lr "kmEditor.edit" appState) </> uuid)
    , map (wrapRoute <| IndexRoute) (s moduleRoot)
    , map (wrapRoute << MigrationRoute) (s moduleRoot </> s (lr "kmEditor.migration" appState) </> uuid)
    , map (wrapRoute << PublishRoute) (s moduleRoot </> s (lr "kmEditor.publish" appState) </> uuid)
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
            [ moduleRoot, lr "kmEditor.edit" appState, Uuid.toString uuid ]

        IndexRoute ->
            [ moduleRoot ]

        MigrationRoute uuid ->
            [ moduleRoot, lr "kmEditor.migration" appState, Uuid.toString uuid ]

        PublishRoute uuid ->
            [ moduleRoot, lr "kmEditor.publish" appState, Uuid.toString uuid ]


isAllowed : Route -> Session -> Bool
isAllowed route session =
    case route of
        CreateRoute _ ->
            Perm.hasPerm session Perm.knowledgeModel

        EditorRoute _ ->
            Perm.hasPerm session Perm.knowledgeModel

        IndexRoute ->
            Perm.hasPerm session Perm.knowledgeModel

        MigrationRoute _ ->
            Perm.hasPerm session Perm.knowledgeModelUpgrade

        PublishRoute _ ->
            Perm.hasPerm session Perm.knowledgeModelPublish
