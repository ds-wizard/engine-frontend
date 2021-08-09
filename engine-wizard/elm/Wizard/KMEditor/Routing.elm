module Wizard.KMEditor.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Dict
import Shared.Auth.Permission as Perm
import Shared.Auth.Session exposing (Session)
import Shared.Data.PaginationQueryString as PaginationQueryString
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

        editQuery =
            Query.enum (lr "kmEditor.create.edit" appState) (Dict.fromList [ ( "true", True ), ( "false", False ) ])
    in
    [ map (createRoute wrapRoute) (s moduleRoot </> s (lr "kmEditor.create" appState) <?> Query.string (lr "kmEditor.create.selected" appState) <?> editQuery)
    , map (wrapRoute << EditorRoute) (s moduleRoot </> s (lr "kmEditor.edit" appState) </> uuid)
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "updatedAt,desc")) (PaginationQueryString.parser (s moduleRoot))
    , map (wrapRoute << MigrationRoute) (s moduleRoot </> s (lr "kmEditor.migration" appState) </> uuid)
    , map (wrapRoute << PublishRoute) (s moduleRoot </> s (lr "kmEditor.publish" appState) </> uuid)
    ]


createRoute : (Route -> a) -> Maybe String -> Maybe Bool -> a
createRoute wrapRoute kmId edit =
    wrapRoute <| CreateRoute kmId edit


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "kmEditor" appState
    in
    case route of
        CreateRoute mbSelected mbEdit ->
            case ( mbSelected, mbEdit ) of
                ( Just id, Just edit ) ->
                    let
                        editString =
                            if edit then
                                "true"

                            else
                                "false"
                    in
                    [ moduleRoot
                    , lr "kmEditor.create" appState
                    , "?" ++ lr "kmEditor.create.selected" appState ++ "=" ++ id ++ "&" ++ lr "kmEditor.create.edit" appState ++ "=" ++ editString
                    ]

                ( Just id, Nothing ) ->
                    [ moduleRoot
                    , lr "kmEditor.create" appState
                    , "?" ++ lr "kmEditor.create.selected" appState ++ "=" ++ id
                    ]

                _ ->
                    [ moduleRoot, lr "kmEditor.create" appState ]

        EditorRoute uuid ->
            [ moduleRoot, lr "kmEditor.edit" appState, Uuid.toString uuid ]

        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]

        MigrationRoute uuid ->
            [ moduleRoot, lr "kmEditor.migration" appState, Uuid.toString uuid ]

        PublishRoute uuid ->
            [ moduleRoot, lr "kmEditor.publish" appState, Uuid.toString uuid ]


isAllowed : Route -> Session -> Bool
isAllowed route session =
    case route of
        CreateRoute _ _ ->
            Perm.hasPerm session Perm.knowledgeModel

        EditorRoute _ ->
            Perm.hasPerm session Perm.knowledgeModel

        IndexRoute _ ->
            Perm.hasPerm session Perm.knowledgeModel

        MigrationRoute _ ->
            Perm.hasPerm session Perm.knowledgeModelUpgrade

        PublishRoute _ ->
            Perm.hasPerm session Perm.knowledgeModelPublish
