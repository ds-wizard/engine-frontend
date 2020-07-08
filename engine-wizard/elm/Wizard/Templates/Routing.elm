module Wizard.Templates.Routing exposing
    ( detail
    , isAllowed
    , parsers
    , toUrl
    )

import Shared.Auth.Permission as Perm
import Shared.Auth.Session exposing (Session)
import Shared.Locale exposing (lr)
import Url.Parser exposing (..)
import Url.Parser.Query as Query
import Wizard.Common.AppState exposing (AppState)
import Wizard.Templates.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "templates" appState
    in
    [ map (wrapRoute << ImportRoute) (s moduleRoot </> s (lr "templates.import" appState) <?> Query.string (lr "templates.import.templateId" appState))
    , map (detail wrapRoute) (s moduleRoot </> string)
    , map (wrapRoute <| IndexRoute) (s moduleRoot)
    ]


detail : (Route -> a) -> String -> a
detail wrapRoute packageId =
    DetailRoute packageId |> wrapRoute


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "templates" appState
    in
    case route of
        DetailRoute packageId ->
            [ moduleRoot, packageId ]

        ImportRoute packageId ->
            case packageId of
                Just id ->
                    [ moduleRoot, lr "templates.import" appState, "?" ++ lr "templates.import.templateId" appState ++ "=" ++ id ]

                Nothing ->
                    [ moduleRoot, lr "templates.import" appState ]

        IndexRoute ->
            [ moduleRoot ]


isAllowed : Route -> Session -> Bool
isAllowed route session =
    Perm.hasPerm session Perm.templates
