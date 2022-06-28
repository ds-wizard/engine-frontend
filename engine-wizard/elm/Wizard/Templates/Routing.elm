module Wizard.Templates.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lr)
import Url.Parser exposing ((</>), (<?>), Parser, map, s, string)
import Url.Parser.Query as Query
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Templates.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "templates" appState
    in
    [ map (wrapRoute << ImportRoute) (s moduleRoot </> s (lr "templates.import" appState) <?> Query.string (lr "templates.import.templateId" appState))
    , map (detail wrapRoute) (s moduleRoot </> string)
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "name")) (PaginationQueryString.parser (s moduleRoot))
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

        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]


isAllowed : Route -> AppState -> Bool
isAllowed route appState =
    case route of
        ImportRoute _ ->
            Feature.templatesImport appState

        _ ->
            Feature.templatesView appState
