module Wizard.Locales.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lr)
import Url.Parser exposing ((</>), (<?>), Parser, map, s, string)
import Url.Parser.Query as Query
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Locales.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    if Admin.isEnabled appState.config.admin then
        []

    else
        let
            moduleRoot =
                lr "locales" appState
        in
        [ map (wrapRoute <| CreateRoute) (s moduleRoot </> s (lr "locales.create" appState))
        , map (wrapRoute << ImportRoute) (s moduleRoot </> s (lr "locales.import" appState) <?> Query.string (lr "locales.import.localeId" appState))
        , map (detail wrapRoute) (s moduleRoot </> string)
        , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "name")) (PaginationQueryString.parser (s moduleRoot))
        ]


detail : (Route -> a) -> String -> a
detail wrapRoute localeId =
    DetailRoute localeId |> wrapRoute


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "locales" appState
    in
    case route of
        CreateRoute ->
            [ moduleRoot, lr "locales.create" appState ]

        DetailRoute localeId ->
            [ moduleRoot, localeId ]

        ImportRoute mbLocaleId ->
            case mbLocaleId of
                Just localeId ->
                    [ moduleRoot, lr "locales.import" appState, "?" ++ lr "locales.import.localeId" appState ++ "=" ++ localeId ]

                Nothing ->
                    [ moduleRoot, lr "locales.import" appState ]

        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]


isAllowed : Route -> AppState -> Bool
isAllowed route appState =
    case route of
        CreateRoute ->
            Feature.localeCreate appState

        ImportRoute _ ->
            Feature.localeImport appState

        DetailRoute _ ->
            Feature.localeView appState

        IndexRoute _ ->
            Feature.localeView appState
