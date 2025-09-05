module Wizard.Pages.Locales.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Url.Parser exposing ((</>), (<?>), Parser, map, s, string)
import Url.Parser.Query as Query
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Locales.Routes exposing (Route(..))
import Wizard.Utils.Feature as Feature


moduleRoot : String
moduleRoot =
    "locales"


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    if Admin.isEnabled appState.config.admin then
        []

    else
        [ map (wrapRoute <| CreateRoute) (s moduleRoot </> s "create")
        , map (wrapRoute << ImportRoute) (s moduleRoot </> s "import" <?> Query.string "localeId")
        , map (detail wrapRoute) (s moduleRoot </> string)
        , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "name")) (PaginationQueryString.parser (s moduleRoot))
        ]


detail : (Route -> a) -> String -> a
detail wrapRoute localeId =
    DetailRoute localeId |> wrapRoute


toUrl : Route -> List String
toUrl route =
    case route of
        CreateRoute ->
            [ moduleRoot, "create" ]

        DetailRoute localeId ->
            [ moduleRoot, localeId ]

        ImportRoute mbLocaleId ->
            case mbLocaleId of
                Just localeId ->
                    [ moduleRoot, "import", "?" ++ "localeId" ++ "=" ++ localeId ]

                Nothing ->
                    [ moduleRoot, "import" ]

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
