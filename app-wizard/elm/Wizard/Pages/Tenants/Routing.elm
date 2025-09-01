module Wizard.Pages.Tenants.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Url.Parser exposing ((</>), Parser, map, s)
import Url.Parser.Extra exposing (uuid)
import Url.Parser.Query as Query
import Uuid
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Tenants.Routes exposing (Route(..), indexRouteEnabledFilterId, indexRouteStatesFilterId)
import Wizard.Utils.Feature as Feature


moduleRoot : String
moduleRoot =
    "tenants"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    let
        wrappedIndexRoute pqs enabled states =
            wrapRoute <| IndexRoute pqs enabled states
    in
    [ map (PaginationQueryString.wrapRoute2 wrappedIndexRoute (Just "createdAt,desc")) (PaginationQueryString.parser2 (s moduleRoot) (Query.string indexRouteEnabledFilterId) (Query.string indexRouteStatesFilterId))
    , map (wrapRoute <| CreateRoute) (s moduleRoot </> s "create")
    , map (wrapRoute << DetailRoute) (s moduleRoot </> uuid)
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        IndexRoute paginationQueryString mbEnabled mbStates ->
            let
                params =
                    PaginationQueryString.filterParams
                        [ ( indexRouteEnabledFilterId, mbEnabled )
                        , ( indexRouteStatesFilterId, mbStates )
                        ]
            in
            [ moduleRoot ++ PaginationQueryString.toUrlWith params paginationQueryString ]

        CreateRoute ->
            [ moduleRoot, "create" ]

        DetailRoute uuid ->
            [ moduleRoot, Uuid.toString uuid ]


isAllowed : Route -> AppState -> Bool
isAllowed _ =
    Feature.tenants
