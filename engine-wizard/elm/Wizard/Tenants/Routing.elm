module Wizard.Tenants.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Dict
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Utils exposing (dictFromMaybeList)
import Url.Parser exposing ((</>), Parser, map, s)
import Url.Parser.Extra exposing (uuid)
import Url.Parser.Query as Query
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Tenants.Routes exposing (Route(..), indexRouteEnabledFilterId)


moduleRoot : String
moduleRoot =
    "tenants"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    let
        wrappedIndexRoute pqs q =
            wrapRoute <| IndexRoute pqs q
    in
    [ map (PaginationQueryString.wrapRoute1 wrappedIndexRoute (Just "name")) (PaginationQueryString.parser1 (s moduleRoot) (Query.string indexRouteEnabledFilterId))
    , map (wrapRoute <| CreateRoute) (s moduleRoot </> s "create")
    , map (wrapRoute << DetailRoute) (s moduleRoot </> uuid)
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        IndexRoute paginationQueryString mbEnabled ->
            let
                params =
                    Dict.toList <| dictFromMaybeList [ ( indexRouteEnabledFilterId, mbEnabled ) ]
            in
            [ moduleRoot ++ PaginationQueryString.toUrlWith params paginationQueryString ]

        CreateRoute ->
            [ moduleRoot, "create" ]

        DetailRoute uuid ->
            [ moduleRoot, Uuid.toString uuid ]


isAllowed : Route -> AppState -> Bool
isAllowed _ =
    Feature.tenants
