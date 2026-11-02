module Common.Pages.Dev.Routes exposing
    ( Route(..)
    , isAllowed
    , parsers
    , persistentCommandIndexRouteStateFilterId
    , toUrl
    )

import Common.Data.Feature as Feature exposing (AppStateLike)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Maybe.Extra as Maybe
import Url
import Url.Parser exposing ((</>), (<?>), Parser, map, s)
import Url.Parser.Extensions exposing (uuid)
import Url.Parser.Query as Query
import Uuid exposing (Uuid)


type Route
    = -- The section of operations shown; the first one without it.
      OperationsRoute (Maybe String)
    | PersistentCommandsDetail Uuid
    | PersistentCommandsIndex PaginationQueryString (Maybe String)


persistentCommandIndexRouteStateFilterId : String
persistentCommandIndexRouteStateFilterId =
    "state"


moduleRoot : String
moduleRoot =
    "dev"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    let
        wrappedPersistentCommandsIndexRoute pqs mbState =
            wrapRoute <| PersistentCommandsIndex pqs mbState

        persistentCommandsIndexRouteParser =
            PaginationQueryString.parser1 (s moduleRoot </> s "persistent-commands")
                (Query.string persistentCommandIndexRouteStateFilterId)
    in
    [ map (wrapRoute << OperationsRoute) (s moduleRoot </> s "operations" <?> Query.string "section")
    , map (wrapRoute << PersistentCommandsDetail) (s moduleRoot </> s "persistent-commands" </> uuid)
    , map (PaginationQueryString.wrapRoute1 wrappedPersistentCommandsIndexRoute (Just "createdAt,desc")) persistentCommandsIndexRouteParser
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        OperationsRoute mbSection ->
            [ moduleRoot
            , "operations" ++ Maybe.unwrap "" (\section -> "?section=" ++ Url.percentEncode section) mbSection
            ]

        PersistentCommandsDetail uuid ->
            [ moduleRoot, "persistent-commands", Uuid.toString uuid ]

        PersistentCommandsIndex paginationQueryString mbState ->
            let
                params =
                    PaginationQueryString.filterParams [ ( persistentCommandIndexRouteStateFilterId, mbState ) ]
            in
            [ moduleRoot, "persistent-commands" ++ PaginationQueryString.toUrlWith params paginationQueryString ]


isAllowed : AppStateLike a -> Bool
isAllowed =
    Feature.dev
