module Common.Pages.UserGroups.Routes exposing
    ( DetailTab(..)
    , Route(..)
    , indexRouteMembershipFilterId
    , isAllowed
    , isSameListing
    , parsers
    , toUrl
    )

{-| The user groups of a workspace (a group belongs to one workspace),
at `/user-groups`, wherever the application puts a workspace's pages.
-}

import Common.Data.Feature as Feature exposing (AppStateLike)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Url.Parser exposing ((</>), Parser, map, s)
import Url.Parser.Extensions as Parser
import Url.Parser.Query as Query
import Uuid exposing (Uuid)


type Route
    = Index PaginationQueryString (Maybe String)
    | Create
    | Detail Uuid DetailTab


type DetailTab
    = Info
    | Members


{-| `member=true` lists the groups the reader is in, `false` the others.
-}
indexRouteMembershipFilterId : String
indexRouteMembershipFilterId =
    "member"


moduleRoot : String
moduleRoot =
    "user-groups"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    let
        wrappedIndexRoute pqs mbMember =
            wrapRoute (Index pqs mbMember)
    in
    [ map (wrapRoute Create) (s moduleRoot </> s "create")
    , map (\uuid -> wrapRoute (Detail uuid Members)) (s moduleRoot </> Parser.uuid </> s "members")
    , map (\uuid -> wrapRoute (Detail uuid Info)) (s moduleRoot </> Parser.uuid)
    , map (PaginationQueryString.wrapRoute1 wrappedIndexRoute (Just "name")) (PaginationQueryString.parser1 (s moduleRoot) (Query.string indexRouteMembershipFilterId))
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        Index pqs mbMember ->
            [ moduleRoot, PaginationQueryString.toUrlWith (PaginationQueryString.filterParams [ ( indexRouteMembershipFilterId, mbMember ) ]) pqs ]

        Create ->
            [ moduleRoot, "create" ]

        Detail uuid Info ->
            [ moduleRoot, Uuid.toString uuid ]

        Detail uuid Members ->
            [ moduleRoot, Uuid.toString uuid, "members" ]


isAllowed : AppStateLike a -> Route -> Bool
isAllowed appState route =
    case route of
        Create ->
            Feature.userGroupsCreate appState

        _ ->
            Feature.userGroupsView appState


{-| Another page or filter of the list keeps the page and its focus.
-}
isSameListing : Route -> Route -> Bool
isSameListing a b =
    case ( a, b ) of
        ( Index _ _, Index _ _ ) ->
            True

        _ ->
            False
