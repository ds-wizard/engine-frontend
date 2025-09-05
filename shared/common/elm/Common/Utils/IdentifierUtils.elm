module Common.Utils.IdentifierUtils exposing
    ( getComponents
    , getOrganizationAndItemId
    )


getComponents : String -> Maybe ( String, String, String )
getComponents packageId =
    case String.split ":" packageId of
        orgId :: kmId :: version :: [] ->
            Just ( orgId, kmId, version )

        _ ->
            Nothing


getOrganizationAndItemId : String -> String
getOrganizationAndItemId fullId =
    case String.split ":" fullId of
        organizationId :: itemId :: _ ->
            organizationId ++ ":" ++ itemId

        _ ->
            ""
