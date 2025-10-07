module Common.Utils.GuideLinks exposing
    ( GuideLinks
    , decoder
    , fromList
    , get
    , merge
    , wrap
    )

import Common.Api.ExternalLink as ExternalLink
import Common.Api.Request exposing (ServerInfo)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)


type GuideLinks
    = GuideLinks (Dict String String)


decoder : Decoder GuideLinks
decoder =
    D.dict D.string
        |> D.map GuideLinks


fromList : List ( String, String ) -> GuideLinks
fromList list =
    GuideLinks <| Dict.fromList list


merge : GuideLinks -> GuideLinks -> GuideLinks
merge (GuideLinks guideLinksA) (GuideLinks guideLinksB) =
    GuideLinks <|
        Dict.merge
            (\key a -> Dict.insert key a)
            (\key a _ -> Dict.insert key a)
            (\key b -> Dict.insert key b)
            guideLinksA
            guideLinksB
            Dict.empty


wrap : ServerInfo -> String -> String
wrap =
    ExternalLink.externalLinkUrl


get : String -> GuideLinks -> String
get key (GuideLinks guideLinks) =
    Dict.get key guideLinks
        |> Maybe.withDefault ""
