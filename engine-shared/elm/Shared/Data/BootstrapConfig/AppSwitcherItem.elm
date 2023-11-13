module Shared.Data.BootstrapConfig.AppSwitcherItem exposing
    ( AppSwitcherItem
    , AppSwitcherItemIcon(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias AppSwitcherItem =
    { title : String
    , description : String
    , url : String
    , icon : AppSwitcherItemIcon
    , external : Bool
    }


type AppSwitcherItemIcon
    = ImageAppSwitcherItemIcon String
    | FontAwesomeAppSwitcherItemIcon String


decoder : Decoder AppSwitcherItem
decoder =
    D.succeed AppSwitcherItem
        |> D.required "title" D.string
        |> D.required "description" D.string
        |> D.required "url" D.string
        |> D.required "icon" appSwitcherItemIconDecoder
        |> D.required "external" D.bool


appSwitcherItemIconDecoder : Decoder AppSwitcherItemIcon
appSwitcherItemIconDecoder =
    D.string
        |> D.map
            (\iconString ->
                if String.startsWith "http" iconString then
                    ImageAppSwitcherItemIcon iconString

                else
                    FontAwesomeAppSwitcherItemIcon iconString
            )
