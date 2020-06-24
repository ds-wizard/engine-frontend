module Shared.Data.BootstrapConfig.DashboardConfig exposing
    ( DashboardConfig
    , decoder
    , default
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.BootstrapConfig.DashboardConfig.DashboardWidget as DashboardWidget exposing (DashboardWidget)


type alias DashboardConfig =
    { widgets : Maybe (Dict String (List DashboardWidget))
    , welcomeInfo : Maybe String
    , welcomeWarning : Maybe String
    }


default : DashboardConfig
default =
    { widgets = Nothing
    , welcomeInfo = Nothing
    , welcomeWarning = Nothing
    }



-- JSON


decoder : Decoder DashboardConfig
decoder =
    D.succeed DashboardConfig
        |> D.required "widgets" (D.maybe DashboardWidget.dictDecoder)
        |> D.required "welcomeInfo" (D.maybe D.string)
        |> D.required "welcomeWarning" (D.maybe D.string)


encode : DashboardConfig -> E.Value
encode config =
    E.object
        [ ( "widgets", E.maybe (E.dict identity (E.list DashboardWidget.encode)) config.widgets )
        , ( "welcomeInfo", E.maybe E.string config.welcomeInfo )
        , ( "welcomeWarning", E.maybe E.string config.welcomeWarning )
        ]
