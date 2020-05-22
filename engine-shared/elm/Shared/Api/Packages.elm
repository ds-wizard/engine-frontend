module Shared.Api.Packages exposing (getPackage)

import Shared.Api exposing (AppStateLike, ToMsg, jwtGet)
import Shared.Data.PackageDetail as PackageDetail exposing (PackageDetail)


getPackage : String -> AppStateLike a -> ToMsg PackageDetail msg -> Cmd msg
getPackage packageId =
    jwtGet ("/packages/" ++ packageId) PackageDetail.decoder
