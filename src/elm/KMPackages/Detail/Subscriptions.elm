module KMPackages.Detail.Subscriptions exposing (..)

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import KMPackages.Detail.Models exposing (Model, PackageDetailRow)
import KMPackages.Detail.Msgs exposing (Msg(DropdownMsg))
import Msgs


subscriptions : (Msg -> Msgs.Msg) -> Model -> Sub Msgs.Msg
subscriptions wrapMsg model =
    case model.packages of
        Success rows ->
            Sub.batch <| List.map (rowSubscriptions wrapMsg) rows

        _ ->
            Sub.none


rowSubscriptions : (Msg -> Msgs.Msg) -> PackageDetailRow -> Sub Msgs.Msg
rowSubscriptions wrapMsg row =
    Dropdown.subscriptions row.dropdownState (wrapMsg << DropdownMsg row.packageDetail)
