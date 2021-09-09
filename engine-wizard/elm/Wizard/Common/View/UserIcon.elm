module Wizard.Common.View.UserIcon exposing
    ( view
    , viewSmall
    )

import Html exposing (Html, div, img)
import Html.Attributes exposing (class, src)
import Shared.Data.User as User exposing (User)


view : User -> Html msg
view user =
    div [ class "ItemIcon" ]
        [ img [ src (User.imageUrl user) ] [] ]


viewSmall : { a | gravatarHash : String, imageUrl : Maybe String } -> Html msg
viewSmall user =
    div [ class "ItemIcon ItemIcon--small" ]
        [ img [ src (User.imageUrlOrGravatar user) ] [] ]
