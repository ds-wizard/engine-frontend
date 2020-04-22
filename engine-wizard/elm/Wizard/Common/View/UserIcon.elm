module Wizard.Common.View.UserIcon exposing (..)

import Html exposing (Html, div, img)
import Html.Attributes exposing (class, src)
import Wizard.Users.Common.User as User exposing (User)


view : User -> Html msg
view user =
    div [ class "ItemIcon" ]
        [ img [ src (User.imageUrl user) ] [] ]
