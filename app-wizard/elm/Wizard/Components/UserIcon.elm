module Wizard.Components.UserIcon exposing
    ( view
    , viewSmall
    , viewUser
    )

import Html exposing (Html, div, img)
import Html.Attributes exposing (class, src)
import Wizard.Api.Models.User as User exposing (User)


view : { a | gravatarHash : String, imageUrl : Maybe String } -> Html msg
view user =
    view_ "ItemIcon" (User.imageUrlOrGravatar user)


viewUser : User -> Html msg
viewUser user =
    view_ "ItemIcon" (User.imageUrl user)


viewSmall : { a | gravatarHash : String, imageUrl : Maybe String } -> Html msg
viewSmall user =
    view_ "ItemIcon ItemIcon--small" (User.imageUrlOrGravatar user)


view_ : String -> String -> Html msg
view_ cssClass imgSrc =
    div [ class cssClass ]
        [ img [ src imgSrc ] [] ]
