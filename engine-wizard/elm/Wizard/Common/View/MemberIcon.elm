module Wizard.Common.View.MemberIcon exposing
    ( ViewCustomConfig
    , view
    , viewCustom
    , viewIconOnly
    )

import Html exposing (Html)
import Html.Attributes exposing (class, title)
import Wizard.Api.Models.Member as Member exposing (Member)
import Wizard.Common.View.ItemIcon as ItemIcon


view : Member -> Html msg
view member =
    ItemIcon.viewExtra
        { text = Member.visibleName member
        , image = Member.imageUrl member
        , attributes = [ class "ItemIcon--Member ItemIcon--Member--Small" ]
        }


viewIconOnly : Member -> Html msg
viewIconOnly member =
    ItemIcon.viewExtra
        { text = Member.visibleName member
        , image = Member.imageUrl member
        , attributes =
            [ class "ItemIcon--Member ItemIcon--Member--Small"
            , title (Member.visibleName member)
            ]
        }


type alias ViewCustomConfig =
    { text : String
    , image : Maybe String
    }


viewCustom : ViewCustomConfig -> Html msg
viewCustom cfg =
    ItemIcon.viewExtra
        { text = cfg.text
        , image = cfg.image
        , attributes =
            [ class "ItemIcon--Member ItemIcon--Member--Small"
            ]
        }
