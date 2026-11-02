module Common.Components.UserAvatarTest exposing (suite)

import Common.Components.UserAvatar as UserAvatar
import Expect
import Html
import Html.Attributes as Attr
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, tag, text)
import Ui.ItemIcon as ItemIcon


ada : { imageUrl : Maybe String, gravatarHash : Maybe String } -> Query.Single msg
ada config =
    UserAvatar.user
        { firstName = "Ada"
        , lastName = "Lovelace"
        , imageUrl = config.imageUrl
        , gravatarHash = config.gravatarHash
        }
        |> ItemIcon.toHtml
        |> List.singleton
        |> Html.div []
        |> Query.fromHtml


{-| One rule for every user's picture: the uploaded one whatever it is, else
the Gravatar laid over the initials, asked for transparent where there is none,
else the initials.
-}
suite : Test
suite =
    describe "Common.Components.UserAvatar.user"
        [ test "an uploaded picture is the picture, with nothing under it" <|
            \_ ->
                ada { imageUrl = Just "https://example.com/ada.png", gravatarHash = Just "abc" }
                    |> Expect.all
                        [ Query.find [ tag "img" ] >> Query.has [ attribute (Attr.src "https://example.com/ada.png") ]
                        , Query.hasNot [ class "ui-item-icon__initials" ]
                        , Query.hasNot [ class "ui-item-icon__over" ]
                        ]
        , test "a Gravatar goes over the initials, transparent where there is none" <|
            \_ ->
                let
                    over =
                        ada { imageUrl = Nothing, gravatarHash = Just "abc" }
                            |> Query.find [ class "ui-item-icon__over" ]
                in
                Expect.all
                    [ \_ -> ada { imageUrl = Nothing, gravatarHash = Just "abc" } |> Query.find [ class "ui-item-icon__initials" ] |> Query.has [ text "AL" ]
                    , \_ -> over |> Query.has [ attribute (Attr.src "//www.gravatar.com/avatar/abc?d=blank") ]
                    ]
                    ()
        , test "without a hash, the initials alone" <|
            \_ ->
                ada { imageUrl = Nothing, gravatarHash = Just "" }
                    |> Expect.all
                        [ Query.find [ class "ui-item-icon__initials" ] >> Query.has [ text "AL" ]
                        , Query.hasNot [ tag "img" ]
                        ]
        ]
