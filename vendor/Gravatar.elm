module Gravatar exposing
    ( img, url
    , Options, defaultOptions, Default(..), Rating(..), withSize, withRating, withDefault, forceDefault
    , hashEmail, urlFromHash
    )

{-| Returns URL or img DOM element for a given `email`,
uses `options` as query parameters with URL.
More about query parameters read at [Gravatar](https://en.gravatar.com/site/implement/images/)
website.


# Gravatar image

@docs img, url


# Options

@docs Options, defaultOptions, Default, Rating, withSize, withRating, withDefault, forceDefault

-}

import Html exposing (Html)
import Html.Attributes exposing (src)
import MD5 exposing (hex)


{-| By default, if there is no (allowed) image associated with an emailaddress,
gravatar will display the gravatar logo. However, there are other fallback
images, too. For more explanation about these, please see [the gravatar docs][default-image].

[default-image]: https://en.gravatar.com/site/implement/images/#default-image

-}
type Default
    = None
    | Url String
    | FourOhFour
    | MysteryMan
    | Identicon
    | MonsterID
    | Wavatar
    | Retro
    | Blank


{-| Gravatar allows adding "rating" meta-data to its avatars. By default, you
will only get G-rated images. For more information, please see [the gravatar docs][rating].

[rating]: https://en.gravatar.com/site/implement/images/#rating

-}
type Rating
    = RatedG
    | RatedPG
    | RatedR
    | RatedX


{-| Allows specifying all the options you could possibly need for gravatar urls.

[Full overview of what the options do](https://en.gravatar.com/site/implement/images/).

-}
type alias Options =
    { size : Maybe Int
    , default : Default
    , rating : Rating
    , forceDefault : Bool
    }


{-| Default options. Passing these results in passing no options in the final
url at all.
-}
defaultOptions : Options
defaultOptions =
    { size = Nothing
    , default = None
    , rating = RatedG
    , forceDefault = False
    }


{-| Sets the size to the passed in value.

    defaultOptions |> withSize (Just 80)
    --> { size = Just 80, ... }

-}
withSize : Maybe Int -> Options -> Options
withSize size options =
    { options | size = size }


{-| Sets the default to the passed in value.

    defaultOptions |> withDefault Retro
    --> { default = Retro, ... }

-}
withDefault : Default -> Options -> Options
withDefault default options =
    { options | default = default }


{-| Sets the rating to the passed in value.

    defaultOptions |> withRating RatedPG
    --> { rating = RatedPG, ... }

-}
withRating : Rating -> Options -> Options
withRating rating options =
    { options | rating = rating }


{-| Enables the `forceDefault` flag.

    defaultOptions |> forceDefault
    --> { forceDefault = True, ... }

-}
forceDefault : Options -> Options
forceDefault options =
    { options | forceDefault = True }


encodeSize : Maybe Int -> Maybe String
encodeSize size =
    case size of
        Just 200 ->
            Nothing

        Just v ->
            Just <| String.fromInt v

        Nothing ->
            Nothing


encodeDefault : Default -> Maybe String
encodeDefault default =
    case default of
        None ->
            Nothing

        Url url_ ->
            Just url_

        FourOhFour ->
            Just "404"

        MysteryMan ->
            Just "mm"

        Identicon ->
            Just "identicon"

        MonsterID ->
            Just "monsterid"

        Wavatar ->
            Just "wavatar"

        Retro ->
            Just "retro"

        Blank ->
            Just "blank"


encodeRating : Rating -> Maybe String
encodeRating rating =
    case rating of
        RatedG ->
            Nothing

        RatedPG ->
            Just "pg"

        RatedR ->
            Just "r"

        RatedX ->
            Just "x"


encodeForceDefault : Bool -> Maybe String
encodeForceDefault toForceDefault =
    if toForceDefault then
        Just "y"

    else
        Nothing


encodeOption : String -> Maybe String -> Maybe String
encodeOption key val =
    Maybe.map (\v -> key ++ "=" ++ v) val


encodeOptions : Options -> String
encodeOptions options =
    let
        optionList : List String
        optionList =
            [ options.size |> encodeSize |> encodeOption "s"
            , options.default |> encodeDefault |> encodeOption "d"
            , options.rating |> encodeRating |> encodeOption "r"
            , options.forceDefault |> encodeForceDefault |> encodeOption "f"
            ]
                |> List.filterMap identity
    in
    case optionList of
        [] ->
            ""

        _ ->
            "?" ++ String.join "&" optionList


{-| Returns img DOM element which points to Gravatar for a given `email`
and using options as query parameters with URL

    img defaultOptions "kuzzmi@example.com"
    -- returns image node with 200px x 200px image
    -- for "kuzzmi@example.com" email.

-}
img : Options -> String -> Html msg
img options email =
    Html.img [ src <| url options email ] []


{-| Returns URL which points to Gravatar for a given `email`
and using options as query parameters with URL

    url defaultOptions "kuzzmi@example.com"
    -- returns url to 200px x 200px image
    -- for "kuzzmi@example.com" email.

    let
        options =
            defaultOptions
                |> withDefault Retro
                |> forceDefault
    in
    url options "kuzzme@example.com"
    -- url to 200 x 200 image in the 'retro' format

-}
url : Options -> String -> String
url options email =
    "//www.gravatar.com/avatar/"
        ++ hashEmail email
        ++ encodeOptions options


urlFromHash : Options -> String -> String
urlFromHash options hash =
    "//www.gravatar.com/avatar/"
        ++ hash
        ++ encodeOptions options


hashEmail : String -> String
hashEmail email =
    -- https://en.gravatar.com/site/implement/hash/
    email
        |> String.trim
        |> String.toLower
        |> MD5.hex
