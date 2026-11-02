module Common.Components.Questionnaire.ReplyString exposing
    ( answeredAt
    , answeredAtFromString
    , fromReply
    , isAnswered
    , toReply
    )

{-| A reply, and when it was given, as strings — the shape `Html.Lazy` can compare.

Lazy compares its arguments by reference, and a string by value. A reply read out
of the questionnaire's `Dict` after an unrelated change is equal but not the same
value, and a `Maybe Reply` built on each render never is; so a question node is
given its reply encoded as JSON, and decodes it inside the lazy function. The node
then re-renders exactly when its reply's text changes. The wizard does the same,
for the same reason.

-}

import Common.Api.Models.ProjectDetail.Reply as Reply exposing (Reply)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Common.Utils.TimeDistance as TimeDistance
import Common.Utils.TimeUtils as TimeUtils
import Gettext
import Json.Decode as D
import Json.Encode as E
import Time
import Time.Distance


fromReply : Maybe Reply -> String
fromReply mbReply =
    case mbReply of
        Just reply ->
            E.encode 0 (Reply.encode reply)

        Nothing ->
            ""


toReply : String -> Maybe Reply
toReply replyString =
    if String.isEmpty replyString then
        Nothing

    else
        Result.toMaybe (D.decodeString Reply.decoder replyString)


isAnswered : Maybe Reply -> Bool
isAnswered mbReply =
    case mbReply of
        Just reply ->
            not (ReplyValue.isEmpty reply.value)

        Nothing ->
            False


{-| When the reply was given, as one string: the readable date and the distance
from now ("5 minutes ago"), joined by `#`. `minute` is the current time already
rounded down to the minute, so the string — and the node that shows it — changes
at most once a minute rather than on every render.
-}
answeredAt : { locale : Gettext.Locale, timeZone : Time.Zone, minute : Time.Posix } -> Maybe Reply -> String
answeredAt config mbReply =
    case mbReply of
        Just reply ->
            TimeUtils.toReadableDateTime config.timeZone reply.createdAt
                ++ "#"
                ++ Time.Distance.inWordsWithConfig { withAffix = True } (TimeDistance.locale config.locale) reply.createdAt config.minute

        Nothing ->
            ""


answeredAtFromString : String -> Maybe { readable : String, distance : String }
answeredAtFromString value =
    case String.split "#" value of
        [ readable, distance ] ->
            Just { readable = readable, distance = distance }

        _ ->
            Nothing
