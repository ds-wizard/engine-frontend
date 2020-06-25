module Uuid exposing
    ( Uuid, uuidGenerator, fromString, toString, encode, decoder
    , fromUuidString, nil
    )

{-| This module provides an opaque type for Uuids, helpers to serialize
from and to String and helpers to generate new Uuids using Elm's
[Random](https://package.elm-lang.org/packages/elm/random/latest/) package.

Uuids are Universally Unique IDentifiers. They are 128 bit ids that are
designed to be extremely unlikely to collide with other Uuids.

This library only supports generating Version 4 Uuid (those generated using
random numbers, as opposed to hashing. See
[Wikipedia on Uuids](https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_.28random.29)
for more details). Version 4 Uuids are constructed using 122 pseudo random bits.

Disclaimer: If you use this Library to generate Uuids, please be advised
that it does not use a cryptographically secure pseudo random number generator.
Depending on your use case the randomness provided may not be enough. The
period of the underlying random generator is high, so creating lot's of random
UUIDs on one client is fine, but please be aware that since the initial random
seed of the current Random implementation is limited to 32 bits, creating
UUIDs on many independent clients may lead to collisions more quickly than you
think (see <https://github.com/danyx23/elm-uuid/issues/10> for details)!

This library is split into two Modules. Uuid (this module) wraps Uuids in
an opaque type for improved type safety. If you prefer to simply get strings
you can use the Uuid.Barebones module which provides methods to generate
and verify Uuid as plain Strings.

Uuids can be generated either by parsing them from the canonical string representation
(see fromString) or by generating them. If you are unfamiliar with random number generation
in pure functional languages, this can be a bit confusing. The gist of it is that:

1.  you need a good random seed and this has to come from outside our wonderfully
    predictable Elm code (meaning you have to create an incoming port and feed in
    some initial randomness)

2.  every call to generate a new Uuid will give you a tuple of a Uuid and a new
    seed. It is very important that whenever you generate a new Uuid you store this
    seed you get back into your model and use this one for the next Uuid generation.
    If you reuse a seed, you will create the same Uuid twice!

Have a look at the examples in the package to see how to use it!

@docs Uuid, uuidGenerator, fromString, toString, encode, decoder

-}

import Json.Decode as JD
import Json.Encode as JE
import Random exposing (Generator, Seed, int, list, map, step)
import String
import Uuid.Barebones exposing (..)


{-| Uuid type. Represents a 128 bit Uuid (Version 4)
-}
type Uuid
    = Uuid String


nil : Uuid
nil =
    Uuid "00000000-0000-0000-0000-000000000000"


fromUuidString : String -> Uuid
fromUuidString str =
    Uuid str


{-| Create a string representation from a Uuid in the canonical 8-4-4-4-12 form, i.e.
"63B9AAA2-6AAF-473E-B37E-22EB66E66B76"
-}
toString : Uuid -> String
toString (Uuid internalString) =
    internalString


{-| Create a Uuid from a String in the canonical form (e.g.
"63B9AAA2-6AAF-473E-B37E-22EB66E66B76"). Note that this module only supports
canonical Uuids, Versions 1-5 and will refuse to parse other Uuid variants.
-}
fromString : String -> Maybe Uuid
fromString text =
    if isValidUuid text then
        Just <| Uuid <| String.toLower text

    else
        Nothing


{-| Random Generator for Uuids. Using this Generator instead of the generate
function let's you use the full power of the Generator to create lists of Uuids,
map them to other types etc.
-}
uuidGenerator : Generator Uuid
uuidGenerator =
    map Uuid uuidStringGenerator


{-| Encode Uuid to Json
-}
encode : Uuid -> JE.Value
encode =
    toString
        >> JE.string


{-| Decoder for getting Uuid out of Json
-}
decoder : JD.Decoder Uuid
decoder =
    JD.string
        |> JD.andThen
            (\string ->
                case fromString string of
                    Just uuid ->
                        JD.succeed uuid

                    Nothing ->
                        JD.fail "Not a valid UUID"
            )
