module Common.Api.Models.PasswordStrength exposing
    ( PasswordStrength(..)
    , decoder
    )

{-| The server's verdict on a password: five steps, of which only `Strong` is
accepted for a new one.
-}

import Json.Decode as D exposing (Decoder)


type PasswordStrength
    = Risky
    | Weak
    | Moderate
    | Safe
    | Strong


{-| The answer also has a `score`, the estimated number of guesses; only the
verdict is read, since the guesses can exceed what a JSON number holds exactly.
-}
decoder : Decoder PasswordStrength
decoder =
    D.field "strength" D.string
        |> D.andThen
            (\strength ->
                case strength of
                    "Risky" ->
                        D.succeed Risky

                    "Weak" ->
                        D.succeed Weak

                    "Moderate" ->
                        D.succeed Moderate

                    "Safe" ->
                        D.succeed Safe

                    "Strong" ->
                        D.succeed Strong

                    _ ->
                        D.fail ("Unknown password strength: " ++ strength)
            )
