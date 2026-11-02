port module Common.Ports.Captcha exposing
    ( initCaptcha
    , onCaptchaSuccess
    )

{-| A captcha the login asks for before it sends the password, when the client
turns it on (`Common.Data.Extensions.loginCaptcha`).

`initCaptcha` draws the challenge into the element the selector names;
`onCaptchaSuccess` brings back the token that proves it was solved, which goes
with the login request. A client that turns the captcha on answers these
ports; in any other, `loginCaptcha` stays off and they are never used.

-}


port initCaptcha : String -> Cmd msg


port onCaptchaSuccess : (String -> msg) -> Sub msg
