module Common.Pages.Public.Components.Consent exposing
    ( field
    , formField
    , isRequired
    )

{-| "I have read the Privacy Policy and agree to the Terms of Service." — the
checkbox a new account agrees with, on the sign-up form and on the consent step
of an external login.

Only the documents the instance links to are named, each in a sentence of its
own, so a translation can say each case as its language needs; there is no
checkbox at all when it links to neither. The names are links inside the
sentence, each opening in a new tab and saying so. Following one does not tick
the box: a click on a link inside a label goes to the link.

-}

import Common.Components.Form.Field as FormField
import Common.Data.AppState exposing (AppState)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Field as Field
import Ui.Link as Link


type Documents
    = Both String String
    | PrivacyOnly String
    | TermsOnly String


documents : AppState -> Maybe Documents
documents appState =
    case ( appState.config.privacy.privacyUrl, appState.config.privacy.termsOfServiceUrl ) of
        ( Just privacyUrl, Just termsUrl ) ->
            Just (Both privacyUrl termsUrl)

        ( Just privacyUrl, Nothing ) ->
            Just (PrivacyOnly privacyUrl)

        ( Nothing, Just termsUrl ) ->
            Just (TermsOnly termsUrl)

        ( Nothing, Nothing ) ->
            Nothing


{-| Whether the instance asks for consent at all.
-}
isRequired : AppState -> Bool
isRequired appState =
    Maybe.isJust (documents appState)


{-| The consent checkbox of an elm-form form, at `path`, with its message
when it is left unticked.
-}
formField : AppState -> Form FormError o -> String -> Html Form.Msg
formField appState form path =
    let
        state =
            Form.getFieldAsBool path form
    in
    field appState
        { id = FormField.fieldId path
        , checked = Maybe.withDefault False state.value
        , onCheck = Form.Input path Form.Checkbox << Form.Field.Bool
        , invalid = Maybe.isJust state.liveError
        , onFocus = Just (Form.Focus path)
        , onBlur = Just (Form.Blur path)
        }


field :
    AppState
    ->
        { id : String
        , checked : Bool
        , onCheck : Bool -> msg
        , invalid : Bool
        , onFocus : Maybe msg
        , onBlur : Maybe msg
        }
    -> Html msg
field appState config =
    case documents appState of
        Nothing ->
            Html.text ""

        Just docs ->
            let
                privacyName =
                    gettext "Privacy Policy" appState.locale

                termsName =
                    gettext "Terms of Service" appState.locale

                viewLink name url =
                    Link.link { label = name, url = url }
                        |> Link.external
                        |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                        |> Link.toHtml

                ( sentence, links, errorText ) =
                    case docs of
                        Both privacyUrl termsUrl ->
                            ( ( gettext "I have read the %s and agree to the %s." appState.locale, [ privacyName, termsName ] )
                            , [ viewLink privacyName privacyUrl, viewLink termsName termsUrl ]
                            , gettext "You have to read the Privacy Policy and agree to the Terms of Service first." appState.locale
                            )

                        PrivacyOnly privacyUrl ->
                            ( ( gettext "I have read the %s." appState.locale, [ privacyName ] )
                            , [ viewLink privacyName privacyUrl ]
                            , gettext "You have to read the Privacy Policy first." appState.locale
                            )

                        TermsOnly termsUrl ->
                            ( ( gettext "I agree to the %s." appState.locale, [ termsName ] )
                            , [ viewLink termsName termsUrl ]
                            , gettext "You have to agree to the Terms of Service first." appState.locale
                            )

                withOptional builder mbValue fieldConfig =
                    case mbValue of
                        Just value ->
                            builder value fieldConfig

                        Nothing ->
                            fieldConfig
            in
            Field.checkbox
                { id = config.id
                , label = String.format (Tuple.first sentence) (Tuple.second sentence)
                , checked = config.checked
                , onCheck = config.onCheck
                }
                |> Field.withLabelHtml (String.formatHtml (Tuple.first sentence) links)
                |> Field.withError
                    (if config.invalid then
                        Just errorText

                     else
                        Nothing
                    )
                |> withOptional Field.withOnFocus config.onFocus
                |> withOptional Field.withOnBlur config.onBlur
                |> Field.toHtml
