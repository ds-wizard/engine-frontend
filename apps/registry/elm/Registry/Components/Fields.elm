module Registry.Components.Fields exposing
    ( Kind(..)
    , emailError
    , field
    , passwordError
    , repeatError
    , requiredError
    )

{-| The fields of the registry's forms, and what is wrong with what is in them.

A form is never refused by a disabled button: its errors are said beside the
fields once it has been submitted, and the submit stays where it is.

-}

import Common.Data.Context exposing (WithLocale)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Field as Field


type Kind
    = Text String
    | Email
    | NewPassword
    | CurrentPassword


field : { id : String, label : String, kind : Kind, value : String, onInput : String -> msg, error : Maybe String } -> Html msg
field config =
    let
        props =
            { id = config.id, label = config.label, value = config.value, onInput = config.onInput }

        ( base, autocomplete ) =
            case config.kind of
                Text auto ->
                    ( Field.input props, auto )

                Email ->
                    ( Field.input props |> Field.withInputMode "email", "email" )

                NewPassword ->
                    ( Field.password props, "new-password" )

                CurrentPassword ->
                    ( Field.password props, "current-password" )
    in
    base
        |> Field.withAutocomplete autocomplete
        |> Field.withRequired True
        |> Field.withError config.error
        |> Field.toHtml


requiredError : WithLocale a -> String -> Maybe String
requiredError appState value =
    if String.isEmpty (String.trim value) then
        Just (gettext "This field is required." appState.locale)

    else
        Nothing


emailError : WithLocale a -> String -> Maybe String
emailError appState value =
    case requiredError appState value of
        Just error ->
            Just error

        Nothing ->
            if String.contains "@" value then
                Nothing

            else
                Just (gettext "This is not a valid email." appState.locale)


passwordError : WithLocale a -> String -> Maybe String
passwordError appState value =
    case requiredError appState value of
        Just error ->
            Just error

        Nothing ->
            if String.length value < 8 then
                Just (gettext "The password has to be at least 8 characters long." appState.locale)

            else
                Nothing


repeatError : WithLocale a -> String -> String -> Maybe String
repeatError appState password repeated =
    if password /= repeated then
        Just (gettext "Passwords don't match!" appState.locale)

    else
        Nothing
