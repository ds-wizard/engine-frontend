module Wizard.Api.Models.Project.ProjectCreation exposing
    ( ProjectCreation(..)
    , customEnabled
    , decoder
    , encode
    , field
    , fromTemplateEnabled
    , richFormOptions
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Gettext exposing (gettext)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type ProjectCreation
    = Custom
    | Template
    | TemplateAndCustom


toString : ProjectCreation -> String
toString projectCreation =
    case projectCreation of
        Custom ->
            "CustomProjectCreation"

        Template ->
            "TemplateProjectCreation"

        TemplateAndCustom ->
            "TemplateAndCustomProjectCreation"


fromString : String -> Maybe ProjectCreation
fromString str =
    case str of
        "CustomProjectCreation" ->
            Just Custom

        "TemplateProjectCreation" ->
            Just Template

        "TemplateAndCustomProjectCreation" ->
            Just TemplateAndCustom

        _ ->
            Nothing


encode : ProjectCreation -> E.Value
encode =
    E.string << toString


decoder : Decoder ProjectCreation
decoder =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just value ->
                        D.succeed value

                    Nothing ->
                        D.fail <| "Unknown project creation: " ++ str
            )


customEnabled : ProjectCreation -> Bool
customEnabled projectCreation =
    case projectCreation of
        Template ->
            False

        _ ->
            True


fromTemplateEnabled : ProjectCreation -> Bool
fromTemplateEnabled projectCreation =
    case projectCreation of
        Custom ->
            False

        _ ->
            True


field : ProjectCreation -> Field
field =
    toString >> Field.string


validation : Validation e ProjectCreation
validation =
    V.string
        |> V.andThen
            (\value ->
                case fromString value of
                    Just v ->
                        V.succeed v

                    Nothing ->
                        V.fail (Error.value InvalidString)
            )


richFormOptions : { a | locale : Gettext.Locale } -> List ( String, String, String )
richFormOptions appState =
    [ ( toString TemplateAndCustom
      , gettext "Templates & Custom" appState.locale
      , gettext "Projects can be created from project templates or with custom settings." appState.locale
      )
    , ( toString Template
      , gettext "Templates only" appState.locale
      , gettext "Only project templates can be used for creating new projects." appState.locale
      )
    , ( toString Custom
      , gettext "Custom only" appState.locale
      , gettext "Project templates feature is disabled, researchers have to choose knowledge model and set up everything." appState.locale
      )
    ]
