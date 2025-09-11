module Common.Data.Role exposing
    ( Role
    , admin
    , csvDecoder
    , dataSteward
    , decoder
    , encode
    , isAdmin
    , isDataSteward
    , options
    , researcher
    , switch
    , toReadableString
    , toString
    , validation
    )

import Csv.Decode as CsvDecode
import Form.Error as Error
import Form.Validate as V exposing (Validation)
import Gettext exposing (gettext)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Maybe.Extra as Maybe


type Role
    = Admin
    | DataSteward
    | Researcher


admin : Role
admin =
    Admin


dataSteward : Role
dataSteward =
    DataSteward


researcher : Role
researcher =
    Researcher


isAdmin : Role -> Bool
isAdmin =
    (==) Admin


isDataSteward : Role -> Bool
isDataSteward =
    (==) DataSteward


decoder : Decoder Role
decoder =
    D.string
        |> D.andThen
            (\str ->
                Maybe.unwrap
                    (D.fail <| "Unknown role " ++ str)
                    D.succeed
                    (fromString str)
            )


csvDecoder : CsvDecode.Decoder Role
csvDecoder =
    CsvDecode.string
        |> CsvDecode.andThen
            (\str ->
                Maybe.unwrap
                    (CsvDecode.fail <| "Unknown role " ++ str)
                    CsvDecode.succeed
                    (fromString str)
            )


encode : Role -> E.Value
encode =
    E.string << toString


validation : Validation e Role
validation =
    V.string
        |> V.andThen
            (\str ->
                Maybe.unwrap
                    (V.fail (Error.value Error.InvalidString))
                    V.succeed
                    (fromString str)
            )


toString : Role -> String
toString role =
    case role of
        Admin ->
            "admin"

        DataSteward ->
            "dataSteward"

        Researcher ->
            "researcher"


fromString : String -> Maybe Role
fromString str =
    case str of
        "admin" ->
            Just Admin

        "dataSteward" ->
            Just DataSteward

        "researcher" ->
            Just Researcher

        _ ->
            Nothing


options : { a | locale : Gettext.Locale } -> List ( String, String )
options appState =
    [ ( toString Researcher, researcherLocale appState.locale )
    , ( toString DataSteward, dataStewardLocale appState.locale )
    , ( toString Admin, adminLocale appState.locale )
    ]


toReadableString : { a | locale : Gettext.Locale } -> Role -> String
toReadableString appState role =
    case role of
        Admin ->
            adminLocale appState.locale

        DataSteward ->
            dataStewardLocale appState.locale

        Researcher ->
            researcherLocale appState.locale


switch : Role -> a -> a -> a -> a
switch role adminValue dataStewardValue researcherValue =
    case role of
        Admin ->
            adminValue

        DataSteward ->
            dataStewardValue

        Researcher ->
            researcherValue


adminLocale : Gettext.Locale -> String
adminLocale =
    gettext "Admin"


dataStewardLocale : Gettext.Locale -> String
dataStewardLocale =
    gettext "Data Steward"


researcherLocale : Gettext.Locale -> String
researcherLocale =
    gettext "Researcher"
