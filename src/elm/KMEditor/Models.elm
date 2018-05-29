module KMEditor.Models exposing (..)

import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (..)
import List.Extra as List
import Utils exposing (validateRegex)


type alias KnowledgeModel =
    { uuid : String
    , name : String
    , organizationId : String
    , kmId : String
    , parentPackageId : Maybe String
    , lastAppliedParentPackageId : Maybe String
    , stateType : KnowledgeModelState
    }


type KnowledgeModelState
    = Default
    | Edited
    | Outdated
    | Migrating
    | Migrated


type alias KnowledgeModelCreateForm =
    { name : String
    , kmId : String
    , parentPackageId : Maybe String
    }


type alias KnowledgeModelPublishForm =
    { major : Int
    , minor : Int
    , patch : Int
    , description : String
    }


type alias KnowledgeModelUpgradeForm =
    { targetPackageId : String
    }


knowledgeModelDecoder : Decoder KnowledgeModel
knowledgeModelDecoder =
    decode KnowledgeModel
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "organizationId" Decode.string
        |> required "kmId" Decode.string
        |> required "parentPackageId" (Decode.nullable Decode.string)
        |> required "lastAppliedParentPackageId" (Decode.nullable Decode.string)
        |> required "stateType" knowledgeModelStateDecoder


knowledgeModelStateDecoder : Decoder KnowledgeModelState
knowledgeModelStateDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Default" ->
                        Decode.succeed Default

                    "Edited" ->
                        Decode.succeed Edited

                    "Outdated" ->
                        Decode.succeed Outdated

                    "Migrating" ->
                        Decode.succeed Migrating

                    "Migrated" ->
                        Decode.succeed Migrated

                    unknownState ->
                        Decode.fail <| "Unknown knowledge model state " ++ unknownState
            )


knowledgeModelListDecoder : Decoder (List KnowledgeModel)
knowledgeModelListDecoder =
    Decode.list knowledgeModelDecoder


kmMatchState : List KnowledgeModelState -> KnowledgeModel -> Bool
kmMatchState states knowledgeModel =
    List.any ((==) knowledgeModel.stateType) states


kmLastVersion : KnowledgeModel -> Maybe String
kmLastVersion km =
    let
        getVersion parent =
            let
                parts =
                    String.split ":" parent

                samePackage =
                    List.getAt 1 parts
                        |> Maybe.map ((==) km.kmId)
                        |> Maybe.withDefault False

                sameOrganization =
                    List.getAt 0 parts
                        |> Maybe.map ((==) km.organizationId)
                        |> Maybe.withDefault False
            in
            if sameOrganization && samePackage then
                List.getAt 2 parts
            else
                Nothing
    in
    km.parentPackageId
        |> Maybe.andThen getVersion


initKnowledgeModelCreateForm : Maybe String -> Form CustomFormError KnowledgeModelCreateForm
initKnowledgeModelCreateForm selectedPackage =
    let
        initials =
            case selectedPackage of
                Just packageId ->
                    [ ( "parentPackageId", Field.string packageId ) ]

                _ ->
                    []
    in
    Form.initial initials knowledgeModelCreateFormValidation


knowledgeModelCreateFormValidation : Validation CustomFormError KnowledgeModelCreateForm
knowledgeModelCreateFormValidation =
    Validate.map3 KnowledgeModelCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "kmId" (validateRegex "^^(?![-])(?!.*[-]$)[a-zA-Z0-9-]+$"))
        (Validate.field "parentPackageId" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))


encodeKnowledgeModelForm : String -> KnowledgeModelCreateForm -> Encode.Value
encodeKnowledgeModelForm uuid form =
    let
        parentPackage =
            case form.parentPackageId of
                Just parentPackageId ->
                    Encode.string parentPackageId

                Nothing ->
                    Encode.null
    in
    Encode.object
        [ ( "uuid", Encode.string uuid )
        , ( "name", Encode.string form.name )
        , ( "kmId", Encode.string form.kmId )
        , ( "parentPackageId", parentPackage )
        , ( "lastAppliedParentPackageId", parentPackage )
        , ( "organizationId", Encode.string "" )
        ]


initKnowledgeModelPublishForm : Form CustomFormError KnowledgeModelPublishForm
initKnowledgeModelPublishForm =
    Form.initial [] knowledgeModelPublishFormValidation


knowledgeModelPublishFormValidation : Validation CustomFormError KnowledgeModelPublishForm
knowledgeModelPublishFormValidation =
    Validate.map4 KnowledgeModelPublishForm
        (Validate.field "major" (Validate.int |> Validate.andThen (Validate.minInt 0)))
        (Validate.field "minor" (Validate.int |> Validate.andThen (Validate.minInt 0)))
        (Validate.field "patch" (Validate.int |> Validate.andThen (Validate.minInt 0)))
        (Validate.field "description" Validate.string)


encodeKnowledgeModelPublishForm : KnowledgeModelPublishForm -> ( String, Encode.Value )
encodeKnowledgeModelPublishForm form =
    let
        version =
            String.join "." <| List.map toString [ form.major, form.minor, form.patch ]

        object =
            Encode.object [ ( "description", Encode.string form.description ) ]
    in
    ( version, object )


initKnowledgeModelUpgradeForm : Form CustomFormError KnowledgeModelUpgradeForm
initKnowledgeModelUpgradeForm =
    Form.initial [] knowledgeModelUpgradeFormValidation


knowledgeModelUpgradeFormValidation : Validation CustomFormError KnowledgeModelUpgradeForm
knowledgeModelUpgradeFormValidation =
    Validate.map KnowledgeModelUpgradeForm
        (Validate.field "targetPackageId" Validate.string)


encodeKnowledgeModelUpgradeForm : KnowledgeModelUpgradeForm -> Encode.Value
encodeKnowledgeModelUpgradeForm form =
    Encode.object
        [ ( "targetPackageId", Encode.string form.targetPackageId ) ]
