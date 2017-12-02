module KnowledgeModels.Models exposing (..)

import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (..)
import List.Extra as List
import Utils exposing (validateRegex)


type alias KnowledgeModel =
    { uuid : String
    , name : String
    , groupId : String
    , artifactId : String
    , parentPackageId : Maybe String
    , lastAppliedParentPackageId : Maybe String
    , stateType : KnowledgeModelState
    }


type KnowledgeModelState
    = Default
    | Edited
    | Outdated
    | Migrating


knowledgeModelDecoder : Decoder KnowledgeModel
knowledgeModelDecoder =
    decode KnowledgeModel
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "groupId" Decode.string
        |> required "artifactId" Decode.string
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
                        |> Maybe.map ((==) km.artifactId)
                        |> Maybe.withDefault False

                sameGroup =
                    List.getAt 0 parts
                        |> Maybe.map ((==) km.groupId)
                        |> Maybe.withDefault False
            in
            if sameGroup && samePackage then
                List.getAt 2 parts
            else
                Nothing
    in
    km.parentPackageId
        |> Maybe.andThen getVersion


type alias KnowledgeModelCreateForm =
    { name : String
    , artifactId : String
    , parentPackageId : Maybe String
    }


initKnowledgeModelCreateForm : Form () KnowledgeModelCreateForm
initKnowledgeModelCreateForm =
    Form.initial [] knowledgeModelCreateFormValidation


knowledgeModelCreateFormValidation : Validation () KnowledgeModelCreateForm
knowledgeModelCreateFormValidation =
    Validate.map3 KnowledgeModelCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "artifactId" (validateRegex "^^(?![-])(?!.*[-]$)[a-zA-Z0-9-]+$"))
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
        , ( "artifactId", Encode.string form.artifactId )
        , ( "parentPackageId", parentPackage )
        , ( "lastAppliedParentPackageId", parentPackage )
        , ( "groupId", Encode.string "" )
        ]


type alias KnowledgeModelPublishForm =
    { major : Int
    , minor : Int
    , patch : Int
    , description : String
    }


initKnowledgeModelPublishForm : Form () KnowledgeModelPublishForm
initKnowledgeModelPublishForm =
    Form.initial [] knowledgeModelPublishFormValidation


knowledgeModelPublishFormValidation : Validation () KnowledgeModelPublishForm
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


type alias KnowledgeModelUpgradeForm =
    { targetPackageId : String
    }


initKnowledgeModelUpgradeForm : Form () KnowledgeModelUpgradeForm
initKnowledgeModelUpgradeForm =
    Form.initial [] knowledgeModelUpgradeFormValidation


knowledgeModelUpgradeFormValidation : Validation () KnowledgeModelUpgradeForm
knowledgeModelUpgradeFormValidation =
    Validate.map KnowledgeModelUpgradeForm
        (Validate.field "targetPackageId" Validate.string)


encodeKnowledgeModelUpgradeForm : KnowledgeModelUpgradeForm -> Encode.Value
encodeKnowledgeModelUpgradeForm form =
    Encode.object
        [ ( "targetPackageId", Encode.string form.targetPackageId ) ]
