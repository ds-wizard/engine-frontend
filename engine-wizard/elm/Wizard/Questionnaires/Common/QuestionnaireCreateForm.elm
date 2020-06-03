module Wizard.Questionnaires.Common.QuestionnaireCreateForm exposing (QuestionnaireCreateForm, encode, init, validation)

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Questionnaires.Common.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)


type alias QuestionnaireCreateForm =
    { name : String
    , packageId : String
    , visibility : QuestionnaireVisibility
    }


init : Maybe String -> Form CustomFormError QuestionnaireCreateForm
init selectedPackage =
    let
        initials =
            case selectedPackage of
                Just packageId ->
                    [ ( "packageId", Field.string packageId ) ]

                _ ->
                    []

        initialsWithVisibility =
            initials ++ [ ( "visibility", Field.string <| QuestionnaireVisibility.toString QuestionnaireVisibility.PrivateQuestionnaire ) ]
    in
    Form.initial initialsWithVisibility validation


validation : Validation CustomFormError QuestionnaireCreateForm
validation =
    Validate.map3 QuestionnaireCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "packageId" Validate.string)
        (Validate.field "visibility" QuestionnaireVisibility.validation)


encode : List String -> QuestionnaireCreateForm -> E.Value
encode tagUuids form =
    E.object
        [ ( "name", E.string form.name )
        , ( "packageId", E.string form.packageId )
        , ( "visibility", QuestionnaireVisibility.encode form.visibility )
        , ( "tagUuids", E.list E.string tagUuids )
        , ( "templateUuid", E.maybe E.string Nothing )
        ]
