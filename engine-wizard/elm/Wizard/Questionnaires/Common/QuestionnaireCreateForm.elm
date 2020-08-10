module Wizard.Questionnaires.Common.QuestionnaireCreateForm exposing (QuestionnaireCreateForm, encode, init, validation)

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)


type alias QuestionnaireCreateForm =
    { name : String
    , packageId : String
    , visibility : QuestionnaireVisibility
    , sharing : QuestionnaireSharing
    }


init : AppState -> Maybe String -> Form FormError QuestionnaireCreateForm
init appState selectedPackage =
    let
        initialPackageId =
            case selectedPackage of
                Just packageId ->
                    [ ( "packageId", Field.string packageId ) ]

                _ ->
                    []

        initials =
            initialPackageId
                ++ [ ( "visibility", QuestionnaireVisibility.field appState.config.questionnaire.questionnaireVisibility.defaultValue )
                   , ( "sharing", QuestionnaireSharing.field appState.config.questionnaire.questionnaireSharing.defaultValue )
                   ]
    in
    Form.initial initials validation


validation : Validation FormError QuestionnaireCreateForm
validation =
    Validate.map4 QuestionnaireCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "packageId" Validate.string)
        (Validate.field "visibility" QuestionnaireVisibility.validation)
        (Validate.field "sharing" QuestionnaireSharing.validation)


encode : List String -> QuestionnaireCreateForm -> E.Value
encode tagUuids form =
    E.object
        [ ( "name", E.string form.name )
        , ( "packageId", E.string form.packageId )
        , ( "visibility", QuestionnaireVisibility.encode form.visibility )
        , ( "sharing", QuestionnaireSharing.encode form.sharing )
        , ( "tagUuids", E.list E.string tagUuids )
        , ( "templateId", E.maybe E.string Nothing )
        ]
