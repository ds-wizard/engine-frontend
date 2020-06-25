module WizardResearch.Pages.ProjectCreate.ProjectCreateForm exposing
    ( ProjectCreateForm
    , encode
    , init
    , selectRecommendedOrFirstTemplate
    , selectRecommendedPackage
    , validation
    )

-- MODEL

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility
import Shared.Data.Template as Template exposing (Template)
import Shared.Form.FormError exposing (FormError)
import Uuid
import WizardResearch.Common.AppState exposing (AppState)


type alias ProjectCreateForm =
    { name : String
    , templateId : String
    , packageId : String
    , tagUuids : List String
    }


validation : List Tag -> Validation FormError ProjectCreateForm
validation tags =
    let
        validateTag tag =
            V.field tag.uuid
                (V.bool
                    |> V.andThen
                        (\checked ->
                            if checked then
                                V.succeed (Just tag.uuid)

                            else
                                V.succeed Nothing
                        )
                )

        validateTagUuids =
            V.sequence (List.map validateTag tags)
                |> V.map (List.filterMap identity)
    in
    V.succeed ProjectCreateForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "templateId" V.string)
        |> V.andMap (V.field "packageId" V.string)
        |> V.andMap (V.field "tagUuids" validateTagUuids)


init : Form FormError ProjectCreateForm
init =
    Form.initial [] (validation [])


encode : AppState -> ProjectCreateForm -> E.Value
encode appState form =
    E.object
        [ ( "name", E.string form.name )
        , ( "templateId", E.string form.templateId )
        , ( "packageId", E.string form.packageId )
        , ( "tagUuids", E.list E.string form.tagUuids )
        , ( "visibility", QuestionnaireVisibility.encode appState.config.questionnaire.questionnaireVisibility.defaultValue )
        ]


selectRecommendedOrFirstTemplate : List Tag -> List Template -> Maybe String -> Form FormError ProjectCreateForm -> Form FormError ProjectCreateForm
selectRecommendedOrFirstTemplate tags templates mbRecommendedTemplateId form =
    let
        mbTemplateId =
            case mbRecommendedTemplateId of
                Just uuid ->
                    Just uuid

                Nothing ->
                    Maybe.map .id (List.head templates)
    in
    case mbTemplateId of
        Just templateId ->
            let
                msg =
                    Form.Input "templateId" Form.Text (Field.String templateId)
            in
            Form.update (validation tags) msg form

        Nothing ->
            form


selectRecommendedPackage : List Tag -> List Template -> Form FormError ProjectCreateForm -> Form FormError ProjectCreateForm
selectRecommendedPackage tags templates form =
    let
        mbTemplate =
            (Form.getFieldAsString "templateId" form).value
                |> Maybe.andThen (Template.findById templates)
    in
    case mbTemplate of
        Just template ->
            case template.recommendedPackageId of
                Just recommendedPackageId ->
                    let
                        msg =
                            Form.Input "packageId" Form.Text (Field.String recommendedPackageId)
                    in
                    Form.update (validation tags) msg form

                Nothing ->
                    form

        _ ->
            form
