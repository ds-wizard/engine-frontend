module Common.Questionnaire.Models.FeedbackForm exposing
    ( FeedbackForm
    , encode
    , initEmpty
    , validation
    )

import Common.Form exposing (CustomFormError)
import Form
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E


type alias FeedbackForm =
    { title : String
    , content : String
    }


initEmpty : Form.Form CustomFormError FeedbackForm
initEmpty =
    Form.initial [] validation


validation : Validation CustomFormError FeedbackForm
validation =
    Validate.map2 FeedbackForm
        (Validate.field "title" Validate.string)
        (Validate.field "content" Validate.string)


encode : String -> String -> FeedbackForm -> E.Value
encode questionUuid packageId form =
    E.object
        [ ( "questionUuid", E.string questionUuid )
        , ( "packageId", E.string packageId )
        , ( "title", E.string form.title )
        , ( "content", E.string form.content )
        ]
