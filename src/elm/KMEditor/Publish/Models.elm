module KMEditor.Publish.Models exposing
    ( Model
    , PublishForm
    , encodePublishForm
    , initEmptyPublishForm
    , initialModel
    , publishFormValidation
    )

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Json.Encode as Encode exposing (..)
import KMEditor.Common.Models exposing (..)
import String exposing (fromInt)


type alias Model =
    { knowledgeModel : ActionResult KnowledgeModelDetail
    , publishingKnowledgeModel : ActionResult String
    , form : Form CustomFormError PublishForm
    }


initialModel : Model
initialModel =
    { knowledgeModel = Loading
    , publishingKnowledgeModel = Unset
    , form = initEmptyPublishForm
    }


type alias PublishForm =
    { major : Int
    , minor : Int
    , patch : Int
    , description : String
    , readme : String
    }


initEmptyPublishForm : Form CustomFormError PublishForm
initEmptyPublishForm =
    Form.initial [] publishFormValidation


publishFormValidation : Validation CustomFormError PublishForm
publishFormValidation =
    Validate.map5 PublishForm
        (Validate.field "major" (Validate.int |> Validate.andThen (Validate.minInt 0)))
        (Validate.field "minor" (Validate.int |> Validate.andThen (Validate.minInt 0)))
        (Validate.field "patch" (Validate.int |> Validate.andThen (Validate.minInt 0)))
        (Validate.field "description" Validate.string)
        (Validate.field "readme" Validate.string)


encodePublishForm : PublishForm -> ( String, Encode.Value )
encodePublishForm form =
    let
        version =
            String.join "." <| List.map fromInt [ form.major, form.minor, form.patch ]

        object =
            Encode.object
                [ ( "description", Encode.string form.description )
                , ( "readme", Encode.string form.readme )
                ]
    in
    ( version, object )
