module Common.Utils.ContentTypeTest exposing (suite)

{-| Whether an uploaded file is text, which decides whether the document
template editor adds it as a file to edit or stores it as an asset.
-}

import Common.Utils.ContentType as ContentType
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Common.Utils.ContentType.isTextFile"
        [ test "the browser's type decides when there is one" <|
            \_ ->
                Expect.equal [ True, True, False ]
                    [ ContentType.isTextFile "style.css" "text/css"
                    , ContentType.isTextFile "data.json" "application/json"
                    , ContentType.isTextFile "logo.png" "image/png"
                    ]
        , test "the extension decides when the browser gives no type" <|
            \_ ->
                Expect.equal [ True, True, False ]
                    [ ContentType.isTextFile "default.html.j2" ""
                    , ContentType.isTextFile "notes.txt" ""
                    , ContentType.isTextFile "brand.ttf" ""
                    ]
        , test "a name with no known extension and no type is not text" <|
            \_ ->
                Expect.equal [ False, False ]
                    [ ContentType.isTextFile "LICENSE" ""
                    , ContentType.isTextFile "blob.xyz" ""
                    ]
        ]
