module Common.Components.RowNotice exposing (view)

{-| What an application has to say at the top of a thing's own page
(`Common.Data.RowPolicy.Notice`): a sentence, and a link if there is one.
Purple, as the row's badge is (`Common.Components.RowBadge`), because it says
whose the thing is rather than its state. Nothing when the application says
nothing.
-}

import Common.Data.RowPolicy exposing (Notice)
import Html exposing (Html)
import Ui.Alert as Alert
import Ui.Icon as Icon
import Ui.Link as Link


view : Maybe Notice -> Html msg
view mbNotice =
    case mbNotice of
        Just notice ->
            Alert.private ""
                -- The private alert's lock would say who can see it; the
                -- notice says where it comes from.
                |> Alert.withIcon (Icon.solid (Maybe.withDefault "fa-circle-info" notice.icon))
                |> Alert.toHtmlWithContent
                    (Html.text notice.text
                        :: (case notice.link of
                                Just link ->
                                    [ Html.text " ", Link.toHtml (Link.link link) ]

                                Nothing ->
                                    []
                           )
                    )

        Nothing ->
            Html.text ""
