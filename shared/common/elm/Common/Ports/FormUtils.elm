port module Common.Ports.FormUtils exposing (scrollToInvalidField)

import Form


scrollToInvalidField : Form.Msg -> Cmd msg
scrollToInvalidField formMsg =
    case formMsg of
        Form.Submit ->
            formScrollToInvalidField ()

        _ ->
            Cmd.none


port formScrollToInvalidField : () -> Cmd msg
