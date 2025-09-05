module Common.Utils.FileIcon exposing (defaultIcon, getFileIcon)

import Common.Utils.FileUtils as FileUtils
import List.Extra as List


getFileIcon : String -> String -> String
getFileIcon fileName mime =
    let
        extension =
            FileUtils.getExtension fileName

        match ( _, filter ) =
            List.member extension filter.extensions || List.member mime filter.mimes
    in
    List.find match fileIcons
        |> Maybe.map Tuple.first
        |> Maybe.withDefault defaultIcon


defaultIcon : String
defaultIcon =
    "far fa-file"


type alias FileIconFilter =
    { extensions : List String
    , mimes : List String
    }


fileIcons : List ( String, FileIconFilter )
fileIcons =
    [ ( "far fa-file-audio"
      , { extensions =
            [ "aac"
            , "flac"
            , "m4a"
            , "mp3"
            , "ogg"
            , "wav"
            , "wma"
            ]
        , mimes =
            [ "audio/aac"
            , "audio/flac"
            , "audio/m4a"
            , "audio/mp3"
            , "audio/ogg"
            , "audio/wav"
            , "audio/wma"
            ]
        }
      )
    , ( "far fa-file-code"
      , { extensions =
            [ "css"
            , "html"
            , "js"
            , "php"
            , "py"
            , "rb"
            , "xml"
            ]
        , mimes =
            [ "text/css"
            , "text/html"
            , "text/javascript"
            , "text/x-php"
            , "text/x-python"
            , "text/x-ruby"
            , "text/xml"
            ]
        }
      )
    , ( "far fa-file-excel"
      , { extensions =
            [ "xls"
            , "xlsx"
            ]
        , mimes =
            [ "application/vnd.ms-excel"
            , "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
            ]
        }
      )
    , ( "far fa-file-image"
      , { extensions =
            [ "bmp"
            , "gif"
            , "jpg"
            , "jpeg"
            , "png"
            , "svg"
            ]
        , mimes =
            [ "image/bmp"
            , "image/gif"
            , "image/jpeg"
            , "image/png"
            , "image/svg+xml"
            ]
        }
      )
    , ( "far fa-file-lines"
      , { extensions =
            [ "log"
            , "md"
            , "txt"
            ]
        , mimes =
            [ "text/markdown"
            , "text/plain"
            ]
        }
      )
    , ( "far fa-file-pdf"
      , { extensions =
            [ "pdf"
            ]
        , mimes =
            [ "application/pdf"
            ]
        }
      )
    , ( "far fa-file-powerpoint"
      , { extensions =
            [ "ppt"
            , "pptx"
            ]
        , mimes =
            [ "application/vnd.ms-powerpoint"
            , "application/vnd.openxmlformats-officedocument.presentationml.presentation"
            ]
        }
      )
    , ( "far fa-file-video"
      , { extensions =
            [ "avi"
            , "flv"
            , "m4v"
            , "mkv"
            , "mov"
            , "mp4"
            , "mpg"
            , "wmv"
            ]
        , mimes =
            [ "video/avi"
            , "video/flv"
            , "video/mp4"
            , "video/mpeg"
            , "video/quicktime"
            , "video/webm"
            ]
        }
      )
    , ( "far fa-file-word"
      , { extensions =
            [ "doc"
            , "docx"
            ]
        , mimes =
            [ "application/msword"
            , "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
            ]
        }
      )
    , ( "far fa-file-zipper"
      , { extensions =
            [ "7z"
            , "bz2"
            , "gz"
            , "rar"
            , "tar"
            , "zip"
            ]
        , mimes =
            [ "application/x-7z-compressed"
            , "application/x-bzip2"
            , "application/x-gzip"
            , "application/x-rar-compressed"
            , "application/x-tar"
            , "application/zip"
            ]
        }
      )
    ]
