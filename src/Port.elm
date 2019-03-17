port module Port exposing (keyboardEvent)


port keyboardEvent : (String -> msg) -> Sub msg
