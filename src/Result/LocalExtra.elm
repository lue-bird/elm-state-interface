module Result.LocalExtra exposing (valueOrOnError)


valueOrOnError : (error -> value) -> (Result error value -> value)
valueOrOnError errorToValue =
    \result ->
        case result of
            Ok value ->
                value

            Err error ->
                error |> errorToValue
