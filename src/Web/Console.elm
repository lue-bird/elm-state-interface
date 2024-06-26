module Web.Console exposing (log, warn, error)

{-| Helpers for console interactions as part of an [`Interface`](Web#Interface)

@docs log, warn, error

-}

import Rope
import Web


{-| An [`Interface`](Web#Interface) for printing a message with general information
like if certain tasks have been successful

> survey submitted and received successfully

Depending on what minifying tools you use for your production build, these might get removed.

Note: uses [`console.log`](https://developer.mozilla.org/en-US/docs/Web/API/console/log_static),
just like [`Debug.log`](https://dark.elm.dmy.fr/packages/elm/core/latest/Debug#log)

-}
log : String -> Web.Interface future_
log string =
    Web.ConsoleLog string
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for printing a message that something didn't succeed but you could recover from, for example

> ⚠️ Unknown device - there may be compatibility issues.

> ⚠️ Recoverable upload failure, will retry. Error was: no status.

Note: uses [`console.warn`](https://developer.mozilla.org/en-US/docs/Web/API/console/warn_static)

-}
warn : String -> Web.Interface future_
warn string =
    Web.ConsoleWarn string
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for printing a message that something failed with bad consequences, for example

> ❌ decoding the selected file failed. Please report this bug at ...

Note: uses [`console.error`](https://developer.mozilla.org/en-US/docs/Web/API/console/error_static)

-}
error : String -> Web.Interface future_
error string =
    Web.ConsoleError string
        |> Rope.singleton
