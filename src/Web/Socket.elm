module Web.Socket exposing
    ( connectTo, disconnect
    , message, messageListen
    )

{-| Helpers for [Socket types](Web#socket) as part of an [`Interface`](Web#Interface)


## connection

@docs connectTo, disconnect


## communicate

@docs message, messageListen

-}

import Rope
import Web


{-| An [`Interface`](Web#Interface) for opening a connection on a given address,
notifying you when it's [connected or disconnected](Web#SocketConnectionEvent)

Once this detects it's available, make sure to set your state's [`SocketId`](Web#SocketId) so you can actually [send](#message)
and [receive](#messageListen) messages.
And once it's disconnected, set your state's [`SocketId`](Web#SocketId) back to nothing:

    case state.socketId of
        Nothing ->
            Web.Socket.connectTo "ws://127.0.0.1:9000"
                |> Web.interfaceMap
                    (\connectionChanged ->
                        case connectionChanged of
                            Web.SocketConnected socketId ->
                                { state | socketId = socketId |> Just }

                            Web.SocketDisconnected ->
                                { state | socketId = Nothing }
                    )

        Just socketId ->
            Web.Socket.message socketId "Meow"

-}
connectTo : String -> Web.Interface Web.SocketConnectionEvent
connectTo address =
    Web.SocketConnect { address = address, on = identity }
        |> Web.InterfaceWithFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for closing a given connection
-}
disconnect : Web.SocketId -> Web.Interface future_
disconnect id =
    Web.SocketDisconnect id
        |> Web.InterfaceWithoutFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for sending data to the server.

It's common to pair this with [`Json.Encode.encode 0`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Encode#encode)
to send json.

-}
message : Web.SocketId -> String -> Web.Interface future_
message id data =
    Web.SocketMessage { id = id, data = data }
        |> Web.InterfaceWithoutFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for detecting when data has been sent from the server
-}
messageListen : Web.SocketId -> Web.Interface String
messageListen id =
    Web.SocketMessageListen { id = id, on = identity }
        |> Web.Listen
        |> Web.InterfaceWithFuture
        |> Rope.singleton
