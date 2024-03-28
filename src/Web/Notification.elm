module Web.Notification exposing (askForPermission, show)

{-| Give important notices to the user as push notifications.
Consider this a convenience feature, not something users have to rely upon.
Always offer users alternative methods to view messages or initiate actions
and allow users to opt out of getting more in the future.

@docs askForPermission, show

You can combine it with [`Web.Window.visibilityChangeListen`](Web-Window#visibilityChangeListen)
to only notify users when they're on a different page

    import Web

    type State
        = State
            { windowVisibility : Web.WindowVisibility
            , whoseMove : Player
            , mode : Mode
            , notificationPermissionToggle : Permission
            }

    type Permission
        = Rejected
        | Accepted

    type Mode
        = LongGameBoardMode
        | SettingsPage

    type Player
        = You
        | Opponent

    interface : State -> Web.Interface State
    interface =
        \(State state) ->
            [ case state.notificationPermissionToggle of
                Accepted ->
                    Web.Notification.askForPermission

                Rejected ->
                    Web.interfaceNone
            , case ( state.windowVisibility, state.whoseTurn ) of
                ( Web.WindowHidden, You ) ->
                    Web.Notification.show
                        { message = "opponent moved", ... }
                        |> Web.interfaceFutureMap
                            (\Web.NotificationClicked ->
                                -- return to the game if previously in settings
                                { state | mode = LongGameBoardMode } |> State
                            )

                ( Web.WindowHidden, Opponent ) ->
                    Web.interfaceNone

                ( Web.WindowShown, _ ) ->
                    Web.interfaceNone
            , case state.mode of
                LongGameBoardMode ->
                    ..listen for opponent move from server..
                        |> Web.interfaceFutureMap
                            (\... -> { state | whoseMove = You })

                SettingsState ->
                    ..toggle for accepting/rejecting notifications..
                        |> Web.interfaceFutureMap
                            (\... -> { state | notificationPermissionToggle = ..opposite.. })
            ]
                |> Web.interfaceBatch

-}

import Rope
import Web


{-| Ask the user to consent to receiving notifications, if they haven't already.

For security reasons, browsers require some kind of user interaction like a button click first.
So you could for example add a toggle to send notifications and ask only for permission
when the toggle is set.

-}
askForPermission : Web.Interface future_
askForPermission =
    Web.NotificationAskForPermission
        |> Web.InterfaceWithoutFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for adding an important for the user to read.
Users can in the future [respond to it by clicking it](Web#NotificationClicked).

To not notify users multiple times for multiple messages of the same kind,
choose an `id` with a description for this kind
so that the notification with the same kind if they exist.

    case messagesFromOpponent of
        [] ->
            Web.interfaceNone

        [ onlyMessage ] ->
            Web.Notification.show
                { id = "opponent messages"
                , message = "Your opponent left a message"
                , details = "\"" ++ onlyMessage ++ "\""
                }

        _ :: _ :: message2Up ->
            Web.Notification.show
                { id = "opponent messages"
                , message =
                    [ "Your opponent left "
                    , 2 + (message2Up |> List.length) |> String.fromInt
                    , " messages"
                    ]
                        |> String.join
                , details = ""
                }

Another example of a reminder shown in advance

    let
        untilMeeting : Duration
        untilMeeting =
            Duration.from meetingTime currentTime
    in
    if untilMeeting |> Quantity.isLessThan (Duration.minutes 10) then
        Web.Notification.show
            { id = "time until meeting"
            , message =
                [ "the meeting starts in "
                , untilMeeting |> Duration.inMinutes |> Basics.ceiling |> String.fromInt)
                , " minutes"
                ]
                    |> String.concat
            , details = ""
            }

    else
        Web.interfaceNone

Note: If you haven't previously used [`askForPermission`](#askForPermission)
it will request this permission now.

-}
show :
    { message : String, details : String, id : String }
    -> Web.Interface Web.NotificationClicked
show content =
    Web.NotificationShow
        { id = content.id
        , message = content.message
        , details = content.details
        , on = identity
        }
        |> Web.InterfaceWithFuture
        |> Rope.singleton
