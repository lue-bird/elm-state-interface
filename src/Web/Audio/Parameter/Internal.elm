module Web.Audio.Parameter.Internal exposing (scaleAlongParameter, valuesAlter)

import Time
import Web exposing (AudioParameterTimeline)


valuesAlter : (Float -> Float) -> (AudioParameterTimeline -> AudioParameterTimeline)
valuesAlter valueAlter =
    \timeline ->
        { startValue = timeline.startValue |> valueAlter
        , keyFrames =
            timeline.keyFrames
                |> List.map (\keyFrame -> { time = keyFrame.time, value = keyFrame.value |> valueAlter })
        }


scaleAlongParameter : Time.Posix -> AudioParameterTimeline -> (AudioParameterTimeline -> AudioParameterTimeline)
scaleAlongParameter startTime timelineToScaleBy =
    \audioParameterTimelineToScale ->
        let
            startValue : Float
            startValue =
                audioParameterTimelineToScale.startValue * timelineToScaleBy.startValue
        in
        { startValue = startValue
        , keyFrames =
            audioParameterTimelineToScale.keyFrames
                |> List.sortBy (\keyFrame -> keyFrame.time |> Time.posixToMillis)
                |> keyFramesScaleAlong
                    { previous = { time = startTime, value = startValue }
                    , toScaleBy =
                        timelineToScaleBy.keyFrames
                            |> List.sortBy (\keyFrame -> keyFrame.time |> Time.posixToMillis)
                    }
        }


keyFramesScaleAlong :
    { toScaleBy : List { time : Time.Posix, value : Float }
    , previous : { time : Time.Posix, value : Float }
    }
    -> (List { time : Time.Posix, value : Float } -> List { time : Time.Posix, value : Float })
keyFramesScaleAlong state =
    \ŧoScale ->
        List.map2
            (\keyFrameToScale keyFrameToScaleBy ->
                { time = keyFrameToScale.time
                , value = keyFrameToScale.value * keyFrameToScaleBy.value
                }
            )
            (ŧoScale |> keyFramesAddSubs { subs = state.toScaleBy |> List.map .time, previous = state.previous })
            (state.toScaleBy |> keyFramesAddSubs { subs = ŧoScale |> List.map .time, previous = state.previous })


keyFramesAddSubs :
    { subs : List Time.Posix
    , previous : { time : Time.Posix, value : Float }
    }
    -> (List { time : Time.Posix, value : Float } -> List { time : Time.Posix, value : Float })
keyFramesAddSubs state =
    -- IGNORE TCO
    \keyFrames ->
        case state.subs of
            [] ->
                keyFrames

            currentSub :: afterCurrentSub ->
                case keyFrames of
                    [] ->
                        (currentSub :: afterCurrentSub)
                            |> List.map (\subTime -> { time = subTime, value = state.previous.value })

                    currentKeyFrame :: afterCurrentKeyFrame ->
                        case compare (currentKeyFrame.time |> Time.posixToMillis) (currentSub |> Time.posixToMillis) of
                            EQ ->
                                currentKeyFrame
                                    :: (afterCurrentKeyFrame
                                            |> keyFramesAddSubs
                                                { subs = afterCurrentSub
                                                , previous = currentKeyFrame
                                                }
                                       )

                            LT ->
                                currentKeyFrame
                                    :: (afterCurrentKeyFrame
                                            |> keyFramesAddSubs
                                                { subs = currentSub :: afterCurrentSub
                                                , previous = currentKeyFrame
                                                }
                                       )

                            GT ->
                                let
                                    locationBetween : Float
                                    locationBetween =
                                        ((currentKeyFrame.time |> Time.posixToMillis) - (currentSub |> Time.posixToMillis) |> Basics.toFloat)
                                            / ((currentKeyFrame.time |> Time.posixToMillis) - (state.previous.time |> Time.posixToMillis) |> Basics.toFloat)

                                    subKeyFrame : { time : Time.Posix, value : Float }
                                    subKeyFrame =
                                        { time = currentSub
                                        , value = linearlyInterpolate state.previous.value currentKeyFrame.value locationBetween
                                        }
                                in
                                subKeyFrame
                                    :: (afterCurrentKeyFrame
                                            |> keyFramesAddSubs
                                                { subs = currentSub :: afterCurrentSub
                                                , previous = subKeyFrame
                                                }
                                       )


linearlyInterpolate : Float -> Float -> Float -> Float
linearlyInterpolate startValue endValue progress =
    if Basics.isInfinite progress then
        startValue

    else
        progress * (endValue - startValue) + startValue
