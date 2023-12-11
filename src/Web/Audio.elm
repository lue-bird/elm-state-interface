module Web.Audio exposing
    ( sourceLoad, fromSource, play
    , volumeScaleBy, delayBy, speedScaleBy, volumeScaleTimeline, pan
    )

{-| Play [`Audio`](Web#Audio) as part of an [`Interface`](Web#Interface).
Documentation and js implementation inspired by [MartinSStewart/elm-audio](https://dark.elm.dmy.fr/packages/MartinSStewart/elm-audio/latest/). Thanks!

    import Time
    import Web
    import Web.Audio

    type alias State =
        { musicSource : Maybe (Result Web.AudioSourceLoadError Audio.Source)
        , musicStartTime : Time.Posix
        , soundSetting : SoundSetting
        }

    type SoundSetting
        = SoundOn
        | SoundOff

    { initialState =
        { musicSource = Nothing
        , soundSetting = SoundOn
        , musicStartTime = Time.millisToPosix 0
        }
    , interface =
        \state ->
            case state.musicSource of
                Just (Ok musicSource) ->
                    case state.soundSetting of
                        SoundOff ->
                            Web.interfaceNone

                        SoundOn ->
                            Web.Audio.fromSource musicSource model.musicStartTime
                                |> Web.Audio.play

                _ ->
                    Web.Audio.sourceLoad "https://cors-anywhere.herokuapp.com/https://freepd.com/music/Wakka%20Wakka.mp3"
                        |> Web.interfaceMap
                            (\result -> { state | musicSource = result |> Just })
    , ...
    }

@docs sourceLoad, fromSource, play
@docs volumeScaleBy, delayBy, speedScaleBy, volumeScaleTimeline, pan

To detune, use [`speedScaleBy`](#speedScaleBy). It's documentation also shows which scale relates to which semitone pitch.

-}

import Dict
import Duration exposing (Duration)
import Rope
import Time
import Web exposing (Audio, AudioSource)


{-| Start later by a given [Duration](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Duration)
-}
delayBy : Duration -> (Audio -> Audio)
delayBy delay =
    \a -> { a | startTime = Duration.addTo a.startTime delay }


{-| Change the stereo panning with a given a signed percentage.

For example `Web.Audio.pan -0.9` means that the sound is almost fully balanced towards the left speaker

-}
pan : Float -> (Audio -> Audio)
pan signedPercentage =
    \a ->
        { a
            | pan =
                if signedPercentage < 0 then
                    a.pan + (Basics.min 1 (abs signedPercentage) * (-1 - a.pan))

                else
                    a.pan + (Basics.min 1 (abs signedPercentage) * (1 - a.pan))
        }


{-| Scale the playback rate by a given factor. This will also affect pitch.

For example, `Web.Audio.speedScaleBy 0.5` means playback will take twice as long and the pitch will be one octave lower, see [AudioBufferSourceNode.playbackRate](https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/playbackRate)

In general, to pitch by semitones:

    Web.Audio.speedScaleBy (2 ^ (semitones / 12))

Note: It would be possible to modify the signal to compensate for the pitch change,
see [Audio time stretching and pitch scaling](https://en.wikipedia.org/wiki/Audio_time_stretching_and_pitch_scaling).
Help appreciated!

-}
speedScaleBy : Float -> (Audio -> Audio)
speedScaleBy speedScaleFactor =
    \a -> { a | speed = a.speed * speedScaleFactor }


{-| Scale how loud it is.
1 preserves the current volume, 0.5 halves it, and 0 mutes it.
If the the volume is less than 0, 0 will be used instead.
-}
volumeScaleBy : Float -> (Audio -> Audio)
volumeScaleBy volumeScaleFactor =
    \a -> { a | volume = a.volume * Basics.max 0 volumeScaleFactor }


{-| Scale how loud it is at different points in time.
The volume will transition linearly between those points.
The points in time don't need to be sorted but they need to be unique.

Let's define an audio function that fades in to full volume and then fades out until it's muted again.

    import Duration
    import Time
    import Web.Audio


    -- 1                ________
    --                /         \
    -- 0 ____________/           \_______
    --    t ->    fade in     fade out
    fadeInOut fadeInTime fadeOutTime audio =
        Web.Audio.scaleVolumeAt
            [ ( Duration.subtractFrom fadeInTime Duration.second, 0 )
            , ( fadeInTime, 1 )
            , ( fadeOutTime, 1 )
            , ( Duration.addTo fadeOutTime Duration.second, 0 )
            ]
            audio

(`Duration` is from [ianmackenzie/elm-units](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/), `Time` is from [elm/time](https://dark.elm.dmy.fr/packages/elm/time/latest/))

-}
volumeScaleTimeline : List ( Time.Posix, Float ) -> (Audio -> Audio)
volumeScaleTimeline volumeTimesAndScale =
    \a ->
        { a
            | volumeTimelines =
                (case volumeTimesAndScale of
                    [] ->
                        Dict.singleton 0 1

                    firstVolumeAt :: secondToLastVolumeAt ->
                        (firstVolumeAt :: secondToLastVolumeAt)
                            |> List.map (\( at, value ) -> ( at |> Time.posixToMillis, value |> Basics.max 0 ))
                            |> Dict.fromList
                )
                    :: a.volumeTimelines
        }


{-| Play audio from an audio source at a given time.

    -- Here we play a song at half speed and it skips the first 15 seconds of the song.
    Web.Audio.fromSource
        myCoolSong
        songStartTime
        |> Web.Audio.speedScaleBy 0.5
        |> Web.Audio.delayBy (Duration.seconds 15)

Note that in some browsers audio will be muted until the user interacts with the webpage.

-}
fromSource : AudioSource -> Time.Posix -> Audio
fromSource source startTime =
    { url = source.url
    , startTime = startTime
    , volume = 1
    , volumeTimelines = []
    , speed = 1
    , pan = 0
    }


{-| An [`Interface`](Web#Interface) for fetching audio data from a given url
and returning an [`AudioSource`](Web#AudioSource) to use with [`fromSource`](#fromSource).
-}
sourceLoad : String -> Web.Interface (Result Web.AudioSourceLoadError Web.AudioSource)
sourceLoad url =
    Web.AudioSourceLoad { url = url, on = identity }
        |> Web.InterfaceWithReceive
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for playing [`Audio`](Web#Audio) created with [`fromSource`](#fromSource).

To play multiple audios:

    [ audio0, audio1, audio2 ]
        |> List.map Web.Audio.play
        |> Web.interfaceBatch

-}
play : Audio -> Web.Interface state_
play audio =
    Web.AudioPlay
        { audio | startTime = Duration.addTo audio.startTime (Duration.milliseconds 50) }
        |> Web.InterfaceWithoutReceive
        |> Rope.singleton
