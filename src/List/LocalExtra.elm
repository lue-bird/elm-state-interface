module List.LocalExtra exposing (foldUpIndexedFrom, justsMapIndexed, justsToAnyOrder)


justsToAnyOrder : List (Maybe value) -> List value
justsToAnyOrder =
    \list ->
        list
            |> List.foldl
                (\maybe soFar ->
                    case maybe of
                        Nothing ->
                            soFar

                        Just value ->
                            value :: soFar
                )
                []


justsMapIndexed : (Int -> element -> Maybe value) -> (List element -> List value)
justsMapIndexed elementToMaybe =
    \list ->
        list
            |> List.foldr
                (\element soFar ->
                    { index = soFar.index - 1
                    , justs =
                        case element |> elementToMaybe soFar.index of
                            Nothing ->
                                soFar.justs

                            Just just ->
                                just :: soFar.justs
                    }
                )
                { index = (list |> List.length) - 1
                , justs = []
                }
            |> .justs


foldUpIndexedFrom :
    folded
    -> (Int -> element -> (folded -> folded))
    -> (List element -> folded)
foldUpIndexedFrom initialFolded reduce =
    \list ->
        list
            |> List.foldl
                (\element soFar ->
                    { index = soFar.index + 1
                    , folded = soFar.folded |> reduce soFar.index element
                    }
                )
                { index = 0, folded = initialFolded }
            |> .folded
