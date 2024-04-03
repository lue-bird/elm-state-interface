module List.LocalExtra exposing (firstJustMap, foldUpIndexedFrom, justs, justsMapIndexed)


firstJustMap : (element -> Maybe found) -> List element -> Maybe found
firstJustMap tryMapToFound list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case tryMapToFound head of
                Just b ->
                    Just b

                Nothing ->
                    firstJustMap tryMapToFound tail


justs : List (Maybe value) -> List value
justs =
    \list -> list |> List.filterMap identity


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


foldUpIndexedFrom : folded -> (Int -> element -> (folded -> folded)) -> List element -> folded
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
