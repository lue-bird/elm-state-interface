module List.LocalExtra exposing (firstJustMap, foldUpIndexedFrom)


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
