module List.LocalExtra exposing (firstJustMap)


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
