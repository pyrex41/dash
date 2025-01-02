module DataEncoder exposing (encodeJValue, unflattenData)

import CSGSchema exposing (JValue(..))
import Dict exposing (Dict)
import Json.Encode as Encode


unflattenData : Dict String JValue -> Encode.Value
unflattenData flatData =
    let
        sectionsDotFields =
            Dict.keys flatData

        groupedData =
            List.foldl
                (\key acc ->
                    case Dict.get key flatData of
                        Just value ->
                            case String.split "." key of
                                section :: field :: [] ->
                                    Dict.update section
                                        (\maybeFields ->
                                            case maybeFields of
                                                Just fields ->
                                                    Just (Dict.insert field value fields)

                                                Nothing ->
                                                    Just (Dict.singleton field value)
                                        )
                                        acc

                                _ ->
                                    acc

                        Nothing ->
                            acc
                )
                Dict.empty
                sectionsDotFields

        encodeSection : Dict String JValue -> Encode.Value
        encodeSection sectionData =
            Encode.object
                (Dict.toList sectionData
                    |> List.map (\( field, value ) -> ( field, encodeJValue value ))
                )
    in
    Encode.object
        (Dict.toList groupedData
            |> List.map (\( section, fields ) -> ( section, encodeSection fields ))
        )



-- Helper to encode our JValue type to Json.Encode.Value


encodeJValue : JValue -> Encode.Value
encodeJValue jvalue =
    case jvalue of
        StringValue str ->
            Encode.string str

        IntValue n ->
            Encode.int n

        FloatValue f ->
            Encode.float f

        BoolValue b ->
            Encode.bool b

        NullValue ->
            Encode.null
