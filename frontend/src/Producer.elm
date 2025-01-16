module Producer exposing (..)

import CSGSchema exposing (Carrier(..), JValue(..), JsonValue(..))
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


type alias ProducerConfig =
    { firstName : String
    , lastName : String
    , phone : String
    , email : String
    , address : String
    , city : String
    , state : String
    , zip : String
    , writingNumbers : Carrier -> String
    , isDefault : Bool
    , id : Int
    }


jdebug : String -> Decode.Decoder a -> Decode.Decoder a
jdebug message decoder =
    Decode.value
        |> Decode.andThen (debugHelper message decoder)


debugHelper : String -> Decode.Decoder a -> Decode.Value -> Decode.Decoder a
debugHelper message decoder value =
    decoder


producerConfigDecoder : Decoder (Dict Int ProducerConfig)
producerConfigDecoder =
    Decode.field "producers" (Decode.list producerConfigItemDecoder)
        |> Decode.map (List.map (\config -> ( config.id, config )))
        |> Decode.map Dict.fromList
        |> jdebug "PRODUCER CONFIG"


producerConfigItemDecoder : Decoder ProducerConfig
producerConfigItemDecoder =
    Decode.succeed ProducerConfig
        |> required "firstName" Decode.string
        |> required "lastName" Decode.string
        |> required "phone" Decode.string
        |> required "email" Decode.string
        |> required "addressLine1" Decode.string
        |> required "addressCity" Decode.string
        |> required "addressState" Decode.string
        |> required "addressZip5" Decode.string
        |> required "writingNumbers" writingNumbersDecoder
        |> optional "isDefault" Decode.bool False
        |> required "id" Decode.int


writingNumbersDecoder : Decoder (Carrier -> String)
writingNumbersDecoder =
    Decode.map4
        (\aetna ace allstate uhc carrier ->
            case carrier of
                Aetna ->
                    aetna

                ACE ->
                    ace

                Allstate ->
                    allstate

                UHC ->
                    uhc
        )
        (Decode.field "Aetna" Decode.string)
        (Decode.field "Chubb" Decode.string)
        (Decode.field "Allstate" Decode.string)
        (Decode.field "UnitedHealthcare" Decode.string)


formatPhone : String -> JsonValue
formatPhone phone =
    let
        areaCode =
            String.slice 0 3 phone

        officeCode =
            String.slice 3 6 phone

        stationCode =
            String.slice 6 10 phone
    in
    JsonObject
        (Dict.fromList
            [ ( "area_code", JsonBase (StringValue areaCode) )
            , ( "central_office_code", JsonBase (StringValue officeCode) )
            , ( "station_code", JsonBase (StringValue stationCode) )
            ]
        )


getProducerSection : Carrier -> ProducerConfig -> JsonValue
getProducerSection carrier config =
    case carrier of
        Aetna ->
            JsonObject
                (Dict.fromList
                    [ ( "producer_first_name", JsonBase (StringValue config.firstName) )
                    , ( "producer_last_name", JsonBase (StringValue config.lastName) )
                    , ( "producer_phone", formatPhone config.phone )
                    , ( "producer_email", JsonBase (StringValue config.email) )
                    , ( "producer_writing_number", JsonBase (StringValue (config.writingNumbers Aetna)) )
                    , ( "deliver_policy_to", JsonBase (StringValue "applicant") )
                    , ( "e_delivery", JsonBase (BoolValue False) )
                    , ( "accurate_recording", JsonBase (BoolValue True) )
                    , ( "interviewed_applicants", JsonBase (BoolValue True) )
                    , ( "application_provided", JsonBase (BoolValue True) )
                    , ( "replacement_notice_copy", JsonBase (BoolValue True) )
                    , ( "agent_requests_split_commissions", JsonBase (BoolValue False) )
                    ]
                )

        ACE ->
            JsonObject
                (Dict.fromList
                    [ ( "business_type", JsonBase (StringValue "new") )
                    , ( "has_other_inforce_policies", JsonBase (BoolValue False) )
                    , ( "Electronic_Combined", JsonBase (BoolValue False) )
                    , ( "deliver_policy_to", JsonBase (StringValue "APP") )
                    , ( "policy_delivery_type", JsonBase (StringValue "paper") )
                    , ( "producer_first_name", JsonBase (StringValue config.firstName) )
                    , ( "producer_last_name", JsonBase (StringValue config.lastName) )
                    , ( "agent_address_line1", JsonBase (StringValue config.address) )
                    , ( "agent_zip5", JsonBase (StringValue config.zip) )
                    , ( "agent_address_city", JsonBase (StringValue config.city) )
                    , ( "agent_address_state", JsonBase (StringValue config.state) )
                    , ( "replacement_notice_copy", JsonBase (BoolValue True) )
                    ]
                )

        Allstate ->
            JsonObject
                (Dict.fromList
                    [ ( "producer_first_name", JsonBase (StringValue config.firstName) )
                    , ( "producer_last_name", JsonBase (StringValue config.lastName) )
                    , ( "producer_phone", formatPhone config.phone )
                    , ( "producer_email", JsonBase (StringValue config.email) )
                    , ( "producer_writing_number", JsonBase (StringValue (config.writingNumbers Allstate)) )
                    , ( "sale", JsonBase (StringValue "internet") )
                    , ( "other_sale_type_description", JsonBase (StringValue "") )
                    , ( "has_other_inforce_policies", JsonBase (BoolValue False) )
                    , ( "deliver_policy_to", JsonBase (StringValue "applicant") )
                    , ( "additional_witness", JsonBase (BoolValue False) )
                    , ( "agent_related", JsonBase (BoolValue False) )
                    , ( "agent_reviewed", JsonBase (BoolValue True) )
                    , ( "applicant_reviewed", JsonBase (BoolValue True) )
                    , ( "replacement_notice_copy", JsonBase (BoolValue True) )
                    ]
                )

        UHC ->
            JsonObject
                (Dict.fromList
                    [ ( "agent_first_name", JsonBase (StringValue config.firstName) )
                    , ( "agent_last_name", JsonBase (StringValue config.lastName) )
                    , ( "producer_phone", formatPhone config.phone )
                    , ( "producer_email", JsonBase (StringValue config.email) )
                    , ( "producer_writing_number", JsonBase (StringValue (config.writingNumbers UHC)) )
                    , ( "policy_delivery_type", JsonBase (StringValue "Mail") )
                    ]
                )
