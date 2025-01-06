module Producer exposing (..)

import CSGSchema exposing (Carrier(..), JValue(..), JsonValue(..))
import Dict
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias ProducerConfig =
    { firstName : String
    , lastName : String
    , phone : String
    , email : String
    , writingNumbers : Carrier -> String
    }


producerConfigs : Int -> Maybe ProducerConfig
producerConfigs producerId =
    Dict.get producerId
        producerConfigsBase


producerConfigsBase : Dict.Dict Int ProducerConfig
producerConfigsBase =
    Dict.fromList
        [ ( 1
          , { firstName = "Josh"
            , lastName = "Musick"
            , phone = "8167996644"
            , email = "josh.musick@medicareschool.com"
            , writingNumbers =
                \carrier ->
                    case carrier of
                        Aetna ->
                            "GNW0059444"

                        ACE ->
                            "I03CP"

                        Allstate ->
                            "707653"

                        UHC ->
                            "6338279"
            }
          )
        , ( 2
          , { firstName = "Garrett"
            , lastName = "McKinzie"
            , phone = "9137389842"
            , email = "garrett.mckinzie@medicareschool.com"
            , writingNumbers =
                \carrier ->
                    case carrier of
                        Aetna ->
                            "GNW6050581"

                        ACE ->
                            "I03QN"

                        Allstate ->
                            "708947"

                        UHC ->
                            "6334513"
            }
          )
        ]


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
                    [ ( "producer_first_name", JsonBase (StringValue config.firstName) )
                    , ( "producer_last_name", JsonBase (StringValue config.lastName) )
                    , ( "producer_phone", formatPhone config.phone )
                    , ( "producer_email", JsonBase (StringValue config.email) )
                    , ( "business_type", JsonBase (StringValue "new") )
                    , ( "has_other_inforce_policies", JsonBase (BoolValue False) )
                    , ( "deliver_policy_to", JsonBase (StringValue "APP") )
                    , ( "policy_delivery_type", JsonBase (StringValue "paper") )
                    , ( "replacement_notice_copy", JsonBase (BoolValue True) )
                    , ( "Electronic_Combined", JsonBase (BoolValue False) )
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
