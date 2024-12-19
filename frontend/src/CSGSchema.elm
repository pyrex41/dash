module CSGSchema exposing (..)

import Dict
import Html exposing (Html, div, label, span, text)
import Html.Attributes exposing (class, id, name, rel, selected, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Time


type JsonValue
    = JsonString String
    | JsonInt Int
    | JsonFloat Float
    | JsonBool Bool
    | JsonNull
    | JsonArray (List JsonValue)
    | JsonObject (Dict.Dict String JsonValue)


type JValue
    = StringValue String
    | IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | NullValue


parseValue : String -> JValue
parseValue valueString =
    case String.toInt valueString of
        Just n ->
            IntValue n

        Nothing ->
            case String.toFloat valueString of
                Just f ->
                    FloatValue f

                Nothing ->
                    case String.toLower valueString of
                        "true" ->
                            BoolValue True

                        "false" ->
                            BoolValue False

                        "null" ->
                            NullValue

                        "" ->
                            NullValue

                        _ ->
                            StringValue valueString


jsonValueDecoder : Decoder JsonValue
jsonValueDecoder =
    Decode.oneOf
        [ Decode.string |> Decode.map JsonString
        , Decode.int |> Decode.map JsonInt
        , Decode.float |> Decode.map JsonFloat
        , Decode.bool |> Decode.map JsonBool
        , Decode.null JsonNull
        , Decode.list (Decode.lazy (\_ -> jsonValueDecoder)) |> Decode.map JsonArray
        , Decode.dict (Decode.lazy (\_ -> jsonValueDecoder)) |> Decode.map JsonObject
        ]


flattenJsonToDict : String -> JsonValue -> Dict.Dict String JValue -> Dict.Dict String JValue
flattenJsonToDict prefix value dict =
    case value of
        JsonString str ->
            Dict.insert prefix (StringValue str) dict

        JsonInt n ->
            Dict.insert prefix (IntValue n) dict

        JsonFloat n ->
            Dict.insert prefix (FloatValue n) dict

        JsonBool b ->
            Dict.insert prefix
                (if b then
                    BoolValue True

                 else
                    BoolValue False
                )
                dict

        JsonObject obj ->
            Dict.foldl
                (\k v acc ->
                    let
                        newPrefix =
                            if String.isEmpty prefix then
                                k

                            else
                                prefix ++ "." ++ k
                    in
                    flattenJsonToDict newPrefix v acc
                )
                dict
                obj

        _ ->
            dict


encodeFormValues : Dict.Dict String JValue -> Decode.Value
encodeFormValues flatData =
    let
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
    in
    Dict.toList flatData
        |> List.map (\( key, value ) -> ( key, encodeJValue value ))
        |> Encode.object



-- New types for the enhanced form structure


type alias ApplicationSchema =
    List FormSection


type alias FormSection =
    { id : String
    , order : Int
    , heading : String
    , title : String
    , body : List FormField
    , dependsOn : Maybe MultiDependsOn
    }


type MultiDependsOn
    = DependsOnType DependsOn
    | SectionDependsOnType SectionDependsOn


type alias FormField =
    { id : String
    , tag : String
    , displayLabel : String
    , fieldType : FormFieldType
    , required : RequiredType
    , dependsOn : Maybe DependsOn
    , order : Int
    }


type FormFieldType
    = RadioField (List RadioOption)
    | ComplexDatePickerField
    | NoDateDatePickerField
    | TextField { maxLength : Maybe Int }
    | StringSearchField
        { maxLength : Maybe Int
        , childFields : List FormField
        , path : String
        , searchDataColumn : Maybe String
        , prepopulate : Maybe Bool
        }
    | ComplexPhoneField
    | SimpleEmailField { maxLength : Int }
    | IntPickerField (List Int)
    | KeyValuePickerField (List KeyValueOption)
    | HeightField
        { minimumValue : Int
        , maximumValue : Int
        , displayType : String
        , valueType : String
        }
    | WeightField
        { minimumValue : Int
        , maximumValue : Int
        }
    | HeadingField { headingType : String }
    | SSNField
        { inputMask : String
        , submitUnmasked : Bool
        }
    | ComplexRadioField { childFields : List FormField }
    | TextBlockField { classes : List String }
    | CheckboxField (List CheckboxOption1) (List CheckboxOption2)
    | StringPickerField (List StringPickerOption)
    | DrugLookupField { isLastFillVisible : Bool }
    | InputTableField
        { childFields : List FormField
        , minimumNumber : Int
        , maximumNumber : Int
        , btnDisplayValue : String
        , displayNameField : String
        }
    | FileUploadField
    | LinkField { url : String }
    | TextNameValueField
        { displayLabel : String
        , displayValue : String
        , id : String
        }


type alias DependsOn =
    { logicOperator : String
    , fieldValueList : List FieldValue
    }


type RequiredType
    = RequiredBool Bool
    | RequiredDependsOn DependsOn


type alias SectionDependsOn =
    { fieldValueList : List SectionFieldValue
    , logicOperator : String
    }


type alias SectionFieldValue =
    { objectName : String
    , attributeName : String
    , value : JsonValue
    }


type alias FieldValue =
    { id : String
    , sectionId : String
    , value : JsonValue
    }


type alias RadioOption =
    { value : JValue
    , key : String
    }


type alias KeyValueOption =
    { value : JValue
    , key : String
    }


type alias CheckboxOption1 =
    { value : Bool
    , key : String
    , id : String
    }


type alias CheckboxOption2 =
    { value : JValue
    , key : String
    }


type alias StringPickerOption =
    { value : JValue
    , key : String
    }



-- Decoders


jdebug : String -> Decode.Decoder a -> Decode.Decoder a
jdebug message decoder =
    Decode.value
        |> Decode.andThen (debugHelper message decoder)


debugHelper : String -> Decode.Decoder a -> Decode.Value -> Decode.Decoder a
debugHelper message decoder value =
    let
        _ =
            Debug.log message (Decode.decodeValue decoder value)
    in
    decoder


formSchemaDecoder : Decoder ApplicationSchema
formSchemaDecoder =
    Decode.list formSectionDecoder


formSectionDecoder : Decoder FormSection
formSectionDecoder =
    Decode.succeed FormSection
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "order" Decode.int
        |> Pipeline.required "heading" Decode.string
        |> Pipeline.required "title" Decode.string
        |> Pipeline.required "body" (Decode.list formField2Decoder)
        |> Pipeline.optional "depends_on"
            (Decode.oneOf
                [ Decode.map (Just << SectionDependsOnType) sectionDependsOnDecoder
                , Decode.map (Just << DependsOnType) dependsOnDecoder
                , Decode.null Nothing
                ]
            )
            Nothing


sectionDependsOnDecoder : Decoder SectionDependsOn
sectionDependsOnDecoder =
    Decode.succeed SectionDependsOn
        |> Pipeline.required "field_value_list" (Decode.list sectionFieldValueDecoder)
        |> Pipeline.required "logic_operator" Decode.string


sectionFieldValueDecoder : Decoder SectionFieldValue
sectionFieldValueDecoder =
    Decode.succeed SectionFieldValue
        |> Pipeline.required "object_name" Decode.string
        |> Pipeline.required "attribute_name" Decode.string
        |> Pipeline.required "value" jsonValueDecoder


formField2Decoder : Decoder FormField
formField2Decoder =
    Decode.succeed FormField
        |> Pipeline.optional "id" Decode.string "NA"
        |> Pipeline.optional "tag" Decode.string ""
        |> Pipeline.optional "display_label" Decode.string ""
        |> Pipeline.custom fieldTypeDecoder
        |> Pipeline.optional "required"
            (Decode.oneOf
                [ -- Try to decode as a boolean first
                  Decode.bool |> Decode.map RequiredBool
                , -- Then try to decode as a depends_on structure
                  Decode.field "depends_on" dependsOnDecoder |> Decode.map RequiredDependsOn
                , -- Finally, default to not required if field is missing or null
                  Decode.succeed (RequiredBool False)
                ]
            )
            (RequiredBool False)
        |> Pipeline.optional "depends_on" (Decode.map Just dependsOnDecoder) Nothing
        |> Pipeline.required "order" Decode.int


fieldTypeDecoder : Decoder FormFieldType
fieldTypeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen decodeFieldType


decodeFieldType : String -> Decoder FormFieldType
decodeFieldType typeStr =
    case typeStr of
        "text_name_value_field" ->
            Decode.map3
                (\displayLabel displayValue id ->
                    TextNameValueField
                        { displayLabel = displayLabel
                        , displayValue = displayValue
                        , id = id
                        }
                )
                (Decode.field "display_label" Decode.string)
                (Decode.field "display_value" Decode.string)
                (Decode.field "id" Decode.string)

        "no_date_date_picker_field" ->
            Decode.succeed NoDateDatePickerField

        "radio_field" ->
            Decode.field "options" (Decode.list radioOptionDecoder)
                |> Decode.map RadioField

        "radio_field_no_tiles" ->
            Decode.map RadioField <|
                Decode.field "options" (Decode.list radioOptionDecoder)

        "complex_date_picker_field" ->
            Decode.succeed ComplexDatePickerField

        "text_field" ->
            Decode.succeed (\maxLength -> TextField { maxLength = maxLength })
                |> Pipeline.optional "maxlength" (Decode.map Just Decode.int) Nothing

        "string_search_field" ->
            Decode.succeed
                (\maxLength childFields path searchDataColumn ->
                    StringSearchField
                        { maxLength = maxLength
                        , childFields = Maybe.withDefault [] childFields
                        , path = Maybe.withDefault "" path
                        , searchDataColumn = searchDataColumn
                        , prepopulate = Nothing
                        }
                )
                |> Pipeline.optional "maxlength" (Decode.nullable Decode.int) Nothing
                |> Pipeline.optional "child_fields" (Decode.nullable (Decode.list (Decode.lazy (\_ -> formField2Decoder)))) Nothing
                |> Pipeline.optional "path" (Decode.nullable Decode.string) Nothing
                |> Pipeline.optional "search_data_column" (Decode.nullable Decode.string) Nothing
                |> Decode.andThen
                    (\field ->
                        Decode.maybe (Decode.field "prepopulate" Decode.bool)
                            |> Decode.map
                                (\prepopulate ->
                                    case field of
                                        StringSearchField config ->
                                            StringSearchField { config | prepopulate = prepopulate }

                                        _ ->
                                            field
                                )
                    )

        "complex_phone_field" ->
            Decode.succeed ComplexPhoneField

        "simple_email_field" ->
            Decode.oneOf
                [ -- Try to decode with maxlength field
                  Decode.field "maxlength" Decode.int
                    |> Decode.map (\maxLength -> SimpleEmailField { maxLength = maxLength })
                , -- Default to a reasonable maxlength if field is missing
                  Decode.succeed (SimpleEmailField { maxLength = 254 })
                ]

        "int_picker_field" ->
            Decode.map IntPickerField
                (Decode.field "options" (Decode.list Decode.int))

        "key_value_picker_field" ->
            Decode.map KeyValuePickerField
                (Decode.field "options" (Decode.list keyValueOptionDecoder))

        "height_field" ->
            Decode.map4
                (\min max displayType valueType ->
                    HeightField
                        { minimumValue = min
                        , maximumValue = max
                        , displayType = displayType
                        , valueType = valueType
                        }
                )
                (Decode.field "minimum_value" Decode.int)
                (Decode.field "maximum_value" Decode.int)
                (Decode.field "display_type" Decode.string)
                (Decode.field "value_type" Decode.string)

        "weight_field" ->
            Decode.map2
                (\min max ->
                    WeightField
                        { minimumValue = min
                        , maximumValue = max
                        }
                )
                (Decode.field "minimum_value" Decode.int)
                (Decode.field "maximum_value" Decode.int)

        "heading_field" ->
            Decode.map (\headingType -> HeadingField { headingType = headingType })
                (Decode.field "heading_type" Decode.string)

        "ssn_field" ->
            Decode.map2
                (\mask unmasked ->
                    SSNField
                        { inputMask = mask
                        , submitUnmasked = unmasked
                        }
                )
                (Decode.field "input_mask" Decode.string)
                (Decode.field "submit_unmasked" Decode.bool)

        "complex_radio_field" ->
            Decode.map
                (\children ->
                    ComplexRadioField
                        { childFields = children }
                )
                (Decode.field "child_fields" (Decode.list (Decode.lazy (\_ -> formField2Decoder))))

        "text_block_field" ->
            Decode.map
                (\classes ->
                    TextBlockField
                        { classes = Maybe.withDefault [] classes }
                )
                (Decode.oneOf
                    [ Decode.field "classes" (Decode.list Decode.string)
                    , Decode.succeed []
                    ]
                    |> Decode.map Just
                )

        "checkbox_field" ->
            Decode.map2 CheckboxField
                (Decode.oneOf
                    [ Decode.field "options" (Decode.list checkboxOptionDecoder)
                    , Decode.succeed []
                    ]
                )
                (Decode.oneOf
                    [ Decode.field "options" (Decode.list checkboxOptionDecoder2)
                    , Decode.succeed []
                    ]
                )

        "string_picker_field" ->
            Decode.map StringPickerField
                (Decode.field "options" (Decode.list stringPickerOptionDecoder))

        "drug_lookup_field" ->
            Decode.map
                (\isLastFillVisible ->
                    DrugLookupField
                        { isLastFillVisible = isLastFillVisible }
                )
                (Decode.field "is_last_fill_visible" Decode.bool)

        "input_table_field" ->
            Decode.map5
                (\childFields minNum maxNum btnDisplay displayName ->
                    InputTableField
                        { childFields = childFields
                        , minimumNumber = minNum
                        , maximumNumber = maxNum
                        , btnDisplayValue = btnDisplay
                        , displayNameField = displayName
                        }
                )
                (Decode.field "child_fields" (Decode.list (Decode.lazy (\_ -> formField2Decoder))))
                (Decode.oneOf
                    [ Decode.field "minimum_number" Decode.int
                    , Decode.succeed 0 -- Default value if field is missing
                    ]
                )
                (Decode.field "maximum_number" Decode.int)
                (Decode.field "btn_display_value" Decode.string)
                (Decode.field "display_name_field" Decode.string)

        "file_upload_field" ->
            Decode.succeed FileUploadField

        "link_field" ->
            Decode.map (\url -> LinkField { url = url })
                (Decode.field "url" Decode.string)

        _ ->
            Decode.fail ("Unknown field type: " ++ typeStr)


dependsOnDecoder : Decoder DependsOn
dependsOnDecoder =
    Decode.succeed DependsOn
        |> Pipeline.required "logic_operator" Decode.string
        |> Pipeline.required "field_value_list" (Decode.list fieldValueDecoder)


fieldValueDecoder : Decoder FieldValue
fieldValueDecoder =
    Decode.succeed FieldValue
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "section_id" Decode.string
        |> Pipeline.required "value" jsonValueDecoder


radioOptionDecoder : Decoder RadioOption
radioOptionDecoder =
    Decode.succeed RadioOption
        |> Pipeline.custom
            (Decode.field "value" jValueDecoder)
        |> Pipeline.custom
            (Decode.field "key"
                (Decode.oneOf
                    [ Decode.string
                    , Decode.int |> Decode.map String.fromInt
                    ]
                )
            )


keyValueOptionDecoder : Decoder KeyValueOption
keyValueOptionDecoder =
    let
        valueDecoder =
            jValueDecoder
                |> Decode.andThen
                    (\value ->
                        case value of
                            NullValue ->
                                Decode.succeed NullValue

                            _ ->
                                Decode.succeed value
                    )
    in
    Decode.succeed KeyValueOption
        |> Pipeline.optional "value" valueDecoder NullValue
        |> Pipeline.custom
            (Decode.field "key"
                (Decode.oneOf
                    [ Decode.string
                    , Decode.int |> Decode.map String.fromInt
                    ]
                )
            )


checkboxOptionDecoder : Decoder CheckboxOption1
checkboxOptionDecoder =
    Decode.succeed CheckboxOption1
        |> Pipeline.required "value" Decode.bool
        |> Pipeline.custom
            (Decode.field "key"
                (Decode.oneOf
                    [ Decode.string
                    , Decode.int |> Decode.map String.fromInt
                    ]
                )
            )
        |> Pipeline.required "id" Decode.string


checkboxOptionDecoder2 : Decoder CheckboxOption2
checkboxOptionDecoder2 =
    Decode.succeed CheckboxOption2
        |> Pipeline.required "value" jValueDecoder
        |> Pipeline.custom
            (Decode.field "key"
                (Decode.oneOf
                    [ Decode.string
                    , Decode.int |> Decode.map String.fromInt
                    ]
                )
            )


stringPickerOptionDecoder : Decoder StringPickerOption
stringPickerOptionDecoder =
    Decode.succeed StringPickerOption
        |> Pipeline.custom
            (Decode.field "value" jValueDecoder)
        |> Pipeline.custom
            (Decode.field "key"
                (Decode.oneOf
                    [ Decode.string
                    , Decode.int |> Decode.map String.fromInt
                    ]
                )
            )


jValueDecoder : Decoder JValue
jValueDecoder =
    Decode.oneOf
        [ Decode.string |> Decode.map StringValue
        , Decode.int |> Decode.map IntValue
        , Decode.bool |> Decode.map BoolValue
        , Decode.null NullValue
        ]


calculateAge : Maybe JValue -> Maybe Time.Posix -> Maybe Int
calculateAge maybeDobJValue maybeCurrentTime =
    case ( maybeDobJValue, maybeCurrentTime ) of
        ( Just jvalue, Just currentTime ) ->
            case jvalue of
                StringValue dobString ->
                    case String.split "-" dobString of
                        [ yearStr, monthStr, dayStr ] ->
                            case String.toInt yearStr of
                                Just birthYear ->
                                    let
                                        currentYear =
                                            Time.toYear Time.utc currentTime

                                        birthMonth =
                                            String.toInt monthStr |> Maybe.withDefault 1

                                        currentMonth =
                                            Time.toMonth Time.utc currentTime |> monthToInt

                                        birthDay =
                                            String.toInt dayStr |> Maybe.withDefault 1

                                        currentDay =
                                            Time.toDay Time.utc currentTime

                                        age =
                                            if
                                                currentMonth
                                                    > birthMonth
                                                    || (currentMonth == birthMonth && currentDay >= birthDay)
                                            then
                                                currentYear - birthYear

                                            else
                                                currentYear - birthYear - 1
                                    in
                                    Just age

                                Nothing ->
                                    Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing



-- Helper function to convert Month to Int


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12



-- Add this helper function to check dependencies


type alias FlatValues =
    Dict.Dict String JValue


isFieldVisible : FormField -> FormSection -> FlatValues -> Bool
isFieldVisible field section formValues =
    let
        -- Check field dependencies if they exist
        dependenciesSatisfied =
            case field.required of
                RequiredDependsOn dependsOn ->
                    checkDependencies dependsOn formValues

                RequiredBool _ ->
                    case field.dependsOn of
                        Just dependsOn ->
                            checkDependencies dependsOn formValues

                        Nothing ->
                            True

        -- Check section dependencies if they exist
        sectionVisible =
            case section.dependsOn of
                Just (DependsOnType dependsOn) ->
                    checkDependencies dependsOn formValues

                Just (SectionDependsOnType sectionDependsOn) ->
                    checkAttributeDependencies sectionDependsOn formValues

                Nothing ->
                    True
    in
    dependenciesSatisfied && sectionVisible


checkSectionDependencies : MultiDependsOn -> FlatValues -> Bool
checkSectionDependencies dependsOn flattenedData =
    case dependsOn of
        DependsOnType dependsOnNormal ->
            checkDependencies dependsOnNormal flattenedData

        SectionDependsOnType sectionDependsOn ->
            checkAttributeDependencies sectionDependsOn flattenedData


checkAttributeDependencies : SectionDependsOn -> FlatValues -> Bool
checkAttributeDependencies sectionDependsOn flattenedData =
    case sectionDependsOn.logicOperator of
        "OR" ->
            List.any
                (\fieldValue ->
                    let
                        path =
                            fieldValue.objectName ++ "." ++ fieldValue.attributeName

                        actualValue =
                            Dict.get path flattenedData
                                |> Maybe.withDefault NullValue

                        expectedValue =
                            case fieldValue.value of
                                JsonString str ->
                                    StringValue str

                                JsonInt n ->
                                    IntValue n

                                JsonFloat n ->
                                    FloatValue n

                                JsonBool b ->
                                    if b then
                                        BoolValue True

                                    else
                                        BoolValue False

                                _ ->
                                    NullValue
                    in
                    actualValue == expectedValue
                )
                sectionDependsOn.fieldValueList

        "AND" ->
            List.all
                (\fieldValue ->
                    let
                        path =
                            fieldValue.objectName ++ "." ++ fieldValue.attributeName

                        actualValue =
                            Dict.get path flattenedData
                                |> Maybe.withDefault NullValue

                        expectedValue =
                            case fieldValue.value of
                                JsonString str ->
                                    StringValue str

                                JsonInt n ->
                                    IntValue n

                                JsonFloat n ->
                                    FloatValue n

                                JsonBool b ->
                                    if b then
                                        BoolValue True

                                    else
                                        BoolValue False

                                _ ->
                                    NullValue
                    in
                    actualValue == expectedValue
                )
                sectionDependsOn.fieldValueList

        _ ->
            False


checkDependencies : DependsOn -> FlatValues -> Bool
checkDependencies dependsOn flattenedData =
    case dependsOn.logicOperator of
        "OR" ->
            List.any
                (\fieldValue ->
                    let
                        -- Build the full path by combining section and field IDs
                        path =
                            fieldValue.sectionId ++ "." ++ fieldValue.id

                        actualValue =
                            Dict.get path flattenedData
                                |> Maybe.withDefault NullValue

                        expectedValue =
                            case fieldValue.value of
                                JsonString str ->
                                    StringValue str

                                JsonInt n ->
                                    IntValue n

                                JsonBool b ->
                                    if b then
                                        BoolValue True

                                    else
                                        BoolValue False

                                JsonFloat n ->
                                    FloatValue n

                                JsonNull ->
                                    NullValue

                                _ ->
                                    NullValue
                    in
                    actualValue == expectedValue
                )
                dependsOn.fieldValueList

        "AND" ->
            List.all
                (\fieldValue ->
                    let
                        path =
                            fieldValue.sectionId ++ "." ++ fieldValue.id

                        actualValue =
                            Dict.get path flattenedData
                                |> Maybe.withDefault NullValue

                        expectedValue =
                            case fieldValue.value of
                                JsonString str ->
                                    StringValue str

                                JsonInt n ->
                                    IntValue n

                                JsonBool b ->
                                    if b then
                                        BoolValue True

                                    else
                                        BoolValue False

                                JsonFloat n ->
                                    FloatValue n

                                _ ->
                                    NullValue
                    in
                    actualValue == expectedValue
                )
                dependsOn.fieldValueList

        _ ->
            False


formatDateForInput : String -> String
formatDateForInput dateStr =
    case String.split "/" dateStr of
        [ month, day, year ] ->
            -- Convert from MM/DD/YYYY to YYYY-MM-DD (HTML date input format)
            String.join "-"
                [ year
                , String.padLeft 2 '0' month
                , String.padLeft 2 '0' day
                ]

        _ ->
            dateStr


formatSSN : String -> String
formatSSN ssn =
    let
        digits =
            String.filter Char.isDigit ssn
    in
    case ( String.left 3 digits, String.slice 3 5 digits, String.right 4 digits ) of
        ( area, group, serial ) ->
            if String.length digits >= 9 then
                area ++ "-" ++ group ++ "-" ++ serial

            else
                digits



-- Add these helper functions


formatPhoneNumber : String -> String
formatPhoneNumber phone =
    let
        rawValue =
            String.filter Char.isDigit phone

        formattedValue =
            String.join "-"
                [ String.slice 0 3 rawValue
                , String.slice 3 6 rawValue
                , String.slice 6 10 rawValue
                ]
    in
    formattedValue
