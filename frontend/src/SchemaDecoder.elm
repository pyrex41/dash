module SchemaDecoder exposing (formSchemaDecoder, applicationDecoder, FormSchema, FormSection, stringify, TValue(..), Option(..), DependsOn(..), SimpleFieldValue, SectionFieldValue, FormField, Required(..), FormFieldType(..), ApplicationWithSchema, JValue(..))

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline

-- Put here all the types that deal with schema
type alias ApplicationWithSchema =
    { id : String
    , data : Decode.Value
    , schema : FormSchema
    }

type alias FormSchema =
    { sections : List FormSection }

type alias FormSection =
    { id : String
    , order : Int
    , heading : String
    , title : String
    , body : List FormField
    , dependsOn : Maybe DependsOn
    }

type DependsOn
    = SimpleDependsOn
        { logicOperator : String
        , fieldValueList : List SimpleFieldValue
        }
    | SectionDependsOn
        { fieldValueList : List SectionFieldValue
        , logicOperator : String
        }

type alias SimpleFieldValue =
    { id : String
    , sectionId : String
    , value : JValue
    }

type alias SectionFieldValue =
    { objectName : String
    , attributeName : String
    , value : JValue
    }

type alias FormField =
    { id : String
    , tag : String
    , displayLabel : String
    , type_ : FormFieldType
    , required : Required
    , options : Maybe (List Option)
    , maxlength : Maybe Int
    , dependsOn : Maybe DependsOn
    , order : Int
    }

type Required
    = RequiredBool Bool
    | RequiredDependsOn DependsOn

type JValue
    = StringValue String
    | IntValue Int
    | BoolValue Bool
    | NullValue

type TValue
    = TString String
    | TInt Int
    | TBool Bool

stringify : TValue -> String
stringify = \value ->
    case value of
        TString s -> s
        TInt i -> String.fromInt i
        TBool b -> 
            if b then "true" else "false"

type Option
    = OptionKV KVOption
    | OptionInt Int

type alias KVOption = 
    { key : TValue
    , value: TValue
    , id : Maybe String
    }

type FormFieldType
    = NoDateDatePickerField
    | RadioField (List Option)
    | ComplexDatePickerField
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
    | IntPickerField (List Option)
    | KeyValuePickerField (List Option)
    | HeightField { minimumValue : Int, maximumValue : Int, displayType : String, valueType : String }
    | WeightField { minimumValue : Int, maximumValue : Int }
    | HeadingField { headingType : String }
    | SSNField { inputMask : String, submitUnmasked : Bool }
    | ComplexRadioField { childFields : List FormField }
    | TextBlockField { classes : List String }
    | CheckboxField (List Option)
    | StringPickerField (List Option)
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
    | TextNameValueField { displayLabel : String, displayValue : String, id : String }

-- DECODERS HERE

jdebug : String -> Decode.Decoder a -> Decode.Decoder a
jdebug message decoder =
    Decode.value
        |> Decode.andThen (\val ->
            let
                _ = Debug.log message (Decode.decodeValue decoder val)
            in
            decoder
        )

formSchemaDecoder : Decode.Decoder FormSchema
formSchemaDecoder =
    Decode.succeed FormSchema
        |> Pipeline.required "sections" (Decode.list formSectionDecoder)
        |> jdebug "formSchemaDecoder"

formSectionDecoder : Decode.Decoder FormSection
formSectionDecoder =
    Decode.succeed FormSection
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "order" Decode.int
        |> Pipeline.required "heading" Decode.string
        |> Pipeline.required "title" Decode.string
        |> Pipeline.required "body" (Decode.list formFieldDecoder)
        |> Pipeline.optional "depends_on"
            (Decode.oneOf
                [ Decode.map (Just << SectionDependsOn) sectionDependsOnDecoder
                , Decode.map (Just << SimpleDependsOn) simpleDependsOnDecoder
                , Decode.null Nothing
                ]
            )
            Nothing
        |> jdebug "formSectionDecoder"

dependsOnDecoder : Decode.Decoder DependsOn
dependsOnDecoder =
    Decode.oneOf
        [ Decode.map SectionDependsOn sectionDependsOnDecoder
        , Decode.map SimpleDependsOn simpleDependsOnDecoder
        ]
        |> jdebug "dependsOnDecoder"

simpleDependsOnDecoder : Decode.Decoder { logicOperator : String, fieldValueList : List SimpleFieldValue }
simpleDependsOnDecoder =
    Decode.succeed (\op list -> { logicOperator = op, fieldValueList = list })
        |> Pipeline.required "logic_operator" Decode.string
        |> Pipeline.required "field_value_list" (Decode.list simpleFieldValueDecoder)
        |> jdebug "simpleDependsOnDecoder"

simpleFieldValueDecoder : Decode.Decoder SimpleFieldValue
simpleFieldValueDecoder =
    Decode.succeed SimpleFieldValue
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "section_id" Decode.string
        |> Pipeline.required "value" jsonValueDecoder
        |> jdebug "simpleFieldValueDecoder"

sectionDependsOnDecoder : Decode.Decoder { fieldValueList : List SectionFieldValue, logicOperator : String }
sectionDependsOnDecoder =
    Decode.succeed (\list op -> { fieldValueList = list, logicOperator = op })
        |> Pipeline.required "field_value_list" (Decode.list sectionFieldValueDecoder)
        |> Pipeline.required "logic_operator" Decode.string
        |> jdebug "sectionDependsOnDecoder"

sectionFieldValueDecoder : Decode.Decoder SectionFieldValue
sectionFieldValueDecoder =
    Decode.succeed SectionFieldValue
        |> Pipeline.required "object_name" Decode.string
        |> Pipeline.required "attribute_name" Decode.string
        |> Pipeline.required "value" jsonValueDecoder
        |> jdebug "sectionFieldValueDecoder"

formFieldDecoder : Decode.Decoder FormField
formFieldDecoder =
    Decode.succeed FormField
        |> Pipeline.optional "id" Decode.string "NA"
        |> Pipeline.optional "tag" Decode.string ""
        |> Pipeline.optional "display_label" Decode.string ""
        |> Pipeline.custom fieldTypeDecoder
        |> Pipeline.optional "required"
            (Decode.oneOf
                [ Decode.bool |> Decode.map RequiredBool
                , Decode.field "depends_on" dependsOnDecoder |> Decode.map RequiredDependsOn
                , Decode.succeed (RequiredBool False)
                ]
            )
            (RequiredBool False)
        |> Pipeline.optional "options" (Decode.nullable (Decode.list optionDecoder)) Nothing
        |> Pipeline.optional "maxlength" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "depends_on" (Decode.nullable dependsOnDecoder) Nothing
        |> Pipeline.required "order" Decode.int
        |> jdebug "formFieldDecoder"


fieldTypeDecoder : Decode.Decoder FormFieldType
fieldTypeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen decodeFieldType
        |> jdebug "fieldTypeDecoder"

-- The decodeFieldType, optionDecoder, jsonValueDecoder, etc. 
-- would be similar to what you had before. Move them here.

jsonValueDecoder : Decode.Decoder JValue
jsonValueDecoder =
    Decode.oneOf
        [ Decode.map IntValue Decode.int
        , Decode.map BoolValue Decode.bool
        , Decode.map StringValue Decode.string
        , Decode.null NullValue
        ]
        |> jdebug "jsonValueDecoder"

decodeFieldType : String -> Decode.Decoder FormFieldType
decodeFieldType typeStr =
    jdebug "decodeFieldType" <| 
    case typeStr of
        "no_date_date_picker_field" ->
            Decode.succeed NoDateDatePickerField

        "radio_field" ->
            Decode.field "options" (Decode.list optionDecoder)
                |> Decode.map RadioField

        "radio_field_no_tiles" ->
            Decode.map RadioField <|
                Decode.field "options" (Decode.list optionDecoder)

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
                |> Pipeline.optional "child_fields" (Decode.nullable (Decode.list (Decode.lazy (\_ -> formFieldDecoder)))) Nothing
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
                (Decode.field "options" (Decode.list optionDecoder))

        "key_value_picker_field" ->
            Decode.map KeyValuePickerField
                (Decode.field "options" (Decode.list optionDecoder))

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
                (Decode.field "child_fields" (Decode.list (Decode.lazy (\_ -> formFieldDecoder))))

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
            Decode.map CheckboxField
                (Decode.oneOf
                    [ Decode.field "options" (Decode.list optionDecoder)
                    , Decode.succeed []
                    ]
                )

        "string_picker_field" ->
            Decode.map StringPickerField
                (Decode.field "options" (Decode.list optionDecoder))

        "drug_lookup_field" ->
            Decode.map
                (\isLastFillVisible ->
                    DrugLookupField
                        { isLastFillVisible = isLastFillVisible }
                )
                (Decode.field "is_last_fill_visible" Decode.bool)
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
                (Decode.field "child_fields" (Decode.list (Decode.lazy (\_ -> formFieldDecoder))))
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

optionDecoder : Decode.Decoder Option
optionDecoder =
    Decode.oneOf
        [ Decode.map OptionKV optionKVDecoder
        , Decode.map OptionInt Decode.int
        , Decode.succeed (OptionKV { key = TString "NA", value = TString "NA", id = Nothing })
        ]

optionKVDecoder : Decode.Decoder KVOption
optionKVDecoder =
    Decode.succeed KVOption
        |> Pipeline.required "key" tValueDecoder
        |> Pipeline.required "value" tValueDecoder
        |> Pipeline.optional "id" (Decode.nullable Decode.string) Nothing

tValueDecoder : Decode.Decoder TValue
tValueDecoder =
    Decode.oneOf
        [ Decode.map TString Decode.string
        , Decode.map TInt Decode.int
        , Decode.map TBool Decode.bool
        ]


-- Finally, decode the entire ApplicationWithSchema
applicationDecoder : Decode.Decoder ApplicationWithSchema
applicationDecoder =
    Decode.succeed ApplicationWithSchema
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "data" Decode.value
        |> Pipeline.required "schema" formSchemaDecoder
        |> jdebug "applicationDecoder"
