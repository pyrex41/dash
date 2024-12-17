module ApplicationView exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)
import Time
import SchemaDecoder as SD exposing (ApplicationWithSchema, FormSchema, FormSection, DependsOn(..), Required(..), JValue(..), FormField, FormFieldType(..), DependsOn, formSchemaDecoder, applicationDecoder, SectionFieldValue, SimpleFieldValue)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode

type alias Model =
    { application : Maybe ApplicationWithSchema
    , formattedData : Maybe FormattedData
    , schema : Maybe FormSchema
    , isLoading : Bool
    , error : Maybe String
    , formValues : Dict String JValue
    , expandedSections : Dict String Bool
    , currentTime : Maybe Time.Posix
    }

type Msg
    = Close
    | GotFormattedData (Result String FormattedData)
    | GotError String
    | CloseModal
    | ToggleSection String
    | UpdateField String String
    | SaveForm
    | NoOp

type alias FormattedData =
    { sections : Dict String (Dict String JValue) }


-- INIT

init : Maybe ApplicationWithSchema -> ( Model, Cmd Msg )
init maybeApplication =
    case maybeApplication of
        Just app ->
            let
                formattedData = formatApplicationData app.data app.schema
                initialFormValues = extractFormValues app.data
            in
            ( { application = Just app
              , formattedData = Just formattedData
              , schema = Just app.schema
              , isLoading = False
              , error = Nothing
              , formValues = initialFormValues
              , expandedSections = Dict.singleton "eligibility" True
              , currentTime = Nothing
              }
            , Cmd.none
            )
        Nothing ->
            ( { application = Nothing
              , formattedData = Nothing
              , schema = Nothing
              , isLoading = False
              , error = Nothing
              , formValues = Dict.empty
              , expandedSections = Dict.empty
              , currentTime = Nothing
              }
            , Cmd.none
            )

-- Helper to extract initial form values from application data
extractFormValues : Decode.Value -> Dict String JValue
extractFormValues jsonData =
    case Decode.decodeValue (Decode.dict Decode.value) jsonData of
        Ok dict ->
            Dict.foldl flattenJsonToDict Dict.empty dict
        Err _ ->
            Dict.empty

flattenJsonToDict : String -> Decode.Value -> Dict String JValue -> Dict String JValue
flattenJsonToDict prefix value dict =
    case Decode.decodeValue Decode.string value of
        Ok str ->
            Dict.insert prefix (SD.StringValue str) dict
        Err _ ->
            case Decode.decodeValue (Decode.dict Decode.value) value of
                Ok nestedDict ->
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
                        nestedDict
                Err _ ->
                    dict

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Close ->
            ( model, Cmd.none )

        GotFormattedData result ->
            case result of
                Ok data ->
                    ( { model | formattedData = Just data, isLoading = False }, Cmd.none )
                Err error ->
                    ( { model | error = Just error, isLoading = False }, Cmd.none )

        GotError error ->
            ( { model | error = Just error, isLoading = False }, Cmd.none )

        CloseModal ->
            ( { model | application = Nothing }, Cmd.none )

        ToggleSection sectionId ->
            let
                isExpanded =
                    Dict.get sectionId model.expandedSections 
                    |> Maybe.withDefault False
                
                newExpandedSections =
                    Dict.insert sectionId (not isExpanded) model.expandedSections
            in
            ( { model | expandedSections = newExpandedSections }, Cmd.none )

        UpdateField fieldId valueString ->
            let
                value = parseValue valueString
            in
            ( { model | formValues = Dict.insert fieldId value model.formValues }
            , Cmd.none
            )

        SaveForm ->
            case model.application of
                Just app ->
                    let
                        payload =
                            Encode.object
                                [ ( "id", Encode.string app.id )
                                , ( "data", encodeFormValues model.formValues )
                                ]
                    in
                    ( model
                    , Http.post
                        { url = "/api/applications/" ++ app.id
                        , body = Http.jsonBody payload
                        , expect = Http.expectJson (Result.mapError httpErrorToString >> GotFormattedData) formattedDataDecoder
                        }
                    )
                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    div [ class "p-6 relative" ]
        [ viewHeader model
        , viewContent model
        ]

viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "flex justify-between items-center mb-4 border-b pb-4" ]
        [ h2 [ class "text-xl font-bold" ] [ text "Application Details" ]
        , button
            [ class "text-gray-500 hover:text-gray-700 text-2xl"
            , onClick CloseModal
            ]
            [ text "×" ]
        ]

viewContent : Model -> Html Msg
viewContent model =
    case ( model.isLoading, model.error, model.schema ) of
        ( True, _, _ ) ->
            div [ class "flex justify-center items-center py-8" ]
                [ div [ class "animate-spin h-8 w-8 border-4 border-purple-600 border-t-transparent rounded-full" ] [] ]

        ( _, Just error, _ ) ->
            div [ class "text-red-500 p-4" ]
                [ text ("Error: " ++ error) ]

        ( _, _, Just schema ) ->
            div [ class "space-y-6" ]
                [ viewForm model schema
                , viewSaveButton
                ]

        _ ->
            div [ class "text-gray-500 p-4" ]
                [ text "Loading application data..." ]

viewForm : Model -> FormSchema -> Html Msg
viewForm model schema =
    div [ class "space-y-6" ]
        (List.sortBy .order schema.sections
            |> List.map (viewSection model)
        )

viewSection : Model -> FormSection -> Html Msg
viewSection model section =
    if isSectionVisible section model.formValues then
        let
            isExpanded =
                Dict.get section.id model.expandedSections
                    |> Maybe.withDefault False
        in
        div [ class "border rounded-lg shadow-sm" ]
            [ div 
                [ class "flex justify-between items-center p-4 cursor-pointer hover:bg-gray-50"
                , onClick (ToggleSection section.id)
                ]
                [ h3 [ class "text-lg font-semibold" ]
                    [ text section.title ]
                , span [ class "text-gray-400" ]
                    [ text (if isExpanded then "▼" else "▶") ]
                ]
            , if isExpanded then
                div [ class "p-4 border-t" ]
                    (List.sortBy .order section.body
                        |> List.map (viewField model section.id)
                    )
              else
                text ""
            ]
    else
        text ""

viewField : Model -> String -> FormField -> Html Msg
viewField model sectionId field =
    if isFieldVisible2 field model.formValues then
        let
            fieldId = sectionId ++ "." ++ field.id
            currentValue = Dict.get fieldId model.formValues |> stringifyMaybeValue
        in
        div [ class "mb-4" ]
            [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                [ text field.displayLabel
                , if isRequired field.required then
                    span [ class "text-red-500 ml-1" ] [ text "*" ]
                  else
                    text ""
                ]
            , viewFieldInput model field fieldId currentValue
            ]
    else
        text ""

viewFieldInput : Model -> FormField -> String -> String -> Html Msg
viewFieldInput model field fieldId currentValue =
    case field.type_ of
        TextField config ->
            input
                [ type_ "text"
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-purple-500 focus:ring-purple-500"
                , value currentValue
                , onInput (UpdateField fieldId)
                , Maybe.map Html.Attributes.maxlength config.maxLength
                    |> Maybe.withDefault (class "")
                ]
                []

        RadioField options ->
            div [ class "mt-1 space-y-2" ]
                (List.map
                    (\option ->
                        case option of  
                            SD.OptionKV kv ->
                                label [ class "inline-flex items-center" ]
                                    [ input
                                        [ type_ "radio"
                                        , name fieldId
                                        , value (SD.stringify kv.value)
                                        , checked (currentValue == SD.stringify kv.value)
                                        , onInput (UpdateField fieldId)
                                        , class "focus:ring-purple-500 h-4 w-4 text-purple-600 border-gray-300"
                                        ]
                                        []
                                    , span [ class "ml-2 text-sm text-gray-700" ]
                                        [ text (SD.stringify kv.key) ]
                                   ]
                            SD.OptionInt i ->
                                text ""
                            
                    )
                    options
                )

        TextNameValueField config ->
            div [ class "mt-1 space-y-2" ]
                [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                    [ text config.displayLabel ]
                , span [ class "text-gray-400" ]
                    [ text config.displayValue ]
                ]

        ComplexDatePickerField ->
            input
                [ type_ "date"
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-purple-500 focus:ring-purple-500"
                , value currentValue
                , onInput (UpdateField fieldId)
                ]
                []

        StringSearchField config ->
            input
                [ type_ "text"
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-purple-500 focus:ring-purple-500"
                , value currentValue
                , onInput (UpdateField fieldId)
                , Maybe.map Html.Attributes.maxlength config.maxLength
                    |> Maybe.withDefault (class "")
                ]
                []

        ComplexPhoneField ->
            input
                [ type_ "tel"
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-purple-500 focus:ring-purple-500"
                , value currentValue
                , onInput (UpdateField fieldId)
                ]
                []

        SimpleEmailField config ->
            input
                [ type_ "email"
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-purple-500 focus:ring-purple-500"
                , value currentValue
                , onInput (UpdateField fieldId)
                , Html.Attributes.maxlength config.maxLength
                ]
                []

        IntPickerField options ->
            select
                [ class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-purple-500 focus:ring-purple-500"
                , value currentValue
                , onInput (UpdateField fieldId)
                ]
                (List.map
                    (\opt ->
                        case opt of
                            SD.OptionInt i ->
                                option
                                    [ value (String.fromInt i) ]
                                    [ text (String.fromInt i) ]
                            SD.OptionKV kv ->
                                text ""
                    )
                    options
                )

        KeyValuePickerField options ->
            select
                [ class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-purple-500 focus:ring-purple-500"
                , value currentValue
                , onInput (UpdateField fieldId)
                ]
                (List.map
                    (\opt ->
                        case opt of
                            SD.OptionKV kv ->
                                option
                                    [ value (SD.stringify kv.value) ]
                                    [ text (SD.stringify kv.key) ]
                            _ -> 
                                text ""
                    )
                    options
                )

        HeightField config ->
            input
                [ type_ "number"
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-purple-500 focus:ring-purple-500"
                , value currentValue
                , onInput (UpdateField fieldId)
                , Html.Attributes.min (String.fromInt config.minimumValue)
                , Html.Attributes.max (String.fromInt config.maximumValue)
                ]
                []

        WeightField config ->
            input
                [ type_ "number"
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-purple-500 focus:ring-purple-500"
                , value currentValue
                , onInput (UpdateField fieldId)
                , Html.Attributes.min (String.fromInt config.minimumValue)
                , Html.Attributes.max (String.fromInt config.maximumValue)
                ]
                []

        HeadingField config ->
            h3 [ class "text-lg font-medium text-gray-900" ]
                [ text currentValue ]

        SSNField config ->
            input
                [ type_ "text"
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-purple-500 focus:ring-purple-500"
                , value currentValue
                , onInput (UpdateField fieldId)
                , Html.Attributes.pattern config.inputMask
                ]
                []

        ComplexRadioField config ->
            div [ class "mt-1 space-y-4" ]
                (List.map
                    (\childField ->
                        viewField model fieldId childField
                    )
                    config.childFields
                )

        TextBlockField config ->
            div [ class (String.join " " config.classes) ]
                [ text currentValue ]

        CheckboxField options ->
            div [ class "mt-1 space-y-2" ]
                (List.map
                    (\opt ->
                        case opt of
                            SD.OptionKV kv ->
                                case kv.value of
                                    SD.TBool b ->
                                        label [ class "inline-flex items-center" ]
                                            [ input
                                                [ type_ "checkbox"
                                                , checked b
                                                , onCheck (\isChecked -> UpdateField fieldId (if isChecked then "true" else "false"))
                                                , class "focus:ring-purple-500 h-4 w-4 text-purple-600 border-gray-300 rounded"
                                                ]
                                                []
                                            , span [ class "ml-2 text-sm text-gray-700" ]
                                                [ text (SD.stringify kv.key) ]
                                            ]
                                    _ ->
                                        text ""
                            SD.OptionInt i ->
                                text ""
                    )
                    options
                )
                
                

        StringPickerField options ->
            select
                [ class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-purple-500 focus:ring-purple-500"
                , value currentValue
                , onInput (UpdateField fieldId)
                ]
                (List.map
                    (\opt ->
                        case opt of
                            SD.OptionKV kv ->
                                option
                                    [ value (SD.stringify kv.value) ]
                                    [ text (SD.stringify kv.key) ]
                            SD.OptionInt i ->
                                option
                                    [ value (String.fromInt i) ]
                                    [ text (String.fromInt i) ]
                    )
                    options
                )

        DrugLookupField config ->
            div []
                [ input
                    [ type_ "text"
                    , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-purple-500 focus:ring-purple-500"
                    , value currentValue
                    , onInput (UpdateField fieldId)
                    ]
                    []
                , if config.isLastFillVisible then
                    div [ class "mt-2 text-sm text-gray-500" ]
                        [ text "Last fill date will be shown" ]
                  else
                    text ""
                ]

        InputTableField config ->
            div [ class "mt-1" ]
                [ div [ class "space-y-4" ]
                    (List.map
                        (\childField ->
                            viewField model fieldId childField
                        )
                        config.childFields
                    )
                , button
                    [ class "mt-2 inline-flex items-center px-3 py-2 border border-transparent text-sm leading-4 font-medium rounded-md text-white bg-purple-600 hover:bg-purple-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-purple-500"
                    ]
                    [ text config.btnDisplayValue ]
                ]

        FileUploadField ->
            input
                [ type_ "file"
                , class "mt-1 block w-full text-sm text-gray-500 file:mr-4 file:py-2 file:px-4 file:rounded-md file:border-0 file:text-sm file:font-semibold file:bg-purple-50 file:text-purple-700 hover:file:bg-purple-100"
                ]
                []

        LinkField config ->
            a
                [ href config.url
                , class "text-purple-600 hover:text-purple-500"
                ]
                [ text currentValue ]

        NoDateDatePickerField ->
            input
                [ type_ "date"
                , class "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-purple-500 focus:ring-purple-500"
                , value currentValue
                , onInput (UpdateField fieldId)
                ]
                []

viewSaveButton : Html Msg
viewSaveButton =
    div [ class "flex justify-end pt-6" ]
        [ button
            [ class "bg-purple-600 text-white px-4 py-2 rounded-md hover:bg-purple-700"
            , onClick SaveForm
            ]
            [ text "Save Changes" ]
        ]

-- HELPERS

isSectionVisible : FormSection -> Dict String JValue -> Bool
isSectionVisible section formValues =
    case section.dependsOn of
        Just dependsOn ->
            checkDependencies dependsOn formValues
        Nothing ->
            True

isFieldVisible2 : FormField -> Dict String JValue -> Bool
isFieldVisible2 field formValues =
    case field.dependsOn of
        Just dependsOn ->
            checkDependencies dependsOn formValues
        Nothing ->
            True

checkDependencies : DependsOn -> Dict String JValue -> Bool
checkDependencies dependsOn formValues =
    case dependsOn of
        SimpleDependsOn deps ->
            case deps.logicOperator of
                "OR" -> List.any (checkSimpleFieldValue formValues) deps.fieldValueList
                "AND" -> List.all (checkSimpleFieldValue formValues) deps.fieldValueList
                _ -> False
                
        SectionDependsOn deps ->
            case deps.logicOperator of
                "OR" -> List.any (checkComplexFieldValue formValues) deps.fieldValueList
                "AND" -> List.all (checkComplexFieldValue formValues) deps.fieldValueList
                _ -> False

checkSimpleFieldValue : Dict String JValue -> SimpleFieldValue -> Bool
checkSimpleFieldValue formValues fieldValue =
    let
        path = fieldValue.sectionId ++ "." ++ fieldValue.id
        actualValue = Dict.get path formValues |> Maybe.withDefault SD.NullValue
    in
    actualValue == fieldValue.value

checkComplexFieldValue : Dict String JValue -> SectionFieldValue -> Bool
checkComplexFieldValue formValues fieldValue =
    let
        path = fieldValue.objectName ++ "." ++ fieldValue.attributeName
        actualValue = Dict.get path formValues |> Maybe.withDefault SD.NullValue
    in
    actualValue == fieldValue.value

encodeFormValues : Dict String JValue -> Encode.Value
encodeFormValues formValues =
    let
        encodeJValue : JValue -> Encode.Value
        encodeJValue jvalue =
            case jvalue of
                SD.StringValue s -> Encode.string s
                SD.IntValue i -> Encode.int i 
                SD.BoolValue b -> Encode.bool b
                SD.NullValue -> Encode.null

        groupedValues =
            Dict.foldl
                (\path value acc ->
                    case String.split "." path of
                        [section, field] ->
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
                )
                Dict.empty
                formValues
    in
    Encode.dict identity 
        (\sectionData -> 
            Encode.dict identity encodeJValue sectionData
        )
        groupedValues

-- DECODERS
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

-- DATA DECODERS


formattedSectionDecoder : Decoder (Dict String JValue)
formattedSectionDecoder =
    Decode.dict jsonValueDecoder
    |> jdebug "formattedSectionDecoder"

-- DECODER FOR FORMATTED DATA
formattedDataDecoder : Decode.Decoder FormattedData
formattedDataDecoder =
    Decode.succeed FormattedData
        |> Pipeline.required "sections" (Decode.dict (Decode.dict jsonValueDecoder))
        |> jdebug "formattedDataDecoder"

jsonValueDecoder : Decode.Decoder JValue
jsonValueDecoder =
    Decode.oneOf
        [ Decode.map SD.IntValue Decode.int
        , Decode.map SD.BoolValue Decode.bool
        , Decode.map SD.StringValue Decode.string
        , Decode.null SD.NullValue
        ]
        |> jdebug "jsonValueDecoder"


-- Decoder Helpers
-- Helper function to convert Value to string


stringifyValue : JValue -> String
stringifyValue value =
    case value of
        SD.StringValue s -> s
        SD.IntValue i -> String.fromInt i
        SD.BoolValue b -> if b then "true" else "false"
        SD.NullValue -> ""

stringifyMaybeValue : Maybe JValue -> String
stringifyMaybeValue maybeValue =
    case maybeValue of
        Just value -> stringifyValue value
        Nothing -> ""

-- Helper function to convert Value to bool
toBool : JValue -> Bool
toBool value =
    case value of
        SD.BoolValue b -> b
        SD.StringValue "true" -> True
        SD.StringValue "1" -> True
        SD.IntValue 1 -> True
        _ -> False

parseValue : String -> JValue
parseValue valueString =
    case valueString |> String.toLower of
        "" -> SD.NullValue
        "true" -> SD.BoolValue True
        "false" -> SD.BoolValue False
        other -> 
            case String.toInt other of
                Just int -> SD.IntValue int
                Nothing -> SD.StringValue other

-- HTTP HELPERS



httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url
        Http.Timeout ->
            "Request timed out"
        Http.NetworkError ->
            "Network error"
        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status
        Http.BadBody message ->
            "Bad body: " ++ message

formatDateForInput : String -> String
formatDateForInput dateStr =
    case String.split "/" dateStr of
        [month, day, year] ->
            String.join "-"
                [ year
                , String.padLeft 2 '0' month
                , String.padLeft 2 '0' day
                ]
        _ ->
            dateStr

formatPhoneNumber : String -> String
formatPhoneNumber phone =
    let
        digits = String.filter Char.isDigit phone
        area = String.slice 0 3 digits
        prefix = String.slice 3 6 digits
        line = String.slice 6 10 digits
    in
    if String.length digits >= 10 then
        "(" ++ area ++ ") " ++ prefix ++ "-" ++ line
    else
        digits

-- DATA FORMATTING

formatApplicationData : Decode.Value -> FormSchema -> FormattedData
formatApplicationData data schema =
    let
        sectionDict =
            List.foldl
                (\section acc ->
                    let
                        fieldDict =
                            List.foldl
                                (\field innerAcc ->
                                    case extractFieldValue data section.id field of
                                        Just val ->
                                            Dict.insert field.id (SD.StringValue val) innerAcc
                                        Nothing ->
                                            innerAcc
                                )
                                Dict.empty
                                section.body
                    in
                    Dict.insert section.id fieldDict acc
                )
                Dict.empty
                schema.sections
    in
    FormattedData sectionDict

extractFieldValue : Decode.Value -> String -> FormField -> Maybe String
extractFieldValue data sectionId field =
    let
        path = sectionId ++ "." ++ field.id
        decoder = 
            Decode.at 
                (String.split "." path)
                Decode.string
    in
    case Decode.decodeValue decoder data of
        Ok value ->
            Just value
        Err _ ->
            Nothing



isRequired : Required -> Bool
isRequired required =
    case required of
        SD.RequiredBool bool ->
            bool
        SD.RequiredDependsOn dependsOn ->
            True  -- If it depends on something, we'll handle the visibility separately