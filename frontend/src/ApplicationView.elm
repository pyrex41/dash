port module ApplicationView exposing (Application, Model, Msg(..), applicationViewDecoder, init, subscriptions, update, view)

import CSGSchema exposing (ApplicationSchema, FormField, FormFieldType(..), FormSection, JValue(..), JsonValue(..), RequiredType(..), isFieldVisible, jsonValueDecoder, parseValue, unwrapJValue)
import DataEncoder exposing (unflattenData)
import Date exposing (Date, Unit(..))
import Debug
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Task
import Time exposing (Month(..))



-- Port for saving application data


port saveApplication : { id : String, data : Encode.Value } -> Cmd msg



-- Port for receiving save response


port saveApplicationResponse : ({ success : Bool, error : Maybe String } -> msg) -> Sub msg


type alias Model =
    { data : JsonValue
    , naic : String
    , carrier : Maybe Carrier
    , medications : List Medication
    , medicationForm : Dict String String
    , schema : ApplicationSchema
    , id : String
    , error : Maybe String
    , expandedSections : Dict String Bool
    , currentDate : Maybe Date
    , showDebugFields : Bool
    , isValid : Bool
    , underwritingType : Maybe Int
    }


type Msg
    = GotError String
    | ToggleSection String
    | UpdateField String String String
    | UpdateComplexPhoneField String String JsonValue
    | SaveForm
    | SaveFormResponse { success : Bool, error : Maybe String }
    | GotCurrentTime Date
    | SubmitToCSG
    | NoOp
    | GotRoutingNumber (Result Http.Error String)
    | UpdateMedicationField String String String
    | SaveMedication String Medication
    | CancelAddMedication String
    | RemoveMedication String Int


type alias Application =
    { id : String
    , naic : String
    , data : Decode.Value
    , schema : ApplicationSchema
    }



-- INIT


init : Application -> ( Model, Cmd Msg )
init app =
    let
        initialFormValues =
            app.data
                |> extractFormValues
                |> Debug.log "initialFormValues"

        initialMedications =
            case Decode.decodeValue (Decode.at [ "medication_information", "prescription_drug_list" ] (Decode.list medicationDecoder)) app.data of
                Ok medications ->
                    medications
                        |> Debug.log "initialMedications"

                Err _ ->
                    []
                        |> Debug.log "initialMedications"

        model =
            { data = initialFormValues
            , naic = app.naic
            , carrier = app.naic |> carrierFromNaic
            , medications = initialMedications
            , medicationForm = Dict.empty
            , schema = app.schema
            , id = app.id
            , error = Nothing
            , expandedSections = Dict.singleton "applicant_info" True
            , currentDate = Nothing
            , showDebugFields = True
            , isValid = False
            , underwritingType = Nothing
            }
    in
    ( model
    , Task.perform GotCurrentTime Date.today
    )



-- Helper to extract initial form values from application data


extractFormValues : Decode.Value -> JsonValue
extractFormValues jsonData =
    case Decode.decodeValue jsonValueDecoder jsonData of
        Ok dict ->
            dict

        Err _ ->
            JsonObject Dict.empty


stringifyMaybe : Maybe JValue -> String
stringifyMaybe maybe =
    Maybe.map stringifyJValue maybe
        |> Maybe.withDefault ""


stringifyJValue : JValue -> String
stringifyJValue jvalue =
    case jvalue of
        StringValue str ->
            str

        IntValue n ->
            String.fromInt n

        FloatValue f ->
            String.fromFloat f

        BoolValue b ->
            if b then
                "true"

            else
                "false"

        _ ->
            ""



-- UPDATE


getValue : String -> String -> JsonValue -> Maybe JValue
getValue sectionId fieldId jsonValue =
    getSection sectionId jsonValue
        |> Dict.get fieldId
        |> Maybe.map unwrapJValue


getValueFull : String -> String -> JsonValue -> JsonValue
getValueFull sectionId fieldId jsonValue =
    getSection sectionId jsonValue
        |> Dict.get fieldId
        |> Maybe.withDefault (JsonBase NullValue)


getValueString : String -> String -> JsonValue -> String
getValueString sectionId fieldId jsonValue =
    getValue sectionId fieldId jsonValue
        |> Maybe.map stringifyJValue
        |> Maybe.withDefault ""


getArray : String -> Dict String JsonValue -> List JsonValue
getArray fieldName jsonValue =
    case Dict.get fieldName jsonValue of
        Just (JsonArray array) ->
            array

        _ ->
            []


setValue : String -> String -> JValue -> JsonValue -> JsonValue
setValue sectionId fieldId value jsonValue =
    let
        oldSection =
            getSection sectionId jsonValue

        newSection =
            Dict.insert fieldId (JsonBase value) oldSection
    in
    setSection sectionId newSection jsonValue


setComplexValue : String -> String -> JsonValue -> JsonValue -> JsonValue
setComplexValue sectionId fieldId complexObject jsonValue =
    let
        oldSection =
            getSection sectionId jsonValue

        newSection =
            Dict.insert fieldId complexObject oldSection
    in
    setSection sectionId newSection jsonValue


getSection : String -> JsonValue -> Dict String JsonValue
getSection sectionId jsonValue =
    case jsonValue of
        JsonObject dict ->
            Dict.get sectionId dict
                |> Maybe.andThen
                    (\value ->
                        case value of
                            JsonObject innerDict ->
                                Just innerDict

                            _ ->
                                Just Dict.empty
                    )
                |> Maybe.withDefault Dict.empty

        _ ->
            Dict.empty


setSection : String -> Dict String JsonValue -> JsonValue -> JsonValue
setSection sectionId newSection jsonValue =
    case jsonValue of
        JsonObject dict ->
            Dict.insert sectionId (JsonObject newSection) dict
                |> JsonObject

        _ ->
            JsonObject Dict.empty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotError error ->
            ( { model | error = Just error }, Cmd.none )

        ToggleSection sectionId ->
            let
                isExpanded =
                    Dict.get sectionId model.expandedSections
                        |> Maybe.withDefault False

                newExpandedSections =
                    Dict.insert sectionId (not isExpanded) model.expandedSections
            in
            ( { model | expandedSections = newExpandedSections }, Cmd.none )

        UpdateField sectionId fieldId valueString ->
            let
                value =
                    parseValue valueString |> JsonBase

                oldSection =
                    getSection sectionId model.data

                newSection =
                    Dict.insert fieldId value oldSection

                newData =
                    setSection sectionId newSection model.data

                cmd =
                    if sectionId == "payment" && fieldId == "eft_routing_number" then
                        if validRoutingNumber valueString then
                            Http.get
                                { url = "https://www.routingnumbers.info/api/name.json?rn=" ++ valueString
                                , expect = Http.expectJson GotRoutingNumber routingNumberDecoder
                                }

                        else
                            Cmd.none

                    else
                        Cmd.none
            in
            ( { model
                | data = newData
                , isValid = validateData model
              }
            , cmd
            )

        UpdateComplexPhoneField sectionId fieldId complexObject ->
            ( { model
                | data = setComplexValue sectionId fieldId complexObject model.data
              }
            , Cmd.none
            )

        GotCurrentTime currentDate ->
            let
                underwritingType =
                    determineUnderwritingType model.data (Just currentDate)

                underwritingInt =
                    underwritingType |> Maybe.withDefault -1

                newData =
                    setValue "enrollment_application" "underwriting_type" (IntValue underwritingInt) model.data
            in
            ( { model
                | currentDate = Just currentDate
                , data = newData
                , underwritingType = underwritingType
              }
            , Cmd.none
            )

        SaveForm ->
            let
                encodedData =
                    encodeData model.data

                newModel =
                    { model | error = Nothing }
            in
            ( newModel
            , saveApplication
                { id = model.id
                , data = encodedData
                }
            )

        SaveFormResponse response ->
            case response.error of
                Just error ->
                    ( { model | error = Just error }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | error = Nothing }
                    , Cmd.none
                    )

        GotRoutingNumber result ->
            case result of
                Ok institutionName ->
                    let
                        newData =
                            setValue "payment" "eft_financial_institution_name" (StringValue institutionName) model.data
                    in
                    ( { model | data = newData }
                    , Cmd.none
                    )

                Err error ->
                    ( model, Cmd.none )

        UpdateMedicationField baseId field value ->
            ( { model | medicationForm = Dict.insert field value model.medicationForm }
            , Cmd.none
            )

        SaveMedication baseId medication ->
            ( { model
                | medications = medication :: model.medications
                , medicationForm = Dict.empty
              }
            , Cmd.none
            )

        CancelAddMedication baseId ->
            ( { model | medicationForm = Dict.empty }
            , Cmd.none
            )

        RemoveMedication baseId idx ->
            ( { model
                | medications =
                    List.indexedMap (\i m -> ( i, m )) model.medications
                        |> List.filter (\( i, _ ) -> i /= idx)
                        |> List.map Tuple.second
              }
            , Cmd.none
            )

        SubmitToCSG ->
            if model.isValid then
                ( model, Cmd.none )

            else
                ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body



-- VIEW


view : Model -> Html Msg
view model =
    case model.error of
        Just error ->
            div [ class "text-red-500 p-4" ]
                [ text ("Error: " ++ error) ]

        _ ->
            div [ class "space-y-6 pt-8" ]
                [ viewForm model
                , div [ class "flex justify-center gap-4 pb-8" ]
                    [ viewSaveButton
                    , viewSubmitButton model.isValid
                    ]
                ]


viewSaveButton : Html Msg
viewSaveButton =
    button
        [ class """
            bg-purple-600 text-white px-6 py-2.5 rounded-lg font-medium
            hover:bg-purple-700 transition-colors shadow-sm
            focus:outline-none focus:ring-2 focus:ring-purple-500 focus:ring-offset-2
            w-[140px] text-sm
          """
        , onClick SaveForm
        ]
        [ text "Save" ]


viewSubmitButton : Bool -> Html Msg
viewSubmitButton isValid =
    button
        [ class <|
            """
            px-6 py-2.5 rounded-lg font-medium
            w-[140px] text-sm
            transition-colors shadow-sm
            focus:outline-none focus:ring-2 focus:ring-offset-2
            disabled:opacity-50 disabled:cursor-not-allowed
          """
                ++ (if isValid then
                        " bg-green-600 text-white hover:bg-green-700 focus:ring-green-500"

                    else
                        " bg-gray-400 text-white"
                   )
        , disabled (not isValid)
        , onClick SubmitToCSG
        ]
        [ text "Submit" ]


viewForm : Model -> Html Msg
viewForm model =
    div [ class "max-w-3xl mx-auto space-y-8 px-6" ]
        (List.sortBy .order model.schema
            |> List.map (renderFormSection model)
        )


getPhoneValue : String -> String -> JsonValue -> String
getPhoneValue sectionId fieldId jsonValue =
    let
        jsonBlob =
            getValueFull sectionId fieldId jsonValue
    in
    case jsonBlob of
        JsonObject dict ->
            let
                helper key =
                    Dict.get key dict
                        |> (\code ->
                                case code of
                                    Just (JsonBase (StringValue s)) ->
                                        s

                                    _ ->
                                        ""
                           )

                areaCode =
                    helper "area_code"

                officeCode =
                    helper "central_office_code"

                stationCode =
                    helper "station_code"

                isValid =
                    hasPhoneValue jsonBlob
            in
            if isValid then
                "(" ++ areaCode ++ ") " ++ officeCode ++ "-" ++ stationCode

            else
                ""

        _ ->
            ""


hasPhoneValue : JsonValue -> Bool
hasPhoneValue jsonValue =
    case jsonValue of
        JsonObject dict ->
            [ "area_code", "central_office_code", "station_code" ]
                |> List.map
                    (\key -> ( key, Dict.get key dict ))
                |> List.map
                    (\( key, code ) ->
                        case code of
                            Just (JsonBase (StringValue s)) ->
                                case String.toInt s of
                                    Just _ ->
                                        if key == "station_code" then
                                            String.length s == 4

                                        else
                                            String.length s == 3

                                    _ ->
                                        False

                            _ ->
                                False
                    )
                |> List.all identity

        _ ->
            False


validateFieldValue : Model -> FormSection -> FormField -> Bool
validateFieldValue model section field =
    let
        isRequired =
            case field.required of
                RequiredBool bool ->
                    bool

                RequiredDependsOn _ ->
                    True

        retrievedValue =
            getValueFull section.id field.id model.data

        fieldValueValid =
            case field.fieldType of
                ComplexPhoneField ->
                    hasPhoneValue retrievedValue

                _ ->
                    getValue section.id field.id model.data
                        |> isJust

        isInvalid =
            isFieldVisible field section model.data
                && isRequired
                && fieldValueValid
                && field.id
                /= "applicant_age"
    in
    not isInvalid


validateSection : Model -> FormSection -> Bool
validateSection model section =
    let
        hasEmptyRequired =
            List.any
                (validateFieldValue model section)
                section.body
    in
    not hasEmptyRequired


renderFormSection : Model -> FormSection -> Html Msg
renderFormSection model section =
    let
        hasVisibleFields =
            let
                visibleFields =
                    List.filter (\field -> isFieldVisible field section model.data) section.body

                {- _ =
                   Debug.log ("Visible fields in section " ++ section.id)
                       { totalFields = List.length section.body
                       , visibleFieldCount = List.length visibleFields
                       , visibleFieldIds = List.map .id visibleFields
                       }
                -}
            in
            not (List.isEmpty visibleFields)

        {- _ =
           Debug.log ("Section " ++ section.id ++ " visibility")
               { hasVisibleFields = hasVisibleFields
               , sectionDependsOn = section.dependsOn
               }
        -}
        isExpanded =
            Dict.get section.id model.expandedSections
                |> Maybe.withDefault False

        hasEmptyRequiredFields =
            validateSection model section

        headerBgClass =
            if hasEmptyRequiredFields then
                "bg-tokyo-orange/20"

            else
                "bg-gray-50"
    in
    if hasVisibleFields then
        div
            [ class """
                border border-gray-200 rounded-lg shadow-sm mb-8 bg-white
                transition-all duration-200 hover:shadow-md
              """
            ]
            [ div
                [ class <| """
                    flex items-center justify-between p-6 cursor-pointer
                    border-b border-gray-200
                    transition-colors duration-200
                  """ ++ " " ++ headerBgClass
                , onClick (ToggleSection section.id)
                ]
                [ div [ class "space-y-2" ]
                    [ h2 [ class "text-xl font-semibold text-gray-900 tracking-tight" ]
                        [ text section.title ]
                    ]
                , span
                    [ class <|
                        "text-gray-400 transition-transform duration-300"
                            ++ (if isExpanded then
                                    " rotate-180"

                                else
                                    ""
                               )
                    ]
                    [ text "▼" ]
                ]
            , if isExpanded then
                div [ class "p-6 space-y-6 border-t border-gray-100" ]
                    (List.sortBy .order section.body
                        |> List.map (renderFormField model section)
                    )

              else
                text ""
            ]

    else
        text ""


renderFormField : Model -> FormSection -> FormField -> Html Msg
renderFormField model section field =
    let
        isRequired =
            case field.required of
                RequiredBool bool ->
                    bool

                RequiredDependsOn _ ->
                    True

        fieldValueValid =
            validateFieldValue model section field

        shouldHighlight =
            not fieldValueValid

        baseInputClass =
            """
            w-full px-4 py-2 border border-gray-300 rounded-md
            focus:outline-none focus:ring-2 focus:ring-purple-500
            text-gray-700 transition-all duration-200
            hover:border-gray-400 text-base bg-white
            """

        labelClass =
            "block text-sm font-medium mb-2 text-gray-700"
                ++ (if shouldHighlight then
                        " bg-tokyo-orange/20"

                    else
                        ""
                   )

        displayLabel =
            case field.fieldType of
                TextBlockField _ ->
                    text ""

                _ ->
                    if isRequired then
                        span [] [ text field.displayLabel, span [ class "text-red-500" ] [ text " *" ] ]

                    else
                        text field.displayLabel

        wrapperClass =
            case field.fieldType of
                HeadingField _ ->
                    "mb-8 mt-16"

                _ ->
                    "mb-6"

        debugLabel =
            if model.showDebugFields then
                div [ class "text-xs text-gray-400 mb-1" ]
                    [ text (section.id ++ "." ++ field.id) ]

            else
                text ""
    in
    if isFieldVisible field section model.data then
        div [ class wrapperClass ]
            [ debugLabel
            , label [ class labelClass ]
                [ displayLabel ]
            , case field.fieldType of
                TextNameValueField fv ->
                    div [ class "flex items-center p-2 bg-gray-50 border border-gray-300 rounded-md text-gray-700" ]
                        [ span [ class "text-gray-600" ] [ text fv.displayLabel ]
                        , span [ class "ml-2" ] [ text fv.displayValue ]
                        ]

                ComplexDatePickerField ->
                    input
                        [ type_ "date"
                        , class baseInputClass
                        , value
                            (getValueString section.id field.id model.data)
                        , onInput (UpdateField section.id field.id)
                        ]
                        []

                NoDateDatePickerField ->
                    input
                        [ type_ "date"
                        , class baseInputClass
                        , value
                            (getValueString section.id field.id model.data)
                        , onInput (UpdateField section.id field.id)
                        ]
                        []

                TextField config ->
                    input
                        [ type_ "text"
                        , class baseInputClass
                        , value
                            (getValueString section.id field.id model.data)
                        , onInput (UpdateField section.id field.id)
                        , Maybe.map (\maxLen -> Html.Attributes.maxlength maxLen) config.maxLength
                            |> Maybe.withDefault (class "")
                        ]
                        []

                StringSearchField config ->
                    div [ class "space-y-2" ]
                        [ input
                            [ type_ "text"
                            , class baseInputClass
                            , value (getValueString section.id field.id model.data)
                            , onInput (UpdateField section.id field.id)
                            , Maybe.map (\maxLen -> Html.Attributes.maxlength maxLen) config.maxLength
                                |> Maybe.withDefault (class "")
                            ]
                            []
                        , div [ class "mt-2" ]
                            (List.map
                                (\childField -> renderFormField model section childField)
                                config.childFields
                            )
                        ]

                ComplexPhoneField ->
                    let
                        formattedValue =
                            getPhoneValue section.id field.id model.data
                    in
                    input
                        [ type_ "tel"
                        , class baseInputClass
                        , value formattedValue
                        , onInput
                            (\input ->
                                let
                                    digits =
                                        String.filter Char.isDigit input

                                    newAreaCode =
                                        String.left 3 digits

                                    newOfficeCode =
                                        String.slice 3 6 digits

                                    newStationCode =
                                        String.slice 6 10 digits

                                    complexObject =
                                        [ ( "area_code", JsonBase (StringValue newAreaCode) )
                                        , ( "central_office_code", JsonBase (StringValue newOfficeCode) )
                                        , ( "station_code", JsonBase (StringValue newStationCode) )
                                        ]
                                            |> Dict.fromList
                                            |> JsonObject
                                in
                                UpdateComplexPhoneField section.id field.id complexObject
                            )
                        ]
                        []

                SimpleEmailField config ->
                    input
                        [ type_ "email"
                        , class baseInputClass
                        , value
                            (getValueString section.id field.id model.data)
                        , onInput (UpdateField section.id field.id)
                        , Html.Attributes.maxlength config.maxLength
                        ]
                        []

                IntPickerField options ->
                    if field.id == "applicant_age" then
                        let
                            dobValue =
                                getValue "applicant_info" "applicant_dob" model.data

                            ageString =
                                CSGSchema.calculateAge dobValue model.currentDate
                                    |> Maybe.map String.fromInt
                                    |> Maybe.withDefault ""
                        in
                        text ("Calculated from DOB: " ++ ageString)

                    else
                        div [ class "relative" ]
                            [ select
                                [ class (baseInputClass ++ " appearance-none")
                                , value
                                    (getValueString section.id field.id model.data)
                                , onInput (UpdateField section.id field.id)
                                ]
                                (option [ value "" ] [ text "Select..." ]
                                    :: List.map
                                        (\opt ->
                                            option
                                                [ value (String.fromInt opt)
                                                , selected
                                                    (case getValue section.id field.id model.data of
                                                        Just (IntValue intValue) ->
                                                            intValue == opt

                                                        _ ->
                                                            False
                                                    )
                                                ]
                                                [ text (String.fromInt opt) ]
                                        )
                                        options
                                )
                            ]

                KeyValuePickerField options ->
                    div [ class "relative" ]
                        [ select
                            [ class (baseInputClass ++ " appearance-none")
                            , value
                                (getValueString section.id field.id model.data)
                            , onInput (UpdateField section.id field.id)
                            ]
                            (option [ value "" ] [ text "Select..." ]
                                :: List.map
                                    (\opt ->
                                        option
                                            [ value (stringifyJValue opt.value)
                                            , selected
                                                (case getValue section.id field.id model.data of
                                                    Just (StringValue value) ->
                                                        StringValue value == opt.value

                                                    _ ->
                                                        False
                                                )
                                            ]
                                            [ text opt.key ]
                                    )
                                    options
                            )
                        ]

                HeightField config ->
                    div [ class "flex space-x-2" ]
                        [ input
                            [ type_ "number"
                            , class baseInputClass
                            , Html.Attributes.min (String.fromInt config.minimumValue)
                            , Html.Attributes.max (String.fromInt config.maximumValue)
                            , value
                                (getValueString section.id field.id model.data)
                            , onInput (UpdateField section.id field.id)
                            ]
                            []
                        , span [ class "self-center text-cyan-300" ] [ text config.displayType ]
                        ]

                WeightField config ->
                    input
                        [ type_ "number"
                        , class baseInputClass
                        , Html.Attributes.min (String.fromInt config.minimumValue)
                        , Html.Attributes.max (String.fromInt config.maximumValue)
                        , value
                            (getValueString section.id field.id model.data)
                        , onInput (UpdateField section.id field.id)
                        ]
                        []

                HeadingField _ ->
                    text ""

                SSNField _ ->
                    input
                        [ type_ "text"
                        , class baseInputClass
                        , value
                            (getValueString section.id field.id model.data)
                        , onInput (UpdateField section.id field.id)
                        ]
                        []

                ComplexRadioField config ->
                    div [ class "space-y-4" ]
                        [ div [ class "space-y-2" ]
                            (List.map
                                (\childField -> renderFormField model section childField)
                                config.childFields
                            )
                        ]

                TextBlockField _ ->
                    div
                        [ class "bg-gray-50 p-4 rounded-md border border-gray-300 text-gray-600" ]
                        [ text field.displayLabel ]

                CheckboxField options1 options2 ->
                    if List.length options1 > 0 then
                        div [ class "space-y-2" ]
                            (List.map
                                (\opt ->
                                    label [ class "flex items-center space-x-2" ]
                                        [ input
                                            [ type_ "checkbox"
                                            , checked opt.value
                                            , value
                                                (if opt.value then
                                                    "true"

                                                 else
                                                    "false"
                                                )
                                            , onInput (UpdateField section.id field.id)
                                            , class "text-cyan-400 border-cyan-600 rounded focus:ring-cyan-400"
                                            ]
                                            []
                                        , span [ class "text-cyan-100" ] [ text opt.key ]
                                        ]
                                )
                                options1
                            )

                    else
                        div [ class "relative" ]
                            [ select
                                [ class (baseInputClass ++ " appearance-none")
                                , value
                                    (getValueString section.id field.id model.data)
                                , onInput (UpdateField section.id field.id)
                                ]
                                (option [ value "" ] [ text "Select..." ]
                                    :: List.map
                                        (\opt ->
                                            option
                                                [ value (stringifyJValue opt.value)
                                                , selected
                                                    (case getValue section.id field.id model.data of
                                                        Just (StringValue value) ->
                                                            StringValue value == opt.value

                                                        _ ->
                                                            False
                                                    )
                                                ]
                                                [ text opt.key ]
                                        )
                                        options2
                                )
                            ]

                StringPickerField options ->
                    div [ class "relative" ]
                        [ select
                            [ class (baseInputClass ++ " appearance-none")
                            , value
                                (getValueString section.id field.id model.data)
                            , onInput (UpdateField section.id field.id)
                            ]
                            (option [ value "" ] [ text "Select..." ]
                                :: List.map
                                    (\opt ->
                                        option
                                            [ value (stringifyJValue opt.value)
                                            , selected
                                                (case getValue section.id field.id model.data of
                                                    Just (StringValue value) ->
                                                        StringValue value == opt.value

                                                    _ ->
                                                        False
                                                )
                                            ]
                                            [ text opt.key ]
                                    )
                                    options
                            )
                        ]

                DrugLookupField config ->
                    renderDrugLookupField model section field

                InputTableField config ->
                    div [ class "space-y-4" ]
                        [ div [ class "space-y-2" ]
                            (List.map
                                (\childField -> renderFormField model section childField)
                                config.childFields
                            )
                        , button
                            [ class """
                                bg-cyan-600 text-cyan-100 px-4 py-2 rounded
                                hover:bg-cyan-500 transition-colors
                              """
                            , type_ "button"
                            ]
                            [ text config.btnDisplayValue ]
                        ]

                FileUploadField ->
                    input
                        [ type_ "file"
                        , class baseInputClass
                        , onInput (UpdateField section.id field.id)
                        ]
                        []

                RadioField options ->
                    div [ class "space-y-2" ]
                        (List.map
                            (\opt ->
                                let
                                    optionValue =
                                        stringifyJValue opt.value

                                    isSelected =
                                        Just opt.value == getValue section.id field.id model.data
                                in
                                label
                                    [ class
                                        ("""
                                        flex items-center gap-2 p-2 rounded-md cursor-pointer
                                        border transition-colors duration-200
                                        """
                                            ++ (if isSelected then
                                                    "bg-purple-50 border-purple-600"

                                                else
                                                    "border-gray-300 hover:bg-gray-50"
                                               )
                                        )
                                    ]
                                    [ input
                                        [ type_ "radio"
                                        , name (section.id ++ "." ++ field.id)
                                        , value optionValue
                                        , checked isSelected
                                        , onInput (\_ -> UpdateField section.id field.id optionValue)
                                        , class "text-purple-600 focus:ring-purple-500"
                                        ]
                                        []
                                    , span [ class "text-gray-700" ] [ text opt.key ]
                                    ]
                            )
                            options
                        )

                LinkField config ->
                    div [ class "space-y-2" ]
                        [ a
                            [ href config.url
                            , target "_blank"
                            , rel "noopener noreferrer"
                            , class """
                                inline-flex items-center gap-2 px-4 py-2
                                bg-cyan-800/20 text-cyan-400 hover:text-cyan-300
                                border border-cyan-600 hover:border-cyan-400
                                rounded-md transition-all duration-200
                                hover:bg-cyan-800/30
                                focus:outline-none focus:ring-2 
                                focus:ring-cyan-400 focus:ring-opacity-50
                              """
                            ]
                            [ text field.displayLabel
                            , span [ class "text-sm" ] [ text "↗" ]
                            ]
                        ]
            ]

    else
        text ""



-- DATA VALIDATION AND PROCESSING


validateData : Model -> Bool
validateData model =
    let
        invalidSections =
            List.filter (\section -> validateSection model section) model.schema

        _ =
            Debug.log "Invalid sections" (List.map .id invalidSections)
    in
    List.all (\section -> not (validateSection model section)) model.schema


determineUnderwritingType : JsonValue -> Maybe Date -> Maybe Int
determineUnderwritingType data currentDate =
    let
        applicantDobString =
            getValue "applicant_info" "applicant_dob" data

        applicantDob =
            case applicantDobString of
                Just (StringValue dobString) ->
                    case Date.fromIsoString dobString of
                        Ok date ->
                            Just date

                        Err _ ->
                            Nothing

                _ ->
                    Nothing

        partBDateString =
            getValue "applicant_info" "part_b_date" data

        partBDate =
            case partBDateString of
                Just (StringValue pbs) ->
                    case Date.fromIsoString pbs of
                        Ok date ->
                            Just date

                        Err _ ->
                            Nothing

                _ ->
                    Nothing
    in
    case applicantDob of
        Just dob ->
            if isOpenEnrollment applicantDob partBDate currentDate then
                Just 1

            else
                Just 0

        Nothing ->
            Nothing


isOpenEnrollment : Maybe Date -> Maybe Date -> Maybe Date -> Bool
isOpenEnrollment applicantDob partBDate currentDate =
    -- Check T65 window first
    if isT65 applicantDob currentDate then
        True

    else
        -- Check Part B enrollment window
        case ( partBDate, currentDate ) of
            ( Just pbDate, Just now ) ->
                Date.diff Months pbDate now < 6

            _ ->
                False


isT65 : Maybe Date -> Maybe Date -> Bool
isT65 applicantDob currentDate =
    case ( applicantDob, currentDate ) of
        ( Just dob, Just now ) ->
            let
                t65Date =
                    Date.add Years 65 dob
            in
            Date.diff Months t65Date now < 6

        _ ->
            False



-- ROUTING NUMBER STUFF


validRoutingNumber : String -> Bool
validRoutingNumber routingNumber =
    let
        isNineDigits =
            String.length routingNumber == 9 && String.all Char.isDigit routingNumber

        weights =
            [ 3, 7, 1, 3, 7, 1, 3, 7, 1 ]

        digits =
            String.toList routingNumber
                |> List.map (String.fromChar >> String.toInt >> Maybe.withDefault 0)

        checksum =
            List.map2 (*) digits weights
                |> List.sum
                |> modBy 10
    in
    isNineDigits && checksum == 0


routingNumberDecoder : Decoder String
routingNumberDecoder =
    Decode.field "name" Decode.string



-- Add subscription to handle save response


subscriptions : Model -> Sub Msg
subscriptions model =
    saveApplicationResponse SaveFormResponse



-- MEDICATION TYPES
-- Base types that are common across carriers
-- Add these helper functions for unwrapping JValues


unwrapStringValue : JValue -> String
unwrapStringValue jvalue =
    case jvalue of
        StringValue str ->
            str

        _ ->
            ""


unwrapBoolValue : JValue -> Bool
unwrapBoolValue jvalue =
    case jvalue of
        BoolValue bool ->
            bool

        _ ->
            False



-- Add medication types and helpers
-- this tracs Ace atm, need to abstract for other carrier


type alias DrugDetails =
    { gpI10 : String
    , productName : String
    , drugName : String
    , displayName : String
    }


type alias DosageDetails =
    { dosage : String
    , ndc : String
    }


type alias Medication =
    { drug : DrugDetails
    , diagnosis : String
    , dosage : DosageDetails
    , frequency : String
    , quantity : Maybe Int
    , lastFillDate : String
    , medStartDate : String
    }


type Carrier
    = ACE
    | Aetna
    | Allstate
    | UHC


medicationDecoder : Decode.Decoder Medication
medicationDecoder =
    Decode.succeed Medication
        |> Pipeline.required "drug" (Decode.maybe drugDetailsDecoder |> Decode.map (Maybe.withDefault defaultDrugDetails))
        |> Pipeline.optional "diagnosis" Decode.string ""
        |> Pipeline.required "dosage" (Decode.maybe dosageDetailsDecoder |> Decode.map (Maybe.withDefault defaultDosageDetails))
        |> Pipeline.optional "frequency" Decode.string ""
        |> Pipeline.optional "quantity" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "lastFillDate" Decode.string ""
        |> Pipeline.optional "medStartDate" Decode.string ""


drugDetailsDecoder : Decode.Decoder DrugDetails
drugDetailsDecoder =
    Decode.succeed DrugDetails
        |> Pipeline.optional "gpI10" Decode.string ""
        |> Pipeline.optional "productName" Decode.string ""
        |> Pipeline.optional "drugName" Decode.string ""
        |> Pipeline.optional "displayName" Decode.string ""


dosageDetailsDecoder : Decode.Decoder DosageDetails
dosageDetailsDecoder =
    Decode.succeed DosageDetails
        |> optional "dosage" Decode.string ""
        |> optional "ndc" Decode.string ""


defaultDrugDetails : DrugDetails
defaultDrugDetails =
    { gpI10 = ""
    , productName = ""
    , drugName = ""
    , displayName = ""
    }


defaultDosageDetails : DosageDetails
defaultDosageDetails =
    { dosage = ""
    , ndc = ""
    }


carrierFromNaic : String -> Maybe Carrier
carrierFromNaic naic =
    case naic of
        "20699" ->
            Just ACE

        "72052" ->
            Just Aetna

        "78700" ->
            Just Aetna

        "68500" ->
            Just Aetna

        "79413" ->
            Just UHC

        "82538" ->
            Just Allstate

        "60534" ->
            Just Allstate

        _ ->
            Nothing


renderDrugLookupField : Model -> FormSection -> FormField -> Html Msg
renderDrugLookupField model section field =
    let
        carrier =
            Maybe.withDefault ACE model.carrier

        showForm =
            model.underwritingType /= Just 1 || model.underwritingType /= Just 2
    in
    div [ class "space-y-6" ]
        [ -- Medication list display
          if List.isEmpty model.medications then
            div [ class "text-gray-500 italic" ]
                [ text "No medications added" ]

          else
            div [ class "space-y-4" ]
                (List.indexedMap (renderMedicationItem "") model.medications)

        -- Add medication form
        , if showForm then
            renderMedicationForm carrier model

          else
            text ""
        ]



-- Render a single medication item based on carrier type


renderMedicationItem : String -> Int -> Medication -> Html Msg
renderMedicationItem baseId index medication =
    div [ class "p-4 border border-gray-200 rounded-md bg-white space-y-2" ]
        [ div [ class "flex justify-between items-start" ]
            [ div [ class "space-y-1" ]
                [ div [ class "font-medium text-gray-900" ]
                    [ text medication.drug.drugName ]
                , div [ class "text-sm text-gray-600" ]
                    [ text ("Diagnosis: " ++ medication.diagnosis) ]
                , if not (String.isEmpty medication.dosage.dosage) then
                    div [ class "text-sm text-gray-600" ]
                        [ text ("Dosage: " ++ medication.dosage.dosage) ]

                  else
                    text ""
                , if not (String.isEmpty medication.frequency) then
                    div [ class "text-sm text-gray-600" ]
                        [ text ("Frequency: " ++ medication.frequency) ]

                  else
                    text ""
                , case medication.quantity of
                    Just qty ->
                        div [ class "text-sm text-gray-600" ]
                            [ text ("Quantity: " ++ String.fromInt qty) ]

                    Nothing ->
                        text ""
                , if not (String.isEmpty medication.lastFillDate) then
                    div [ class "text-sm text-gray-600" ]
                        [ text ("Last Fill Date: " ++ medication.lastFillDate) ]

                  else
                    text ""
                ]
            , button
                [ class """text-red-600 hover:text-red-700 p-1 rounded-md
                          hover:bg-red-50 transition-colors"""
                , onClick (RemoveMedication baseId index)
                ]
                [ text "×" ]
            ]
        ]


renderMedicationForm : Carrier -> Model -> Html Msg
renderMedicationForm carrier model =
    let
        commonFields =
            [ formField "Medication Name" "drugName" "text"
            , formField "Diagnosis" "diagnosis" "text"
            ]

        carrierFields =
            case carrier of
                ACE ->
                    []

                Aetna ->
                    []

                Allstate ->
                    [ formField "Dosage" "dosage" "text"
                    , formField "Frequency" "frequency" "text"
                    , formField "Quantity" "prescription_freq_other" "number"
                    ]

                UHC ->
                    [ formField "Dosage" "dosage" "text"
                    , formField "Frequency" "frequency" "text"
                    , formField "Quantity" "quantity" "number"
                    , formField "Last Fill Date" "lastFillDate" "date"
                    ]

        formField label_ fieldName inputType =
            div [ class "space-y-2" ]
                [ label [ class "block text-sm font-medium text-gray-700" ]
                    [ text label_ ]
                , input
                    [ class """w-full px-4 py-2 border border-gray-300 rounded-md
                              focus:outline-none focus:ring-2 focus:ring-purple-500"""
                    , type_ inputType
                    , onInput (UpdateMedicationField "" fieldName)
                    , value (Dict.get fieldName model.medicationForm |> Maybe.withDefault "")
                    ]
                    []
                ]
    in
    div [ class "p-6 border border-gray-200 rounded-md space-y-4" ]
        [ div [ class "space-y-4" ]
            (commonFields ++ carrierFields)
        , div [ class "flex justify-end gap-4" ]
            [ button
                [ class "px-4 py-2 text-sm text-gray-600 hover:text-gray-800"
                , onClick (CancelAddMedication "")
                ]
                [ text "Cancel" ]
            , button
                [ class "px-4 py-2 text-sm bg-purple-600 text-white rounded-md hover:bg-purple-700"
                , onClick (SaveMedication "" (createNewMedication model ""))
                ]
                [ text "Save" ]
            ]
        ]


addMedicationsToJson : Dict String (List Medication) -> Encode.Value -> Encode.Value
addMedicationsToJson medications baseJson =
    case Decode.decodeValue (Decode.dict Decode.value) baseJson of
        Ok dict ->
            Dict.foldl
                (\key meds acc ->
                    Dict.insert key (encodeMedicationList meds) acc
                )
                dict
                medications
                |> Encode.dict identity identity

        Err _ ->
            baseJson


encodeMedicationList : List Medication -> Encode.Value
encodeMedicationList medications =
    Encode.list
        (\med ->
            Encode.object
                [ ( "drugName", Encode.string med.drug.drugName )
                , ( "diagnosis", Encode.string med.diagnosis )
                , ( "dosage", Encode.string med.dosage.dosage )
                , ( "frequency", Encode.string med.frequency )
                , ( "quantity"
                  , case med.quantity of
                        Just q ->
                            Encode.int q

                        Nothing ->
                            Encode.null
                  )
                ]
        )
        medications


createNewMedication : Model -> String -> Medication
createNewMedication model baseId =
    { drug =
        { gpI10 = ""
        , productName = Dict.get "drugName" model.medicationForm |> Maybe.withDefault ""
        , drugName = Dict.get "drugName" model.medicationForm |> Maybe.withDefault ""
        , displayName = Dict.get "drugName" model.medicationForm |> Maybe.withDefault ""
        }
    , diagnosis = Dict.get "diagnosis" model.medicationForm |> Maybe.withDefault ""
    , dosage =
        { dosage = Dict.get "dosage" model.medicationForm |> Maybe.withDefault ""
        , ndc = ""
        }
    , frequency = Dict.get "frequency" model.medicationForm |> Maybe.withDefault ""
    , quantity =
        Dict.get "quantity" model.medicationForm
            |> Maybe.andThen String.toInt
    , lastFillDate = Dict.get "lastFillDate" model.medicationForm |> Maybe.withDefault ""
    , medStartDate = ""
    }


encodeData : JsonValue -> Encode.Value
encodeData jsonValue =
    case jsonValue of
        JsonObject dict ->
            Dict.map (\_ v -> encodeJsonValue v) dict
                |> Encode.dict identity identity

        _ ->
            Encode.null


encodeJsonValue : JsonValue -> Encode.Value
encodeJsonValue value =
    case value of
        JsonObject dict ->
            Dict.map (\_ v -> encodeJsonValue v) dict
                |> Encode.dict identity identity

        JsonArray arr ->
            Encode.list encodeJsonValue arr

        JsonBase (StringValue str) ->
            Encode.string str

        JsonBase (IntValue n) ->
            Encode.int n

        JsonBase (FloatValue f) ->
            Encode.float f

        JsonBase (BoolValue b) ->
            Encode.bool b

        JsonBase NullValue ->
            Encode.null


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


applicationViewDecoder : Decode.Decoder Application
applicationViewDecoder =
    Decode.map4 Application
        (Decode.field "id" Decode.string)
        (Decode.field "naic" Decode.string)
        (Decode.field "data" Decode.value)
        (Decode.field "schema" (Decode.field "sections" CSGSchema.formSchemaDecoder))
