port module ApplicationView exposing (Application, Model, Msg(..), applicationViewDecoder, init, subscriptions, update, view)

import CSGSchema exposing (ApplicationSchema, Carrier(..), FormField, FormFieldType(..), FormSection, JValue(..), JsonValue(..), RequiredType(..), carrierFromNaic, defaultAetnaMedicationSection, defaultMedicationSection, isFieldVisible, jsonValueDecoder, parseValue, unwrapJValue)
import DataEncoder exposing (unflattenData)
import Date exposing (Date, Unit(..))
import Debug
import Dict exposing (Dict)
import Hash
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import List.Extra
import Producer exposing (getProducerSection)
import Regex
import Task
import Time exposing (Month(..))



-- Port for saving application data


port saveApplication : { id : String, data : Encode.Value } -> Cmd msg



-- Port for receiving save response


port saveApplicationResponse : ({ success : Bool, error : Maybe String } -> msg) -> Sub msg



-- Add at the top with other ports


port forceRefreshLAProToken : () -> Cmd msg


port getLAProTokenResponse : (String -> msg) -> Sub msg



-- Port for submitting to CSG


port submitToCSG : ( String, Int ) -> Cmd msg



-- Port for receiving CSG submission response


port submitToCSGResponse : ({ success : Bool, error : Maybe String } -> msg) -> Sub msg


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
    , drugSearchResults : List DrugSearchResult
    , selectedDrug : Maybe String
    , drugDosages : List DrugInfo
    , loadingDrugData : Bool
    , laproToken : Maybe String
    , searchError : Maybe String
    , isSearching : Bool
    , hasUnsavedChanges : Bool
    , producerId : Int
    , producerConfigs : Dict Int Producer.ProducerConfig
    , submittingToCSG : Bool
    , csgSubmissionError : Maybe String
    }


type Msg
    = GotError String
    | ToggleSection String
    | UpdateField String String String
    | UpdateComplexPhoneField String String JsonValue
    | SaveForm
    | SaveFormResponse { success : Bool, error : Maybe String }
    | SetUnderwritingType Int
    | SetProducer Int
    | GotCurrentTime Date
    | SubmitToCSG
    | CSGSubmissionResponse { success : Bool, error : Maybe String }
    | NoOp
    | GotRoutingNumber (Result Http.Error String)
    | UpdateMedicationField String String String
    | SaveMedication String Medication
    | CancelAddMedication String
    | RemoveMedication String Int
    | SearchDrugs String
    | DrugSearchResponse (Result Http.Error (List DrugSearchResult))
    | SelectDrug String
    | DrugDosageResponse (Result Http.Error (List DrugInfo))
    | GotLAProToken String
    | CheckForUnsavedChanges Time.Posix


type alias Application =
    { id : String
    , naic : String
    , data : Decode.Value
    , formattedData : Decode.Value
    , schema : ApplicationSchema
    }



-- INIT


init : Decode.Value -> Application -> ( Model, Cmd Msg )
init producerConfigJson app =
    let
        producerConfigs =
            producerConfigJson
                |> Decode.decodeValue Producer.producerConfigDecoder
                |> Result.toMaybe
                |> Maybe.withDefault Dict.empty
                |> Debug.log "producerConfigs"

        defaultProducer =
            producerConfigs
                |> Dict.toList
                |> List.filter (\( _, config ) -> config.isDefault)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 1

        initialFormValuesRaw =
            app.data
                |> extractFormValues

        initialFormattedValues =
            app.formattedData
                |> extractFormValues

        initialFormValues0 =
            case initialFormattedValues of
                JsonBase NullValue ->
                    initialFormValuesRaw

                _ ->
                    initialFormattedValues

        carrierInit =
            app.naic
                |> carrierFromNaic
                |> Debug.log "carrierInit"

        producerConfig =
            Dict.get defaultProducer producerConfigs

        initialMedications : Maybe (List Medication)
        initialMedications =
            case Decode.decodeValue (Decode.at [ "medication_information", "prescription_drug_list" ] (Decode.list medicationDecoder)) app.data of
                Ok medications ->
                    Just medications

                Err _ ->
                    case Decode.decodeValue (Decode.at [ "health_history", "prescription_drug_list" ] (Decode.list medicationDecoder)) app.data of
                        Ok medications ->
                            Just medications

                        Err _ ->
                            Nothing

        producerSection =
            case ( carrierInit, producerConfig ) of
                ( Just carrier, Just config ) ->
                    getProducerSection carrier config |> Just

                _ ->
                    Nothing

        _ =
            Debug.log "producerSection" producerSection

        initialFormValues1 =
            case producerSection of
                Just section ->
                    overwriteSection "producer" section initialFormValues0

                Nothing ->
                    initialFormValues0

        initialFormValues =
            case initialMedications of
                Just medications ->
                    updateModelDataWithMedications carrierInit medications initialFormValues1

                Nothing ->
                    initialFormValues1

        schema =
            app.schema
                |> List.map
                    (\section ->
                        if section.id == "medication_information" then
                            defaultMedicationSection

                        else if section.id == "health_history" then
                            defaultAetnaMedicationSection

                        else
                            section
                    )

        model =
            { data = initialFormValues |> Debug.log "initialFormValues"
            , naic = app.naic
            , carrier = carrierInit
            , medications = Maybe.withDefault [] initialMedications
            , medicationForm = Dict.empty
            , schema = schema
            , id = app.id
            , error = Nothing
            , expandedSections = Dict.empty
            , currentDate = Nothing
            , showDebugFields = True
            , isValid = False
            , underwritingType = Nothing
            , drugSearchResults = []
            , selectedDrug = Nothing
            , drugDosages = []
            , loadingDrugData = False
            , laproToken = Nothing
            , searchError = Nothing
            , isSearching = False
            , hasUnsavedChanges = False
            , producerId = defaultProducer
            , producerConfigs = producerConfigs
            , submittingToCSG = False
            , csgSubmissionError = Nothing
            }
    in
    ( model
    , Cmd.batch
        [ Task.perform GotCurrentTime Date.today
        , forceRefreshLAProToken ()
        ]
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


overwriteSection : String -> JsonValue -> JsonValue -> JsonValue
overwriteSection sectionId newSection jsonValue =
    case jsonValue of
        JsonObject dict ->
            Dict.insert sectionId newSection dict
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
                    parseValue valueString
                        |> JsonBase
                        |> Debug.log "UpdateField"

                oldSection =
                    getSection sectionId model.data

                -- Only update if the value actually changed
                shouldUpdate =
                    getValue sectionId fieldId model.data
                        |> Maybe.map (stringifyJValue >> (/=) valueString)
                        |> Maybe.withDefault True

                newModel =
                    if shouldUpdate then
                        let
                            newSection =
                                Dict.insert fieldId value oldSection

                            newData =
                                setSection sectionId newSection model.data

                            -- Check if we need to update enrollment type
                            shouldUpdateEnrollment =
                                (sectionId == "applicant_info" && fieldId == "applicant_dob")
                                    || (sectionId == "applicant_info" && fieldId == "part_b_date")

                            newUnderwritingType =
                                if shouldUpdateEnrollment then
                                    determineUnderwritingType newData model.currentDate

                                else
                                    model.underwritingType

                            -- If enrollment type changed, update it in the data
                            finalData =
                                case newUnderwritingType of
                                    Just underwritingInt ->
                                        setValue "enrollment_application" "underwriting_type" (IntValue underwritingInt) newData

                                    Nothing ->
                                        newData
                        in
                        { model
                            | data = finalData
                            , underwritingType = newUnderwritingType
                            , isValid = validateData model
                            , hasUnsavedChanges = finalData /= model.data
                        }

                    else
                        model

                cmd =
                    if sectionId == "payment" && fieldId == "eft_routing_number" && shouldUpdate then
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
            ( newModel, cmd )

        UpdateComplexPhoneField sectionId fieldId complexObject ->
            let
                newData =
                    setComplexValue sectionId fieldId complexObject model.data
            in
            ( { model
                | data = newData
                , hasUnsavedChanges = newData /= model.data
              }
            , Cmd.none
            )

        SetProducer producer ->
            let
                producerConfig =
                    Dict.get producer model.producerConfigs

                newData =
                    case ( model.carrier, producerConfig ) of
                        ( Just carrier, Just config ) ->
                            case getProducerSection carrier config of
                                JsonObject dict ->
                                    setSection "producer" dict model.data

                                _ ->
                                    model.data

                        _ ->
                            model.data
            in
            ( { model
                | producerId = producer
                , data = newData
                , hasUnsavedChanges = newData /= model.data
              }
            , Cmd.none
            )

        SetUnderwritingType underwritingInt ->
            let
                newData =
                    setValue "enrollment_application" "underwriting_type" (IntValue underwritingInt) model.data
            in
            ( { model
                | data = newData
                , hasUnsavedChanges = newData /= model.data
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
                    { model | error = Nothing, hasUnsavedChanges = False }
            in
            ( newModel
            , if model.hasUnsavedChanges then
                saveApplication
                    { id = model.id
                    , data = encodedData
                    }

              else
                Cmd.none
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
            ( { model
                | medicationForm = Dict.insert field value model.medicationForm
                , hasUnsavedChanges = True
              }
            , Cmd.none
            )

        SaveMedication baseId medication ->
            let
                newMedications =
                    medication :: model.medications

                newData =
                    updateModelDataWithMedications model.carrier newMedications model.data
            in
            ( { model
                | medications = newMedications
                , medicationForm = Dict.empty
                , data = newData
                , hasUnsavedChanges = True
              }
            , Cmd.none
            )

        CancelAddMedication baseId ->
            ( { model | medicationForm = Dict.empty }
            , Cmd.none
            )

        RemoveMedication baseId idx ->
            let
                newMedications =
                    List.indexedMap (\i m -> ( i, m )) model.medications
                        |> List.filter (\( i, _ ) -> i /= idx)
                        |> List.map Tuple.second

                newData =
                    updateModelDataWithMedications model.carrier newMedications model.data
            in
            ( { model
                | medications = newMedications
                , data = newData
                , hasUnsavedChanges = True
              }
            , Cmd.none
            )

        SubmitToCSG ->
            ( { model | submittingToCSG = True, csgSubmissionError = Nothing }
            , submitToCSG ( model.id, model.producerId )
            )

        CSGSubmissionResponse response ->
            ( { model
                | submittingToCSG = False
                , csgSubmissionError = response.error
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        SearchDrugs query ->
            let
                _ =
                    Debug.log "SearchDrugs" query

                _ =
                    Debug.log "Token" model.laproToken

                shouldSearch =
                    String.length query > 2
            in
            ( { model
                | drugSearchResults = []
                , medicationForm = Dict.insert "drugName" query model.medicationForm
                , searchError = Nothing
                , isSearching = shouldSearch
              }
            , if shouldSearch then
                case model.laproToken of
                    Just token ->
                        if String.isEmpty token then
                            forceRefreshLAProToken ()

                        else
                            searchDrugs token query

                    Nothing ->
                        forceRefreshLAProToken ()

              else
                Cmd.none
            )

        DrugSearchResponse result ->
            let
                _ =
                    Debug.log "DrugSearchResponse" result
            in
            case result of
                Ok results ->
                    ( { model
                        | drugSearchResults = results
                        , isSearching = False
                        , searchError = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    let
                        _ =
                            Debug.log "DrugSearchError" (httpErrorToString error)
                    in
                    case error of
                        Http.BadStatus _ ->
                            -- Token expired, get a new one and retry
                            ( { model
                                | drugSearchResults = []
                                , isSearching = True
                                , searchError = Nothing
                                , laproToken = Nothing
                              }
                            , forceRefreshLAProToken ()
                            )

                        _ ->
                            ( { model
                                | drugSearchResults = []
                                , isSearching = False
                                , searchError = Just (httpErrorToString error)
                              }
                            , Cmd.none
                            )

        SelectDrug drugName ->
            ( { model
                | selectedDrug = Just drugName
                , medicationForm = Dict.insert "drugName" drugName model.medicationForm
                , drugSearchResults = [] -- Clear search results immediately
                , loadingDrugData = True
              }
            , case model.laproToken of
                Just token ->
                    getDrugDosages drugName token

                Nothing ->
                    forceRefreshLAProToken ()
            )

        DrugDosageResponse result ->
            case result of
                Ok dosages ->
                    ( { model
                        | drugDosages = dosages
                        , loadingDrugData = False
                      }
                    , Cmd.none
                    )

                Err error ->
                    case error of
                        Http.BadStatus _ ->
                            -- Token expired, get a new one and retry
                            ( { model
                                | drugDosages = []
                                , loadingDrugData = True
                                , laproToken = Nothing
                              }
                            , forceRefreshLAProToken ()
                            )

                        _ ->
                            ( { model
                                | drugDosages = []
                                , loadingDrugData = False
                                , searchError = Just (httpErrorToString error)
                              }
                            , Cmd.none
                            )

        GotLAProToken token ->
            let
                _ =
                    Debug.log "ELM GotLAProToken" token

                validToken =
                    not (String.isEmpty token)

                _ =
                    Debug.log "ELM validToken" validToken

                nextCmd =
                    if not validToken then
                        forceRefreshLAProToken ()

                    else if model.isSearching then
                        case Dict.get "drugName" model.medicationForm of
                            Just query ->
                                if String.length query > 2 then
                                    searchDrugs token query

                                else
                                    Cmd.none

                            Nothing ->
                                Cmd.none

                    else if model.loadingDrugData then
                        case model.selectedDrug of
                            Just drugName ->
                                getDrugDosages token drugName

                            Nothing ->
                                Cmd.none

                    else
                        Cmd.none

                _ =
                    Debug.log "Setting token in model"
                        (if validToken then
                            Just token

                         else
                            Nothing
                        )
            in
            ( { model
                | laproToken =
                    if validToken then
                        Just token

                    else
                        Nothing
              }
            , nextCmd
            )

        CheckForUnsavedChanges posix ->
            if model.hasUnsavedChanges && not model.isSearching then
                update SaveForm model

            else
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
    div [ class "container mx-auto px-4 py-8" ]
        [ div [ class "flex justify-between items-center mb-6" ]
            [ h1 [ class "text-2xl font-bold" ] [ text "Application" ]
            ]
        , case model.error of
            Just error ->
                div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative mb-4" ]
                    [ text error ]

            Nothing ->
                text ""
        , case model.csgSubmissionError of
            Just error ->
                div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative mb-4" ]
                    [ text error ]

            Nothing ->
                text ""
        , viewControls model
        , viewForm model
        , div [ class "flex justify-center mt-8" ]
            [ viewSubmitButton model ]
        ]


viewControls : Model -> Html Msg
viewControls model =
    let
        order : List Int
        order =
            model.producerId
                :: (Dict.keys model.producerConfigs
                        |> List.filter
                            (\id -> id /= model.producerId)
                   )
    in
    div [ class "producer-section max-w-3xl mx-auto px-6 mb-8" ]
        [ div [ class "producer-controls" ]
            [ div [ class "producer-group" ]
                [ label [ class "producer-label" ]
                    [ text "Producer" ]
                , select
                    [ class "underwriting-select"
                    , value (String.fromInt model.producerId)
                    , onInput (\str -> SetProducer (String.toInt str |> Maybe.withDefault 2))
                    ]
                    (List.map (viewProducerOption model.producerConfigs) order)
                ]
            , div [ class "underwriting-group" ]
                [ label [ class "producer-label" ]
                    [ text "Underwriting Type" ]
                , select
                    [ class "underwriting-select"
                    , value (Maybe.map String.fromInt model.underwritingType |> Maybe.withDefault "")
                    , onInput (\str -> SetUnderwritingType (String.toInt str |> Maybe.withDefault 0))
                    ]
                    [ option [ value "0" ] [ text "Underwritten" ]
                    , option [ value "1" ] [ text "Open Enrollment" ]
                    , option [ value "2" ] [ text "Guaranteed Issue" ]
                    ]
                ]
            ]
        ]


viewProducerOption : Dict.Dict Int Producer.ProducerConfig -> Int -> Html Msg
viewProducerOption producerConfigs producerId =
    case Dict.get producerId producerConfigs of
        Just config ->
            option [ value (String.fromInt producerId) ]
                [ text (config.firstName ++ " " ++ config.lastName) ]

        Nothing ->
            text ""


viewSubmitButton : Model -> Html Msg
viewSubmitButton model =
    if model.submittingToCSG then
        button
            [ class "disabled:opacity-50 cursor-not-allowed bg-purple-600 text-white px-4 py-2 rounded"
            , disabled True
            ]
            [ text "Submitting..." ]

    else
        button
            [ class "bg-purple-600 hover:bg-purple-700 text-white px-4 py-2 rounded"
            , onClick SubmitToCSG
            ]
            [ text "Submit to CSG" ]


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
            if isValid && String.length areaCode == 3 && String.length officeCode == 3 && String.length stationCode == 4 then
                "(" ++ areaCode ++ ") " ++ officeCode ++ "-" ++ stationCode

            else
                areaCode ++ officeCode ++ stationCode

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
            let
                _ =
                    if field.id == "medicare_information_claim_number" then
                        Debug.log "medicare_information_claim_number" field

                    else
                        field
            in
            case ( field.fieldType, field.id ) of
                ( ComplexPhoneField, _ ) ->
                    hasPhoneValue retrievedValue

                ( TextField _, "medicare_information_claim_number" ) ->
                    isValidMBI (getValueString section.id field.id model.data)
                        |> Debug.log "isValidMBI"

                ( SSNField _, _ ) ->
                    isValidSSN (getValueString section.id field.id model.data)

                _ ->
                    getValue section.id field.id model.data
                        |> isJust

        isInvalid =
            isFieldVisible field section model.data
                && isRequired
                && not fieldValueValid
                && field.id
                /= "applicant_age"
                && field.id
                /= "type"
                && field.id
                /= "document"
    in
    isInvalid


validateSection : Model -> FormSection -> Bool
validateSection model section =
    let
        hasEmptyRequired =
            List.any
                (validateFieldValue model section)
                section.body
    in
    hasEmptyRequired


renderFormSection : Model -> FormSection -> Html Msg
renderFormSection model section =
    let
        hasVisibleFields =
            let
                visibleFields =
                    List.filter (\field -> isFieldVisible field section model.data) section.body
            in
            not (List.isEmpty visibleFields)

        _ =
            Debug.log "section.id visibleFields" ( section.id, hasVisibleFields )

        isExpanded =
            Dict.get section.id model.expandedSections
                |> Maybe.withDefault False

        hasEmptyRequiredFields =
            validateSection model section

        sectionClasses =
            "form-section mb-8 "
                ++ (if hasEmptyRequiredFields then
                        "invalid "

                    else
                        ""
                   )
                ++ (if isExpanded then
                        "expanded"

                    else
                        ""
                   )

        warningBadge =
            if hasEmptyRequiredFields then
                div [ class "section-warning-badge" ]
                    [ text "⚠️ Required fields missing" ]

            else
                text ""

        headerContent =
            div [ class "section-header-content" ]
                [ h2 [ class "section-header" ]
                    [ text section.title ]
                , div [ class "flex items-center gap-4" ]
                    [ warningBadge
                    , span
                        [ class <|
                            "section-caret"
                                ++ (if isExpanded then
                                        " rotate-180"

                                    else
                                        ""
                                   )
                        ]
                        [ text "▼" ]
                    ]
                ]
    in
    if hasVisibleFields then
        div [ class sectionClasses ]
            [ div
                [ class "section-clickable-area"
                , onClick (ToggleSection section.id)
                ]
                [ if isExpanded then
                    div [ class "section-header-wrapper" ] [ headerContent ]

                  else
                    headerContent
                ]
            , if isExpanded then
                div [ class "section-content" ]
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
            not (validateFieldValue model section field)

        hiddenFields =
            [ "med_heading", "medication_heading_2" ]

        shouldHighlight =
            not fieldValueValid

        baseInputClass =
            "form-input"

        labelClass =
            "form-label"
                ++ (if shouldHighlight then
                        " invalid-label"

                    else
                        ""
                   )
                ++ (if isRequired then
                        " required"

                    else
                        ""
                   )

        displayLabel =
            case field.fieldType of
                TextBlockField _ ->
                    text ""

                _ ->
                    text field.displayLabel

        wrapperClass =
            case field.fieldType of
                HeadingField _ ->
                    "mb-8 mt-16"

                _ ->
                    "mb-6"

        fieldWrapper content =
            if shouldHighlight then
                div [ class "field-wrapper invalid p-4" ]
                    [ content ]

            else
                content

        debugLabel =
            if model.showDebugFields then
                div [ class "text-xs text-cyber-muted mb-1" ]
                    [ text (section.id ++ "." ++ field.id) ]

            else
                text ""

        errorMessage =
            if shouldHighlight then
                div [ class "invalid-message" ]
                    [ text "This field is required" ]

            else
                text ""
    in
    if isFieldVisible field section model.data && not (List.member field.id hiddenFields) then
        div [ class wrapperClass ]
            [ debugLabel
            , label [ class labelClass ]
                [ displayLabel ]
            , fieldWrapper
                (case field.fieldType of
                    TextNameValueField fv ->
                        div [ class "flex items-center p-2 bg-cyber-dark/50 border border-cyber-primary/30 rounded-md text-cyber-text" ]
                            [ span [ class "text-cyber-muted" ] [ text fv.displayLabel ]
                            , span [ class "ml-2" ] [ text fv.displayValue ]
                            ]

                    ComplexDatePickerField ->
                        div []
                            [ input
                                [ type_ "date"
                                , class baseInputClass
                                , value (getValueString section.id field.id model.data)
                                , onInput (UpdateField section.id field.id)
                                , onBlur SaveForm
                                ]
                                []
                            ]

                    NoDateDatePickerField ->
                        div []
                            [ input
                                [ type_ "date"
                                , class baseInputClass
                                , value (getValueString section.id field.id model.data)
                                , onInput (UpdateField section.id field.id)
                                , onBlur SaveForm
                                ]
                                []
                            ]

                    TextField config ->
                        div []
                            [ input
                                ([ type_ "text"
                                 , class baseInputClass
                                 , value
                                    (if field.id == "medicare_information_claim_number" then
                                        getValueString section.id field.id model.data
                                            |> String.filter Char.isAlphaNum
                                            |> String.toUpper

                                     else
                                        getValueString section.id field.id model.data
                                    )
                                 , onInput
                                    (if field.id == "medicare_information_claim_number" then
                                        \input ->
                                            let
                                                cleanInput =
                                                    String.filter Char.isAlphaNum input
                                                        |> String.toUpper
                                            in
                                            UpdateField section.id field.id cleanInput

                                     else
                                        UpdateField section.id field.id
                                    )
                                 , onBlur SaveForm
                                 ]
                                    ++ (if field.id == "medicare_information_claim_number" then
                                            [ Html.Attributes.pattern "[1-9][A-HJ-KMNP-RT-Y][A-HJ-KMNP-RT-Y0-9][0-9][A-HJ-KMNP-RT-Y][A-HJ-KMNP-RT-Y0-9][0-9][A-HJ-KMNP-RT-Y][A-HJ-KMNP-RT-Y][0-9][0-9]"
                                            , Html.Attributes.title "Please enter a valid Medicare Beneficiary Identifier (MBI)"
                                            ]

                                        else
                                            []
                                       )
                                    ++ (Maybe.map (\maxLen -> [ Html.Attributes.maxlength maxLen ]) config.maxLength
                                            |> Maybe.withDefault []
                                       )
                                )
                                []
                            ]

                    StringSearchField config ->
                        div [ class "space-y-2" ]
                            [ div []
                                [ input
                                    [ type_ "text"
                                    , class baseInputClass
                                    , value (getValueString section.id field.id model.data)
                                    , onInput (UpdateField section.id field.id)
                                    , Maybe.map (\maxLen -> Html.Attributes.maxlength maxLen) config.maxLength
                                        |> Maybe.withDefault (class "")
                                    , onBlur SaveForm
                                    ]
                                    []
                                ]
                            , div [ class "mt-2" ]
                                (List.map
                                    (\childField -> renderFormField model section childField)
                                    config.childFields
                                )
                            ]

                    ComplexPhoneField ->
                        div []
                            [ input
                                [ type_ "tel"
                                , class baseInputClass
                                , value (formatPhoneNumber (getPhoneValue section.id field.id model.data))
                                , onInput
                                    (\input ->
                                        let
                                            digits =
                                                String.filter Char.isDigit input
                                                    |> String.left 10

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
                                , Html.Attributes.placeholder "(555) 555-5555"
                                , Html.Attributes.pattern "[0-9]*"
                                , Html.Attributes.maxlength 14
                                , onBlur SaveForm
                                ]
                                []
                            , div [ class "text-xs text-gray-500 mt-1" ]
                                [ text "Format: (555) 555-5555" ]
                            ]

                    SimpleEmailField config ->
                        div []
                            [ input
                                [ type_ "email"
                                , class baseInputClass
                                , value (getValueString section.id field.id model.data)
                                , onInput (UpdateField section.id field.id)
                                , Html.Attributes.maxlength config.maxLength
                                , onBlur SaveForm
                                ]
                                []
                            ]

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
                                    , value (getValueString section.id field.id model.data)
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
                                , value (getValueString section.id field.id model.data)
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
                        div []
                            [ div [ class "flex space-x-2" ]
                                [ input
                                    [ type_ "number"
                                    , class baseInputClass
                                    , Html.Attributes.min (String.fromInt config.minimumValue)
                                    , Html.Attributes.max (String.fromInt config.maximumValue)
                                    , value (getValueString section.id field.id model.data)
                                    , onInput (UpdateField section.id field.id)
                                    , onBlur SaveForm
                                    ]
                                    []
                                , span [ class "self-center text-cyber-primary" ] [ text config.displayType ]
                                ]
                            ]

                    WeightField config ->
                        div []
                            [ input
                                [ type_ "number"
                                , class baseInputClass
                                , Html.Attributes.min (String.fromInt config.minimumValue)
                                , Html.Attributes.max (String.fromInt config.maximumValue)
                                , value (getValueString section.id field.id model.data)
                                , onInput (UpdateField section.id field.id)
                                , onBlur SaveForm
                                ]
                                []
                            ]

                    HeadingField _ ->
                        text ""

                    SSNField _ ->
                        let
                            currentValue =
                                getValueString section.id field.id model.data
                                    |> formatSSN
                                    |> Debug.log "currentValue SSNField"
                        in
                        div []
                            [ input
                                [ type_ "text"
                                , class baseInputClass
                                , value currentValue
                                , onInput
                                    (\input ->
                                        let
                                            digits =
                                                String.filter Char.isDigit input
                                                    |> String.left 9

                                            -- Don't clean/format the input when storing it
                                            -- Just store the raw digits
                                        in
                                        UpdateField section.id field.id digits
                                    )
                                , Html.Attributes.placeholder "XXX-XX-XXXX"
                                , Html.Attributes.pattern "[0-9]*"
                                , Html.Attributes.maxlength 11 -- Allow for formatting characters
                                , onBlur SaveForm
                                ]
                                []
                            , div [ class "text-xs text-gray-500 mt-1" ]
                                [ text "Format: XXX-XX-XXXX" ]
                            ]

                    ComplexRadioField config ->
                        div [ class "space-y-4" ]
                            [ div [ class "space-y-2" ]
                                (List.map
                                    (\childField -> renderFormField model section childField)
                                    config.childFields
                                )
                            ]

                    TextBlockField _ ->
                        div [ class "bg-cyber-dark/50 p-4 rounded-md border border-cyber-primary/30 text-cyber-text" ]
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
                                                , class "form-checkbox text-cyber-primary border-cyber-primary/50 rounded focus:ring-cyber-primary/50"
                                                , onBlur SaveForm
                                                ]
                                                []
                                            , span [ class "text-cyber-text" ] [ text opt.key ]
                                            ]
                                    )
                                    options1
                                )

                        else
                            div [ class "relative" ]
                                [ select
                                    [ class (baseInputClass ++ " appearance-none")
                                    , value (getValueString section.id field.id model.data)
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
                                , value (getValueString section.id field.id model.data)
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
                                [ class "cyber-button"
                                , type_ "button"
                                ]
                                [ text config.btnDisplayValue ]
                            ]

                    FileUploadField ->
                        div []
                            [ input
                                [ type_ "file"
                                , class baseInputClass
                                , onInput (UpdateField section.id field.id)
                                , onBlur SaveForm
                                ]
                                []
                            ]

                    RadioField options ->
                        div [ class "radio-group" ]
                            (List.map
                                (\opt ->
                                    let
                                        optionValue =
                                            stringifyJValue opt.value

                                        isSelected =
                                            Just opt.value == getValue section.id field.id model.data
                                    in
                                    label
                                        [ class <|
                                            "form-radio-label"
                                                ++ (if isSelected then
                                                        " selected"

                                                    else
                                                        ""
                                                   )
                                        ]
                                        [ input
                                            [ type_ "radio"
                                            , name (section.id ++ "." ++ field.id)
                                            , value optionValue
                                            , checked isSelected
                                            , onClick (UpdateField section.id field.id optionValue)
                                            , class "form-radio"
                                            , onBlur SaveForm
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
                                    inline-flex items-center gap-3 px-4 py-2.5
                                    bg-indigo-50 text-indigo-700
                                    hover:bg-indigo-100
                                    rounded-lg transition-all duration-200
                                    focus:outline-none focus:ring-2 
                                    focus:ring-indigo-500/50
                                    shadow-sm
                                  """
                                ]
                                [ div [ class "flex items-center gap-2" ]
                                    [ span [ class "text-indigo-500" ] [ text "🔗" ]
                                    , span [ class "font-medium" ] [ text "Link" ]
                                    , span [ class "text-indigo-400" ] [ text "↗" ]
                                    ]
                                ]
                            ]
                )
            , errorMessage
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
    Sub.batch
        [ saveApplicationResponse SaveFormResponse
        , getLAProTokenResponse GotLAProToken
        , submitToCSGResponse CSGSubmissionResponse

        --, Time.every 5000 CheckForUnsavedChanges -- Changed from 1000 to 5000
        ]



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

        -- Drug search and form
        , if showForm then
            div [ class "space-y-4" ]
                [ -- Drug search input with autocomplete
                  div [ class "relative" ]
                    [ div [ class "relative" ]
                        [ input
                            [ class "form-input w-full"
                            , type_ "text"
                            , placeholder "Search drug by name"
                            , onInput SearchDrugs
                            , value (Dict.get "drugName" model.medicationForm |> Maybe.withDefault "")
                            , onBlur SaveForm
                            ]
                            []
                        , if model.isSearching then
                            div [ class "absolute inset-y-0 right-0 flex items-center pr-3" ]
                                [ div [ class "animate-spin h-5 w-5 text-gray-400" ]
                                    [ -- Loading spinner SVG
                                      Html.node "svg"
                                        [ class "animate-spin h-5 w-5 text-gray-400"
                                        , attribute "xmlns" "http://www.w3.org/2000/svg"
                                        , attribute "fill" "none"
                                        , attribute "viewBox" "0 0 24 24"
                                        ]
                                        [ Html.node "circle"
                                            [ class "opacity-25"
                                            , attribute "cx" "12"
                                            , attribute "cy" "12"
                                            , attribute "r" "10"
                                            , attribute "stroke" "currentColor"
                                            , attribute "stroke-width" "4"
                                            ]
                                            []
                                        , Html.node "path"
                                            [ class "opacity-75"
                                            , attribute "fill" "currentColor"
                                            , attribute "d" "M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                                            ]
                                            []
                                        ]
                                    ]
                                ]

                          else
                            text ""
                        ]
                    , case model.searchError of
                        Just error ->
                            div [ class "absolute mt-1 w-full text-sm text-red-600" ]
                                [ text error ]

                        Nothing ->
                            if not (List.isEmpty model.drugSearchResults) then
                                div
                                    [ class """absolute z-10 w-full mt-1 bg-white shadow-lg 
                                             max-h-60 rounded-md py-1 text-base overflow-auto
                                             focus:outline-none sm:text-sm border border-gray-200"""
                                    ]
                                    [ ul [ class "divide-y divide-gray-200" ]
                                        (List.map
                                            (\result ->
                                                li
                                                    [ class """px-4 py-2 hover:bg-purple-50 cursor-pointer
                                                             text-gray-900 select-none relative"""
                                                    , onClick (SelectDrug result.value)
                                                    ]
                                                    [ text result.value ]
                                            )
                                            model.drugSearchResults
                                        )
                                    ]

                            else if String.length (Dict.get "drugName" model.medicationForm |> Maybe.withDefault "") > 2 && not model.isSearching && List.isEmpty model.drugSearchResults && model.selectedDrug == Nothing then
                                div
                                    [ class "absolute z-10 w-full mt-1 bg-white shadow-lg rounded-md py-4 text-center text-gray-500 border border-gray-200" ]
                                    [ text "No results found" ]

                            else
                                text ""
                    ]

                -- Loading state
                , if model.loadingDrugData then
                    div [ class "flex items-center justify-center py-4" ]
                        [ div [ class "animate-spin rounded-full h-8 w-8 border-b-2 border-purple-500" ] []
                        , span [ class "ml-2 text-sm text-gray-600" ] [ text "Loading dosage data..." ]
                        ]

                  else
                    text ""

                -- Selected drug form
                , case model.selectedDrug of
                    Just drugName ->
                        div [ class "space-y-4 p-4 border border-gray-200 rounded-md" ]
                            [ -- Dosage selection
                              div [ class "space-y-2" ]
                                [ label [ class "block text-sm font-medium text-gray-700" ]
                                    [ text "Select Dosage" ]
                                , div [ class "space-y-2" ]
                                    (model.drugDosages
                                        |> List.foldr
                                            (\dosage acc ->
                                                if List.any (\d -> d.drugname == dosage.drugname) acc then
                                                    acc

                                                else
                                                    dosage :: acc
                                            )
                                            []
                                        |> List.map
                                            (\dosage ->
                                                label [ class "flex items-center space-x-3" ]
                                                    [ input
                                                        [ type_ "radio"
                                                        , name "dosage"
                                                        , value dosage.dosageid
                                                        , onClick (UpdateMedicationField "" "dosageId" dosage.dosageid)
                                                        , class "form-radio"
                                                        , onBlur SaveForm
                                                        ]
                                                        []
                                                    , span [ class "text-sm text-gray-900" ]
                                                        [ text dosage.drugname ]
                                                    ]
                                            )
                                    )
                                ]

                            -- Common fields
                            , formField model "Diagnosis" "diagnosis" "text"
                            , div [ class "grid grid-cols-2 gap-4" ]
                                [ formField model "Quantity" "quantity" "number"
                                , div [ class "space-y-2" ]
                                    [ label [ class "block text-sm font-medium text-gray-700" ]
                                        [ text "Frequency" ]
                                    , select
                                        [ class "form-input"
                                        , onInput (UpdateMedicationField "" "frequency")
                                        ]
                                        [ option [ value "" ] [ text "Select frequency" ]
                                        , option [ value "30 Days" ] [ text "30 Days" ]
                                        , option [ value "60 Days" ] [ text "60 Days" ]
                                        , option [ value "90 Days" ] [ text "90 Days" ]
                                        , option [ value "6 Months" ] [ text "6 Months" ]
                                        , option [ value "1 Year" ] [ text "1 Year" ]
                                        ]
                                    ]
                                ]

                            -- Date fields
                            , formField model "Last Fill Date" "lastFillDate" "date"
                            , formField model "Start Date" "medStartDate" "date"

                            -- Form actions
                            , div [ class "flex justify-end space-x-4" ]
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

                    Nothing ->
                        text ""
                ]

          else
            text ""
        ]


formField : Model -> String -> String -> String -> Html Msg
formField model label_ fieldName inputType =
    div [ class "space-y-2" ]
        [ label [ class "block text-sm font-medium text-gray-700" ]
            [ text label_ ]
        , input
            [ class "form-input w-full"
            , type_ inputType
            , onInput (UpdateMedicationField "" fieldName)
            , value (Dict.get fieldName model.medicationForm |> Maybe.withDefault "")
            , onBlur SaveForm
            ]
            []
        ]



-- Add new types for drug search


type alias DrugSearchResult =
    { key : String
    , value : String
    }


type alias DrugInfo =
    { dosageid : String
    , drugname : String
    , ndccode : String
    }



-- Add HTTP functions for drug search


searchDrugs : String -> String -> Cmd Msg
searchDrugs token query =
    if String.length query > 2 then
        Http.request
            { method = "POST"
            , headers =
                [ Http.header "Authorization" token
                , Http.header "Content-Type" "application/json"
                , Http.header "Accept" "application/json"
                ]
            , url = "https://api.leadadvantagepro.com/api/drug/getDrugNames"
            , body = Http.jsonBody (Encode.object [ ( "drug_name", Encode.string query ) ])
            , expect = Http.expectJson DrugSearchResponse drugSearchResultsDecoder
            , timeout = Just 10000 -- 10 second timeout
            , tracker = Nothing
            }

    else
        Cmd.none


getDrugDosages : String -> String -> Cmd Msg
getDrugDosages token drugName =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Authorization" token
            , Http.header "Content-Type" "application/json"
            , Http.header "Accept" "application/json"
            ]
        , url = "https://api.leadadvantagepro.com/api/drug/searchByDrugName"
        , body = Http.jsonBody (Encode.object [ ( "drug_name", Encode.string drugName ) ])
        , expect = Http.expectJson DrugDosageResponse drugDosageListDecoder
        , timeout = Just 10000 -- 10 second timeout
        , tracker = Nothing
        }



-- Add decoders for drug search responses


drugSearchResultsDecoder : Decoder (List DrugSearchResult)
drugSearchResultsDecoder =
    Decode.list
        (Decode.string
            |> Decode.map
                (\name ->
                    { key = String.toLower name
                    , value = name
                    }
                )
        )


drugDosageListDecoder : Decoder (List DrugInfo)
drugDosageListDecoder =
    Decode.list drugInfoDecoder


drugInfoDecoder : Decoder DrugInfo
drugInfoDecoder =
    Decode.succeed DrugInfo
        |> Pipeline.required "dosageid" Decode.string
        |> Pipeline.required "drugname" Decode.string
        |> Pipeline.required "ndccode" Decode.string


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


createNewMedication : Model -> String -> Medication
createNewMedication model baseId =
    let
        selectedDosage =
            Dict.get "dosageId" model.medicationForm
                |> Maybe.andThen
                    (\dosageId ->
                        List.filter (\d -> d.dosageid == dosageId) model.drugDosages
                            |> List.head
                    )
    in
    { drug =
        { gpI10 = "" -- This will be set by the GPI10 search
        , productName = Maybe.map .drugname selectedDosage |> Maybe.withDefault ""
        , drugName = Maybe.map .drugname selectedDosage |> Maybe.withDefault ""
        , displayName = Maybe.map .drugname selectedDosage |> Maybe.withDefault ""
        }
    , diagnosis = Dict.get "diagnosis" model.medicationForm |> Maybe.withDefault ""
    , dosage =
        { dosage = extractDosage (Maybe.map .drugname selectedDosage |> Maybe.withDefault "")
        , ndc = Maybe.map .ndccode selectedDosage |> Maybe.withDefault ""
        }
    , frequency = Dict.get "frequency" model.medicationForm |> Maybe.withDefault ""
    , quantity =
        Dict.get "quantity" model.medicationForm
            |> Maybe.andThen String.toInt
    , lastFillDate = Dict.get "lastFillDate" model.medicationForm |> Maybe.withDefault ""
    , medStartDate = Dict.get "medStartDate" model.medicationForm |> Maybe.withDefault ""
    }



-- Helper function to extract dosage from drug name


extractDosage : String -> String
extractDosage drugName =
    let
        parts =
            String.split " " drugName

        dosageParts =
            List.filter (\part -> String.toUpper part == part) parts
    in
    case List.head dosageParts of
        Just dosage ->
            if dosage == "SOL" then
                "SOLN"

            else
                dosage

        Nothing ->
            "SOLN"


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
                [ ( "drug"
                  , Encode.object
                        [ ( "gpI10", Encode.string med.drug.gpI10 )
                        , ( "productName", Encode.string med.drug.productName )
                        , ( "drugName", Encode.string med.drug.drugName )
                        , ( "displayName", Encode.string med.drug.displayName )
                        ]
                  )
                , ( "diagnosis", Encode.string med.diagnosis )
                , ( "dosage"
                  , Encode.object
                        [ ( "dosage", Encode.string med.dosage.dosage )
                        , ( "ndc", Encode.string med.dosage.ndc )
                        ]
                  )
                , ( "frequency", Encode.string med.frequency )
                , ( "quantity"
                  , case med.quantity of
                        Just q ->
                            Encode.int q

                        Nothing ->
                            Encode.null
                  )
                , ( "lastFillDate", Encode.string med.lastFillDate )
                , ( "medStartDate", Encode.string med.medStartDate )
                ]
        )
        medications


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


transformAllstateMedications : Dict String JsonValue -> Dict String JsonValue
transformAllstateMedications medicationInfo =
    let
        prescriptionDrugList =
            Dict.get "prescription_drug_list" medicationInfo
                |> Maybe.andThen
                    (\value ->
                        case value of
                            JsonArray arr ->
                                Just arr

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault []

        maybeSplit : String -> Maybe ( String, String )
        maybeSplit fullName =
            fullName
                |> String.split " "
                |> List.Extra.splitWhen (not << isUpper)
                |> Maybe.map (\( a, b ) -> ( String.join " " a, String.join " " b ))

        prescribedMedications : List JsonValue
        prescribedMedications =
            prescriptionDrugList
                |> List.map
                    (\object ->
                        case object of
                            JsonObject dic ->
                                let
                                    maybeFullName =
                                        Dict.get "drugName" dic

                                    diagnosis =
                                        Dict.get "diagnosis" dic
                                            |> Maybe.withDefault (JsonBase NullValue)

                                    frequency =
                                        Dict.get "frequency" dic
                                            |> Maybe.withDefault (JsonBase NullValue)

                                    prescriptionFreqOther =
                                        Dict.get "quantity" dic
                                            |> Maybe.withDefault (JsonBase NullValue)
                                in
                                case maybeFullName of
                                    Just (JsonBase (StringValue fullName)) ->
                                        case maybeSplit fullName of
                                            Just ( medName, dosage ) ->
                                                JsonObject
                                                    ([ ( "med_name", medName |> (StringValue >> JsonBase) )
                                                     , ( "diagnosis", diagnosis )
                                                     , ( "dosage", dosage |> (StringValue >> JsonBase) )
                                                     , ( "frequency", frequency )
                                                     , ( "prescription_frequency_other", prescriptionFreqOther )
                                                     , ( "using", JsonBase (BoolValue True) )
                                                     ]
                                                        |> List.filter (\( _, v ) -> v /= JsonBase NullValue)
                                                        |> Dict.fromList
                                                    )

                                            Nothing ->
                                                JsonBase NullValue

                                    _ ->
                                        JsonBase NullValue

                            _ ->
                                JsonBase NullValue
                    )

        lastMedication : JsonValue
        lastMedication =
            List.Extra.last prescribedMedications
                |> Maybe.andThen
                    (\med ->
                        case med of
                            JsonObject medDict ->
                                Dict.get "med_name" medDict
                                    |> Maybe.andThen
                                        (\name ->
                                            case name of
                                                JsonBase NullValue ->
                                                    Nothing

                                                JsonBase (StringValue _) ->
                                                    Just name

                                                _ ->
                                                    Nothing
                                        )

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault (JsonBase NullValue)

        lastDosage : JsonValue
        lastDosage =
            List.Extra.last prescribedMedications
                |> Maybe.andThen
                    (\med ->
                        case med of
                            JsonObject medDict ->
                                Dict.get "dosage" medDict
                                    |> Maybe.andThen
                                        (\value ->
                                            case value of
                                                JsonBase NullValue ->
                                                    Nothing

                                                JsonBase (StringValue _) ->
                                                    Just value

                                                _ ->
                                                    Nothing
                                        )

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault (JsonBase NullValue)
    in
    medicationInfo
        -- |> Dict.remove "prescription_drug_list"
        |> Dict.insert "prescribed_medications" (JsonArray prescribedMedications)
        |> insertIfNotNull "med_name" lastMedication
        |> insertIfNotNull "dosage" lastDosage


splitMedNameAndDosage : String -> ( String, String )
splitMedNameAndDosage fullName =
    let
        parts =
            String.split " " fullName

        ( medNameParts, dosageParts ) =
            List.foldr
                (\part ( nameAcc, dosageAcc, foundUpper ) ->
                    if String.toUpper part == part then
                        ( nameAcc, part :: dosageAcc, True )

                    else if foundUpper then
                        ( nameAcc, part :: dosageAcc, foundUpper )

                    else
                        ( part :: nameAcc, dosageAcc, foundUpper )
                )
                ( [], [], False )
                parts
                |> (\( n, d, _ ) -> ( n, d ))
    in
    ( String.join " " medNameParts
    , String.join " " dosageParts
    )


isUpper : String -> Bool
isUpper str =
    String.toUpper str == str


insertIfNotNull : String -> JsonValue -> Dict String JsonValue -> Dict String JsonValue
insertIfNotNull key value dict =
    case value of
        JsonBase NullValue ->
            dict

        _ ->
            Dict.insert key value dict


transformAetnaMedications : Dict String JsonValue -> Dict String JsonValue
transformAetnaMedications healthHistory =
    let
        prescriptionDrugList =
            Dict.get "prescription_drug_list" healthHistory
                |> Maybe.andThen
                    (\value ->
                        case value of
                            JsonArray arr ->
                                Just arr

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault []
                |> Debug.log "Prescription drug list for Aetna transformation"

        nameTransform : String -> String
        nameTransform fullName =
            fullName
                |> String.split " "
                |> List.Extra.takeWhile (not << isUpper)
                |> String.join " "

        prescribedMedications =
            prescriptionDrugList
                |> List.indexedMap
                    (\index object ->
                        case object of
                            JsonObject dic ->
                                let
                                    maybeDrug =
                                        Dict.get "drug" dic
                                in
                                (case maybeDrug of
                                    Just (JsonObject drug) ->
                                        let
                                            maybeFullName =
                                                Dict.get "drugName" drug

                                            maybeDiagnosis =
                                                Dict.get "diagnosis" dic
                                        in
                                        case ( maybeFullName, maybeDiagnosis ) of
                                            ( Just (JsonBase (StringValue fullName)), Just (JsonBase (StringValue diagnosis)) ) ->
                                                JsonObject
                                                    (Dict.fromList
                                                        [ ( "med_name", JsonBase (StringValue (nameTransform fullName)) )
                                                        , ( "diagnosis", JsonBase (StringValue diagnosis) )
                                                        ]
                                                    )

                                            _ ->
                                                JsonBase NullValue

                                    _ ->
                                        JsonBase NullValue
                                )
                                    |> Tuple.pair (String.fromInt index)

                            _ ->
                                ( String.fromInt index, JsonBase NullValue )
                    )
                |> List.filter (\( _, value ) -> value /= JsonBase NullValue)

        lastMedication : JsonValue
        lastMedication =
            List.Extra.last prescribedMedications
                |> Maybe.andThen
                    (\( _, med ) ->
                        case med of
                            JsonObject medDict ->
                                Dict.get "med_name" medDict
                                    |> Maybe.andThen
                                        (\name ->
                                            case name of
                                                JsonBase NullValue ->
                                                    Nothing

                                                JsonBase (StringValue _) ->
                                                    Just name

                                                _ ->
                                                    Nothing
                                        )

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault (JsonBase NullValue)
    in
    healthHistory
        -- |> Dict.remove "prescription_drug_list"
        |> Dict.insert "prescribed_medications" (JsonObject (Dict.fromList prescribedMedications))
        |> insertIfNotNull "med_name" lastMedication


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


applicationViewDecoder : Decode.Decoder Application
applicationViewDecoder =
    Decode.succeed Application
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "naic" Decode.string
        |> Pipeline.required "data" Decode.value
        |> Pipeline.required "formattedData" Decode.value
        |> Pipeline.required "schema" (Decode.field "sections" CSGSchema.formSchemaDecoder)


updateModelDataWithMedications : Maybe Carrier -> List Medication -> JsonValue -> JsonValue
updateModelDataWithMedications carrier medications data =
    let
        baseUpdate =
            setComplexValue
                "medication_information"
                "prescription_drug_list"
                (JsonArray (List.map medicationToJsonValue medications))
                data

        medicationSection =
            getSection "medication_information" baseUpdate

        _ =
            Debug.log "** carrier **" carrier
    in
    case carrier of
        Just Aetna ->
            let
                transformedHealthHistory =
                    transformAetnaMedications (getSection "health_history" baseUpdate)
            in
            setSection "health_history" transformedHealthHistory baseUpdate

        Just Allstate ->
            setSection "medication_information" (transformAllstateMedications medicationSection) baseUpdate

        _ ->
            baseUpdate


medicationToJsonValue : Medication -> JsonValue
medicationToJsonValue med =
    JsonObject
        (Dict.fromList
            [ ( "drug"
              , JsonObject
                    (Dict.fromList
                        [ ( "gpI10", JsonBase (StringValue med.drug.gpI10) )
                        , ( "productName", JsonBase (StringValue med.drug.productName) )
                        , ( "drugName", JsonBase (StringValue med.drug.drugName) )
                        , ( "displayName", JsonBase (StringValue med.drug.displayName) )
                        ]
                    )
              )
            , ( "diagnosis", JsonBase (StringValue med.diagnosis) )
            , ( "dosage"
              , JsonObject
                    (Dict.fromList
                        [ ( "dosage", JsonBase (StringValue med.dosage.dosage) )
                        , ( "ndc", JsonBase (StringValue med.dosage.ndc) )
                        ]
                    )
              )
            , ( "frequency", JsonBase (StringValue med.frequency) )
            , ( "quantity"
              , case med.quantity of
                    Just q ->
                        JsonBase (IntValue q)

                    Nothing ->
                        JsonBase NullValue
              )
            , ( "lastFillDate", JsonBase (StringValue med.lastFillDate) )
            , ( "medStartDate", JsonBase (StringValue med.medStartDate) )
            ]
        )



-- Add these helper functions
-- Add this helper function near other helper functions


formatPhoneNumber : String -> String
formatPhoneNumber phoneStr =
    let
        digits =
            String.filter Char.isDigit phoneStr
    in
    if String.length digits >= 7 then
        "(" ++ String.left 3 digits ++ ") " ++ String.slice 3 6 digits ++ "-" ++ String.slice 6 10 digits

    else if String.length digits >= 4 then
        "(" ++ String.left 3 digits ++ ") " ++ String.slice 3 6 digits

    else if String.length digits > 0 then
        "(" ++ String.left 3 digits

    else
        ""


formatSSN : String -> String
formatSSN ssnStr =
    let
        digits =
            String.filter Char.isDigit ssnStr
    in
    [ String.left 3 digits
    , String.slice 3 5 digits
    , String.slice 5 9 digits
    ]
        |> List.filterMap
            (\str ->
                case str of
                    "" ->
                        Nothing

                    _ ->
                        Just str
            )
        |> String.join "-"


isValidSSN : String -> Bool
isValidSSN ssn =
    let
        digits =
            String.filter Char.isDigit ssn

        invalidPrefixes =
            [ "000", "666", "9" ]

        invalidFullSSNs =
            [ "000000000"
            , "111111111"
            , "222222222"
            , "333333333"
            , "444444444"
            , "555555555"
            , "666666666"
            , "777777777"
            , "888888888"
            , "999999999"
            , "123456789"
            ]

        prefix =
            String.left 3 digits

        hasValidLength =
            String.length digits == 9

        hasValidPrefix =
            not (List.any (\p -> String.startsWith p prefix) invalidPrefixes)

        isNotInvalidSSN =
            not (List.member digits invalidFullSSNs)
    in
    hasValidLength && hasValidPrefix && isNotInvalidSSN


isValidMBI : String -> Bool
isValidMBI mbi =
    let
        mbiRegex =
            [ "^" -- Start of string
            , "[1-9]" -- Position 1: numeric 1-9
            , "[AC-HJ-KMNP-RT-Y]" -- Position 2: letter (excluding S, L, O, I, B, Z)
            , "[AC-HJ-KMNP-RT-Y0-9]" -- Position 3: letter or number
            , "[0-9]" -- Position 4: numeric 0-9
            , "[AC-HJ-KMNP-RT-Y]" -- Position 5: letter
            , "[AC-HJ-KMNP-RT-Y0-9]" -- Position 6: letter or number
            , "[0-9]" -- Position 7: numeric 0-9
            , "[AC-HJ-KMNP-RT-Y]" -- Position 8: letter
            , "[AC-HJ-KMNP-RT-Y]" -- Position 9: letter
            , "[0-9]" -- Position 10: numeric 0-9
            , "[0-9]" -- Position 11: numeric 0-9
            , "$" -- End of string
            ]
                |> String.join ""
                |> Regex.fromString
                |> Maybe.withDefault Regex.never

        cleanMBI =
            String.filter Char.isAlphaNum mbi
                |> String.toUpper
    in
    Regex.contains mbiRegex cleanMBI
