port module ApplicationView exposing (Application, Model, Msg(..), init, subscriptions, update, view)

import CSGSchema exposing (ApplicationSchema, FormField, FormFieldType(..), FormSection, JValue(..), JsonValue(..), RequiredType(..), encodeFormValues, isFieldVisible, parseValue)
import DataEncoder exposing (unflattenData)
import Date exposing (Date, Unit(..))
import Debug
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task
import Time exposing (Month(..))



-- Port for saving application data


port saveApplication : { id : String, data : Encode.Value } -> Cmd msg



-- Port for receiving save response


port saveApplicationResponse : ({ success : Bool, error : Maybe String } -> msg) -> Sub msg


type alias Model =
    { flatData : Dict String JValue
    , medications : Dict String (List MedicationType)
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
    | UpdateField String String
    | SaveForm
    | SaveFormResponse { success : Bool, error : Maybe String }
    | GotCurrentTime Date
    | SubmitToCSG
    | NoOp
    | GotRoutingNumber (Result Http.Error String)
    | ToggleAddMedicationForm String
    | UpdateMedicationField String String String
    | SaveMedication String MedicationType
    | CancelAddMedication String
    | RemoveMedication String Int


type alias Application =
    { id : String
    , data : Decode.Value
    , schema : ApplicationSchema
    }


type alias FormattedData =
    { sections : Dict String (Dict String JValue) }



-- INIT


init : Application -> ( Model, Cmd Msg )
init app =
    let
        initialFormValues =
            app.data
                |> extractFormValues
                |> Debug.log "initialFormValues"

        initialMedications =
            extractMedications app.data
                |> Debug.log "initialMedications"

        model =
            { flatData = initialFormValues
            , medications = initialMedications
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


extractFormValues : Decode.Value -> Dict String JValue
extractFormValues jsonData =
    case Decode.decodeValue (Decode.dict Decode.value) jsonData of
        Ok dict ->
            Dict.foldl flattenJsonToDict Dict.empty dict

        Err _ ->
            Dict.empty


flattenJsonToDict : String -> Decode.Value -> Dict String JValue -> Dict String JValue
flattenJsonToDict prefix value dict =
    -- Try decoding as string first
    case Decode.decodeValue Decode.string value of
        Ok str ->
            Dict.insert prefix (StringValue str) dict

        Err _ ->
            -- Try decoding as int
            case Decode.decodeValue Decode.int value of
                Ok num ->
                    Dict.insert prefix (IntValue num) dict

                Err _ ->
                    -- Try decoding as float
                    case Decode.decodeValue Decode.float value of
                        Ok num ->
                            Dict.insert prefix (FloatValue num) dict

                        Err _ ->
                            -- Try decoding as bool
                            case Decode.decodeValue Decode.bool value of
                                Ok bool ->
                                    Dict.insert prefix (BoolValue bool) dict

                                Err _ ->
                                    -- Finally try as nested object
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

        UpdateField fieldId valueString ->
            let
                value =
                    parseValue valueString

                newFlatData =
                    Dict.insert fieldId value model.flatData

                cmd =
                    if fieldId == "payment.eft_routing_number" then
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
                | flatData = newFlatData
                , isValid = validateData model
              }
            , cmd
            )

        GotCurrentTime currentDate ->
            let
                underwritingType =
                    determineUnderwritingType model.flatData (Just currentDate)

                underwritingInt =
                    underwritingType |> Maybe.withDefault -1

                flatData =
                    Dict.insert "enrollment_application.underwriting_type" (IntValue underwritingInt) model.flatData
            in
            ( { model
                | currentDate = Just currentDate
                , flatData = flatData
                , underwritingType = underwritingType
              }
            , Cmd.none
            )

        SaveForm ->
            let
                encodedData =
                    unflattenData model.flatData
                        |> addMedicationsToJson model.medications

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
                        newFlatData =
                            Dict.insert "payment.eft_financial_institution_name" (StringValue institutionName) model.flatData
                    in
                    ( { model | flatData = newFlatData }
                    , Cmd.none
                    )

                Err error ->
                    ( model, Cmd.none )

        ToggleAddMedicationForm fieldId ->
            let
                showFormKey =
                    fieldId ++ ".showForm"

                currentShowForm =
                    Dict.get showFormKey model.flatData
                        |> Maybe.map unwrapBoolValue
                        |> Maybe.withDefault False

                newFlatData =
                    Dict.insert showFormKey (BoolValue (not currentShowForm)) model.flatData
            in
            ( { model | flatData = newFlatData }, Cmd.none )

        UpdateMedicationField baseId field value ->
            let
                tempKey =
                    baseId ++ ".temp." ++ field

                newFlatData =
                    Dict.insert tempKey (StringValue value) model.flatData
            in
            ( { model | flatData = newFlatData }, Cmd.none )

        SaveMedication baseId medication ->
            let
                tempFields =
                    [ "drugName", "diagnosis", "dosage", "frequency" ]
                        |> List.map
                            (\field ->
                                ( field
                                , Dict.get (baseId ++ ".temp." ++ field) model.flatData
                                    |> Maybe.map unwrapStringValue
                                    |> Maybe.withDefault ""
                                )
                            )
                        |> Dict.fromList

                newMedication =
                    { drugName = Dict.get "drugName" tempFields |> Maybe.withDefault ""
                    , diagnosis = Dict.get "diagnosis" tempFields |> Maybe.withDefault ""
                    , dosage = Dict.get "dosage" tempFields |> Maybe.withDefault ""
                    , frequency = Dict.get "frequency" tempFields |> Maybe.withDefault ""
                    , quantity = Nothing
                    }

                updatedMedications =
                    Dict.update baseId
                        (\maybeMeds ->
                            case maybeMeds of
                                Just meds ->
                                    Just (meds ++ [ Medication newMedication ])

                                Nothing ->
                                    Just [ Medication newMedication ]
                        )
                        model.medications

                newFlatData =
                    model.flatData
                        |> Dict.insert (baseId ++ ".showForm") (BoolValue False)
                        |> Dict.insert (baseId ++ ".taken_prescription_drugs") (BoolValue True)
            in
            ( { model
                | medications = updatedMedications
                , flatData = newFlatData
              }
            , Cmd.none
            )

        CancelAddMedication baseId ->
            let
                newFlatData =
                    Dict.insert (baseId ++ ".showForm") (BoolValue False) model.flatData
            in
            ( { model | flatData = newFlatData }, Cmd.none )

        RemoveMedication baseId idx ->
            let
                updatedMedications =
                    Dict.update baseId
                        (\maybeMeds ->
                            case maybeMeds of
                                Just meds ->
                                    Just (List.take idx meds ++ List.drop (idx + 1) meds)

                                Nothing ->
                                    Nothing
                        )
                        model.medications

                newFlatData =
                    model.flatData
                        |> Dict.insert (baseId ++ ".taken_prescription_drugs")
                            (BoolValue
                                (Dict.get baseId updatedMedications
                                    |> Maybe.map (not << List.isEmpty)
                                    |> Maybe.withDefault False
                                )
                            )
            in
            ( { model
                | medications = updatedMedications
                , flatData = newFlatData
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


validateSection : Model -> FormSection -> Bool
validateSection model section =
    let
        hasEmptyRequired =
            List.any
                (\field ->
                    let
                        isRequired =
                            case field.required of
                                RequiredBool bool ->
                                    bool

                                RequiredDependsOn _ ->
                                    True

                        fieldValue =
                            case field.fieldType of
                                ComplexPhoneField ->
                                    let
                                        basePath =
                                            section.id ++ "." ++ field.id

                                        hasValue =
                                            [ ".area_code", ".central_office_code", ".station_code" ]
                                                |> List.map (\suffix -> Dict.get (basePath ++ suffix) model.flatData)
                                                |> List.all (Maybe.map (not << String.isEmpty << stringifyJValue) >> Maybe.withDefault False)
                                    in
                                    if hasValue then
                                        "has-value"

                                    else
                                        ""

                                _ ->
                                    Dict.get (section.id ++ "." ++ field.id) model.flatData
                                        |> Maybe.map stringifyJValue
                                        |> Maybe.withDefault ""

                        isInvalid =
                            isFieldVisible field section model.flatData
                                && isRequired
                                && String.isEmpty fieldValue
                                && field.id
                                /= "applicant_age"
                    in
                    isInvalid
                )
                section.body
    in
    hasEmptyRequired


renderFormSection : Model -> FormSection -> Html Msg
renderFormSection model section =
    let
        hasVisibleFields =
            let
                visibleFields =
                    List.filter (\field -> isFieldVisible field section model.flatData) section.body

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

        fieldValue =
            case field.fieldType of
                ComplexPhoneField ->
                    let
                        basePath =
                            section.id ++ "." ++ field.id

                        hasValue =
                            [ ".area_code", ".central_office_code", ".station_code" ]
                                |> List.map (\suffix -> Dict.get (basePath ++ suffix) model.flatData)
                                |> List.all (Maybe.map (not << String.isEmpty << stringifyJValue) >> Maybe.withDefault False)
                    in
                    if hasValue then
                        "has-value"

                    else
                        ""

                _ ->
                    Dict.get (section.id ++ "." ++ field.id) model.flatData
                        |> Maybe.map stringifyJValue
                        |> Maybe.withDefault ""

        shouldHighlight =
            isRequired && String.isEmpty fieldValue && field.id /= "applicant_age"

        baseInputClass =
            """
            w-full px-4 py-3 border border-gray-300 rounded-lg
            focus:outline-none focus:ring-2 focus:ring-purple-500 focus:border-purple-500
            text-gray-700 transition-all duration-200
            hover:border-gray-400 text-base bg-white
            """

        labelClass =
            "block text-sm font-medium mb-2 text-gray-700 p-2"
                ++ (if shouldHighlight then
                        " bg-tokyo-orange/20"

                    else
                        " bg-white"
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
    if isFieldVisible field section model.flatData then
        div [ class wrapperClass ]
            [ debugLabel
            , label [ class labelClass ]
                [ text field.displayLabel ]
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
                        , value (Dict.get (section.id ++ "." ++ field.id) model.flatData |> stringifyMaybe)
                        , onInput (UpdateField (section.id ++ "." ++ field.id))
                        ]
                        []

                NoDateDatePickerField ->
                    input
                        [ type_ "date"
                        , class baseInputClass
                        , value (Dict.get (section.id ++ "." ++ field.id) model.flatData |> stringifyMaybe)
                        , onInput (UpdateField (section.id ++ "." ++ field.id))
                        ]
                        []

                TextField config ->
                    input
                        [ type_ "text"
                        , class baseInputClass
                        , value (Dict.get (section.id ++ "." ++ field.id) model.flatData |> stringifyMaybe)
                        , onInput (UpdateField (section.id ++ "." ++ field.id))
                        , Maybe.map (\maxLen -> Html.Attributes.maxlength maxLen) config.maxLength
                            |> Maybe.withDefault (class "")
                        ]
                        []

                StringSearchField config ->
                    div [ class "space-y-2" ]
                        [ input
                            [ type_ "text"
                            , class baseInputClass
                            , value (Dict.get (section.id ++ "." ++ field.id) model.flatData |> stringifyMaybe)
                            , onInput (UpdateField (section.id ++ "." ++ field.id))
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
                        basePath =
                            section.id ++ "." ++ field.id

                        areaCode =
                            Dict.get (basePath ++ ".area_code") model.flatData |> stringifyMaybe

                        officeCode =
                            Dict.get (basePath ++ ".central_office_code") model.flatData |> stringifyMaybe

                        stationCode =
                            Dict.get (basePath ++ ".station_code") model.flatData |> stringifyMaybe

                        formattedValue =
                            if String.isEmpty areaCode && String.isEmpty officeCode && String.isEmpty stationCode then
                                ""

                            else
                                areaCode ++ "-" ++ officeCode ++ "-" ++ stationCode
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
                                in
                                if String.length digits <= 3 then
                                    UpdateField (basePath ++ ".area_code") digits

                                else if String.length digits <= 6 then
                                    UpdateField (basePath ++ ".central_office_code") newOfficeCode

                                else
                                    UpdateField (basePath ++ ".station_code") newStationCode
                            )
                        ]
                        []

                SimpleEmailField config ->
                    input
                        [ type_ "email"
                        , class baseInputClass
                        , value (Dict.get (section.id ++ "." ++ field.id) model.flatData |> stringifyMaybe)
                        , onInput (UpdateField (section.id ++ "." ++ field.id))
                        , Html.Attributes.maxlength config.maxLength
                        ]
                        []

                IntPickerField options ->
                    if field.id == "applicant_age" then
                        let
                            dobValue =
                                Dict.get "applicant_info.applicant_dob" model.flatData

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
                                , value (Dict.get (section.id ++ "." ++ field.id) model.flatData |> stringifyMaybe)
                                , onInput (UpdateField (section.id ++ "." ++ field.id))
                                ]
                                (option [ value "" ] [ text "Select..." ]
                                    :: List.map
                                        (\opt ->
                                            option
                                                [ value (String.fromInt opt)
                                                , selected (Dict.get (section.id ++ "." ++ field.id) model.flatData == Just (IntValue opt))
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
                            , value (Dict.get (section.id ++ "." ++ field.id) model.flatData |> stringifyMaybe)
                            , onInput (UpdateField (section.id ++ "." ++ field.id))
                            ]
                            (option [ value "" ] [ text "Select..." ]
                                :: List.map
                                    (\opt ->
                                        option
                                            [ value (stringifyJValue opt.value)
                                            , selected (Dict.get (section.id ++ "." ++ field.id) model.flatData == Just opt.value)
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
                            , value (Dict.get (section.id ++ "." ++ field.id) model.flatData |> stringifyMaybe)
                            , onInput (UpdateField (section.id ++ "." ++ field.id))
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
                        , value (Dict.get (section.id ++ "." ++ field.id) model.flatData |> stringifyMaybe)
                        , onInput (UpdateField (section.id ++ "." ++ field.id))
                        ]
                        []

                HeadingField _ ->
                    text ""

                SSNField _ ->
                    input
                        [ type_ "text"
                        , class baseInputClass
                        , value (Dict.get (section.id ++ "." ++ field.id) model.flatData |> stringifyMaybe)
                        , onInput (UpdateField (section.id ++ "." ++ field.id))
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
                                            , onInput (UpdateField (section.id ++ "." ++ field.id ++ "." ++ opt.id))
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
                                , value (Dict.get (section.id ++ "." ++ field.id) model.flatData |> stringifyMaybe)
                                , onInput (UpdateField (section.id ++ "." ++ field.id))
                                ]
                                (option [ value "" ] [ text "Select..." ]
                                    :: List.map
                                        (\opt ->
                                            option
                                                [ value (stringifyJValue opt.value)
                                                , selected (Dict.get (section.id ++ "." ++ field.id) model.flatData == Just opt.value)
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
                            , value (Dict.get (section.id ++ "." ++ field.id) model.flatData |> stringifyMaybe)
                            , onInput (UpdateField (section.id ++ "." ++ field.id))
                            ]
                            (option [ value "" ] [ text "Select..." ]
                                :: List.map
                                    (\opt ->
                                        option
                                            [ value (stringifyJValue opt.value)
                                            , selected (Dict.get (section.id ++ "." ++ field.id) model.flatData == Just opt.value)
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
                        , onInput (UpdateField (section.id ++ "." ++ field.id))
                        ]
                        []

                RadioField options ->
                    let
                        fieldPath =
                            section.id ++ "." ++ field.id

                        currentValue =
                            Dict.get fieldPath model.flatData
                    in
                    div [ class "space-y-2" ]
                        (List.map
                            (\opt ->
                                let
                                    optionValue =
                                        stringifyJValue opt.value

                                    currentValueStr =
                                        Maybe.map stringifyJValue (Dict.get fieldPath model.flatData)
                                            |> Maybe.withDefault ""

                                    isSelected =
                                        currentValueStr == optionValue
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
                                        , name fieldPath
                                        , value optionValue
                                        , checked isSelected
                                        , onClick (UpdateField fieldPath optionValue)
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
                            , -- Add an external link icon
                              span [ class "text-sm" ] [ text "↗" ]
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


determineUnderwritingType : Dict String JValue -> Maybe Date -> Maybe Int
determineUnderwritingType flatData currentDate =
    let
        applicantDobString =
            Dict.get "applicant_info.applicant_dob" flatData

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
            Dict.get "applicant_info.part_b_date" flatData

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


type alias BaseMedication =
    { med_name : String
    , diagnosis : String
    }


type MedicationType
    = Medication
        { drugName : String
        , diagnosis : String
        , dosage : String
        , frequency : String
        , quantity : Maybe Int
        }


type Carrier
    = ACE
    | Aetna
    | Allstate
    | UHC


carrierFromString : JValue -> Carrier
carrierFromString jvalue =
    case jvalue of
        StringValue str ->
            case str of
                "ACE" ->
                    ACE

                "Aetna" ->
                    Aetna

                "Allstate" ->
                    Allstate

                "UHC" ->
                    UHC

                _ ->
                    ACE

        _ ->
            ACE


medicationToDict : MedicationType -> Dict String JsonValue
medicationToDict medType =
    case medType of
        Medication details ->
            Dict.fromList
                [ ( "drugName", JsonString details.drugName )
                , ( "diagnosis", JsonString details.diagnosis )
                , ( "dosage", JsonString details.dosage )
                , ( "frequency", JsonString details.frequency )
                , ( "quantity"
                  , case details.quantity of
                        Just q ->
                            JsonInt q

                        Nothing ->
                            JsonNull
                  )
                ]



-- Helper to create a new medication based on carrier


initMedication : Carrier -> BaseMedication -> MedicationType
initMedication carrier base =
    case carrier of
        ACE ->
            Medication
                { drugName = base.med_name
                , diagnosis = base.diagnosis
                , dosage = ""
                , frequency = ""
                , quantity = Nothing
                }

        Aetna ->
            Medication
                { drugName = base.med_name
                , diagnosis = base.diagnosis
                , dosage = ""
                , frequency = ""
                , quantity = Nothing
                }

        Allstate ->
            Medication
                { drugName = base.med_name
                , diagnosis = base.diagnosis
                , dosage = ""
                , frequency = ""
                , quantity = Nothing
                }

        UHC ->
            Medication
                { drugName = ""
                , diagnosis = ""
                , dosage = ""
                , frequency = ""
                , quantity = Nothing
                }


renderDrugLookupField : Model -> FormSection -> FormField -> Html Msg
renderDrugLookupField model section field =
    let
        baseId =
            section.id ++ "." ++ field.id

        carrier =
            -- Get carrier from model or field config
            Maybe.withDefault ACE (Dict.get "carrier" model.flatData |> Maybe.map carrierFromString)

        medications =
            getMedicationList model baseId carrier

        showForm =
            Dict.get (baseId ++ ".showForm") model.flatData
                |> Maybe.map unwrapBoolValue
                |> Maybe.withDefault False
    in
    div [ class "space-y-6" ]
        [ -- Medication list display
          if List.isEmpty medications then
            div [ class "text-gray-500 italic" ]
                [ text "No medications added" ]

          else
            div [ class "space-y-4" ]
                (List.indexedMap (renderMedicationItem carrier baseId) medications)

        -- Add medication form
        , if showForm then
            renderMedicationForm carrier baseId model

          else
            button
                [ class "flex items-center gap-2 px-4 py-2 text-sm bg-purple-600 text-white rounded-md hover:bg-purple-700"
                , onClick (ToggleAddMedicationForm baseId)
                ]
                [ text "Add Medication" ]
        ]


getMedicationList : Model -> String -> Carrier -> List MedicationType
getMedicationList model baseId carrier =
    Dict.get baseId model.medications
        |> Maybe.withDefault []



-- Render a single medication item based on carrier type


renderMedicationItem : Carrier -> String -> Int -> MedicationType -> Html Msg
renderMedicationItem carrier baseId index medication =
    let
        baseContent =
            case medication of
                Medication details ->
                    [ viewBaseMedication details ]

        viewBaseMedication base =
            div []
                [ div [ class "font-medium" ] [ text base.drugName ]
                , div [ class "text-sm text-gray-600" ]
                    [ text ("Diagnosis: " ++ base.diagnosis) ]
                ]
    in
    div [ class "p-4 bg-gray-50 rounded-md relative" ]
        [ div [ class "space-y-2" ] baseContent
        , button
            [ class "absolute top-2 right-2 text-gray-400 hover:text-red-600"
            , onClick (RemoveMedication baseId index)
            ]
            [ text "×" ]
        ]



-- Carrier-specific detail views


viewPrescribedMedications prescribedMeds =
    div [ class "mt-2 space-y-1" ]
        (Dict.toList prescribedMeds
            |> List.map
                (\( _, med ) ->
                    div [ class "text-sm text-gray-600" ]
                        [ text (med.drugName ++ " - " ++ med.diagnosis) ]
                )
        )


viewAllstateDetails details =
    div [ class "mt-2 space-y-1 text-sm text-gray-600" ]
        [ div [] [ text ("Dosage: " ++ details.dosage) ]
        , div [] [ text ("Frequency: " ++ details.frequency) ]
        , if not (String.isEmpty details.prescription_freq_other) then
            div [] [ text ("Quantity: " ++ details.prescription_freq_other) ]

          else
            text ""
        ]


viewUHCDetails details =
    div [ class "mt-2 space-y-1 text-sm text-gray-600" ]
        [ div [] [ text ("Dosage: " ++ details.dosage.dosage) ]
        , div [] [ text ("Frequency: " ++ details.frequency) ]
        , div [] [ text ("Quantity: " ++ String.fromInt details.quantity) ]
        , if not (String.isEmpty details.lastFillDate) then
            div [] [ text ("Last Fill: " ++ details.lastFillDate) ]

          else
            text ""
        ]



-- Render the add/edit form with carrier-specific fields


renderMedicationForm : Carrier -> String -> Model -> Html Msg
renderMedicationForm carrier baseId model =
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
                    , onInput (UpdateMedicationField baseId fieldName)
                    , value
                        (Dict.get (baseId ++ ".temp." ++ fieldName) model.flatData
                            |> Maybe.map unwrapStringValue
                            |> Maybe.withDefault ""
                        )
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
                , onClick (CancelAddMedication baseId)
                ]
                [ text "Cancel" ]
            , button
                [ class "px-4 py-2 text-sm bg-purple-600 text-white rounded-md hover:bg-purple-700"
                , onClick (SaveMedication baseId (createNewMedication model baseId))
                ]
                [ text "Save" ]
            ]
        ]


extractMedications : Decode.Value -> Dict String (List MedicationType)
extractMedications jsonData =
    case Decode.decodeValue (Decode.dict Decode.value) jsonData of
        Ok dict ->
            Dict.foldl
                (\key value acc ->
                    case String.split "." key of
                        [ section, field ] ->
                            if String.endsWith "prescription_drug_list" field then
                                case decodeMedicationList value of
                                    Ok meds ->
                                        Dict.insert (section ++ "." ++ field) meds acc

                                    Err _ ->
                                        acc

                            else
                                acc

                        _ ->
                            acc
                )
                Dict.empty
                dict

        Err _ ->
            Dict.empty


decodeMedicationList : Decode.Value -> Result Decode.Error (List MedicationType)
decodeMedicationList value =
    Decode.decodeValue
        (Decode.list
            (Decode.map5
                (\drugName diagnosis dosage frequency quantity ->
                    Medication
                        { drugName = drugName
                        , diagnosis = diagnosis
                        , dosage = dosage
                        , frequency = frequency
                        , quantity = quantity
                        }
                )
                (Decode.field "drugName" Decode.string)
                (Decode.field "diagnosis" Decode.string)
                (Decode.field "dosage" Decode.string)
                (Decode.field "frequency" Decode.string)
                (Decode.maybe (Decode.field "quantity" Decode.int))
            )
        )
        value


addMedicationsToJson : Dict String (List MedicationType) -> Encode.Value -> Encode.Value
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


encodeMedicationList : List MedicationType -> Encode.Value
encodeMedicationList medications =
    Encode.list
        (\(Medication med) ->
            Encode.object
                [ ( "drugName", Encode.string med.drugName )
                , ( "diagnosis", Encode.string med.diagnosis )
                , ( "dosage", Encode.string med.dosage )
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


createNewMedication : Model -> String -> MedicationType
createNewMedication model baseId =
    let
        drugName =
            Dict.get (baseId ++ ".temp.drugName") model.flatData
                |> Maybe.map unwrapStringValue
                |> Maybe.withDefault ""

        diagnosis =
            Dict.get (baseId ++ ".temp.diagnosis") model.flatData
                |> Maybe.map unwrapStringValue
                |> Maybe.withDefault ""

        dosage =
            Dict.get (baseId ++ ".temp.dosage") model.flatData
                |> Maybe.map unwrapStringValue
                |> Maybe.withDefault ""

        frequency =
            Dict.get (baseId ++ ".temp.frequency") model.flatData
                |> Maybe.map unwrapStringValue
                |> Maybe.withDefault ""
    in
    Medication
        { drugName = drugName
        , diagnosis = diagnosis
        , dosage = dosage
        , frequency = frequency
        , quantity = Nothing
        }
