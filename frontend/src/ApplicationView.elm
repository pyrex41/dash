module ApplicationView exposing (Application, Model, Msg(..), init, update, view)

import CSGSchema exposing (ApplicationSchema, FormField, FormFieldType(..), FormSection, JValue(..), RequiredType(..), encodeFormValues, isFieldVisible, parseValue)
import Debug
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task
import Time


type alias Model =
    { flatData : Dict String JValue
    , schema : ApplicationSchema
    , id : String
    , error : Maybe String
    , expandedSections : Dict String Bool
    , currentTime : Maybe Time.Posix
    , showDebugFields : Bool
    , isValid : Bool
    }


type Msg
    = GotError String
    | ToggleSection String
    | UpdateField String String
    | SaveForm
    | GotCurrentTime Time.Posix
    | SubmitToCSG
    | NoOp
    | GotRoutingNumber (Result Http.Error String)


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

        model =
            { flatData = initialFormValues
            , schema = app.schema
            , id = app.id
            , error = Nothing
            , expandedSections = Dict.singleton "applicant_info" True
            , currentTime = Nothing
            , showDebugFields = True
            , isValid = False
            }
    in
    ( { model | isValid = validateData model }
    , Task.perform GotCurrentTime Time.now
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
                            let
                                _ =
                                    Debug.log "Valid routing number" valueString
                            in
                            Http.get
                                { url = "https://www.routingnumbers.info/api/name.json?rn=" ++ valueString
                                , expect = Http.expectJson GotRoutingNumber routingNumberDecoder
                                }

                        else
                            let
                                _ =
                                    Debug.log "Invalid routing number" valueString
                            in
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

        GotCurrentTime currentTime ->
            ( { model | currentTime = Just currentTime }, Cmd.none )

        SaveForm ->
            if Dict.isEmpty model.flatData then
                ( model, Cmd.none )

            else
                let
                    payload =
                        Encode.object
                            [ ( "id", Encode.string model.id )
                            , ( "data", encodeFormValues model.flatData )
                            ]
                in
                ( model
                , Http.post
                    { url = "/api/applications/" ++ model.id
                    , body = Http.jsonBody payload
                    , expect = Http.expectWhatever (Result.mapError httpErrorToString >> (\_ -> NoOp))
                    }
                )

        GotRoutingNumber result ->
            case result of
                Ok institutionName ->
                    let
                        _ =
                            Debug.log "Got institution name" institutionName

                        newFlatData =
                            Dict.insert "payment.eft_financial_institution_name" (StringValue institutionName) model.flatData
                    in
                    ( { model | flatData = newFlatData }
                    , Cmd.none
                    )

                Err error ->
                    let
                        _ =
                            Debug.log "Error getting routing number" error
                    in
                    ( model, Cmd.none )

        SubmitToCSG ->
            if model.isValid then
                -- Add your CSG submission logic here
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
            List.any (\field -> isFieldVisible field section model.flatData) section.body

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
                                CSGSchema.calculateAge dobValue model.currentTime
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
                    div [ class "space-y-2" ]
                        [ input
                            [ type_ "text"
                            , class baseInputClass
                            , value (Dict.get (section.id ++ "." ++ field.id) model.flatData |> stringifyMaybe)
                            , onInput (UpdateField (section.id ++ "." ++ field.id))
                            ]
                            []
                        , if config.isLastFillVisible then
                            div [ class "text-sm text-cyan-300" ]
                                [ text "Last fill date will be shown here" ]

                          else
                            text ""
                        ]

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



-- CARRIER FORMATTERS
-- on hold -- using python formatter for now


type alias PhoneNumber =
    { areaCode : String
    , centralOfficeCode : String
    , stationCode : String
    }


formatPhoneNumber : String -> PhoneNumber
formatPhoneNumber phone =
    let
        areaCode =
            String.slice 0 3 phone

        centralOfficeCode =
            String.slice 3 5 phone

        stationCode =
            String.slice 5 8 phone
    in
    { areaCode = areaCode
    , centralOfficeCode = centralOfficeCode
    , stationCode = stationCode
    }


type ComprehensivePlan
    = PlanA
    | PlanB
    | PlanC
    | PlanD
    | PlanF
    | PlanG
    | HighDeductiblePlanF
    | Extended


comprehensivePlanFromString : String -> Maybe ComprehensivePlan
comprehensivePlanFromString plan =
    case plan of
        "A" ->
            Just PlanA

        "B" ->
            Just PlanB

        "C" ->
            Just PlanC

        "D" ->
            Just PlanD

        "F" ->
            Just PlanF

        "G" ->
            Just PlanG

        "High Deductible Plan F" ->
            Just HighDeductiblePlanF

        "Extended" ->
            Just Extended

        _ ->
            Nothing


type BasicPlan
    = PlanK
    | PlanL
    | PlanM
    | PlanN
    | Basic
    | PartialDeductible


basicPlanFromString : String -> Maybe BasicPlan
basicPlanFromString plan =
    case plan of
        "K" ->
            Just PlanK

        "L" ->
            Just PlanL

        "M" ->
            Just PlanM

        "Basic" ->
            Just Basic

        "N" ->
            Just PlanN

        "50% Part A Deductible" ->
            Just PartialDeductible

        _ ->
            Nothing


type LegacyPlan
    = PlanE
    | PlanH
    | PlanI
    | PlanJ
    | PreStandardized


legacyPlanFromString : String -> Maybe LegacyPlan
legacyPlanFromString plan =
    case plan of
        "E" ->
            Just PlanE

        "H" ->
            Just PlanH

        "I" ->
            Just PlanI

        "J" ->
            Just PlanJ

        "Pre-Standardized" ->
            Just PreStandardized

        _ ->
            Nothing


type SupPlan
    = CP ComprehensivePlan
    | BP BasicPlan
    | LP LegacyPlan


anyPlanFromString : String -> Maybe SupPlan
anyPlanFromString plan =
    case comprehensivePlanFromString plan of
        Just cp ->
            Just (CP cp)

        Nothing ->
            case basicPlanFromString plan of
                Just bp ->
                    Just (BP bp)

                Nothing ->
                    case legacyPlanFromString plan of
                        Just lp ->
                            Just (LP lp)

                        Nothing ->
                            Nothing


type BenefitType
    = AdditionalBenefits
    | FewerBenefitsLowerPremiums
    | LowerPremiums
    | Other


getPlanSwitchReason : SupPlan -> Maybe SupPlan -> BenefitType
getPlanSwitchReason targetPlan currentPlanMaybe =
    case currentPlanMaybe of
        Nothing ->
            Other

        Just currentPlan ->
            case targetPlan of
                BP PlanN ->
                    case currentPlan of
                        BP PlanN ->
                            LowerPremiums

                        BP _ ->
                            AdditionalBenefits

                        CP _ ->
                            FewerBenefitsLowerPremiums

                        _ ->
                            Other

                CP PlanG ->
                    case currentPlan of
                        CP PlanG ->
                            LowerPremiums

                        BP _ ->
                            AdditionalBenefits

                        CP PlanC ->
                            FewerBenefitsLowerPremiums

                        CP PlanF ->
                            FewerBenefitsLowerPremiums

                        CP HighDeductiblePlanF ->
                            FewerBenefitsLowerPremiums

                        CP Extended ->
                            FewerBenefitsLowerPremiums

                        _ ->
                            Other

                _ ->
                    Other


validateData : Model -> Bool
validateData model =
    let
        invalidSections =
            List.filter (\section -> validateSection model section) model.schema

        _ =
            Debug.log "Invalid sections" (List.map .id invalidSections)
    in
    List.all (\section -> not (validateSection model section)) model.schema



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
    if String.isEmpty routingNumber then
        True

    else if not isNineDigits then
        False

    else
        checksum == 0


routingNumberDecoder : Decoder String
routingNumberDecoder =
    Decode.field "name" Decode.string
