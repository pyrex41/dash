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
import Time


type alias Model =
    { flatData : Dict String JValue
    , schema : ApplicationSchema
    , id : String
    , error : Maybe String
    , expandedSections : Dict String Bool
    , currentTime : Maybe Time.Posix
    }


type Msg
    = GotError String
    | ToggleSection String
    | UpdateField String String
    | SaveForm
    | NoOp


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
            extractFormValues app.data
    in
    ( { flatData = initialFormValues
      , schema = app.schema
      , id = app.id
      , error = Nothing
      , expandedSections = Dict.singleton "applicant_info" True
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
            Dict.insert prefix (StringValue str) dict

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
                oldValue =
                    Dict.get fieldId model.flatData

                value =
                    parseValue valueString

                newFlatData =
                    Dict.insert fieldId value model.flatData

                _ =
                    Debug.log "UpdateField"
                        { fieldId = fieldId
                        , oldValue = oldValue
                        , newValue = valueString
                        , parsedValue = value
                        , beforeUpdate = Dict.get fieldId model.flatData
                        , afterUpdate = Dict.get fieldId newFlatData
                        }
            in
            ( { model | flatData = newFlatData }
            , Cmd.none
            )

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
                , div [ class "flex justify-center pb-8" ]
                    [ viewSaveButton ]
                ]


viewSaveButton : Html Msg
viewSaveButton =
    button
        [ class """
            bg-purple-600 text-white px-6 py-2.5 rounded-lg font-medium
            hover:bg-purple-700 transition-colors shadow-sm
            focus:outline-none focus:ring-2 focus:ring-purple-500 focus:ring-offset-2
            min-w-[120px] text-sm
          """
        , onClick SaveForm
        ]
        [ text "Save" ]


viewForm : Model -> Html Msg
viewForm model =
    div [ class "max-w-3xl mx-auto space-y-8 px-6" ]
        (List.sortBy .order model.schema
            |> List.map (renderFormSection model)
        )


renderFormSection : Model -> FormSection -> Html Msg
renderFormSection model section =
    let
        isExpanded =
            Dict.get section.id model.expandedSections
                |> Maybe.withDefault False
    in
    div
        [ class """
            border border-gray-200 rounded-lg shadow-sm mb-8 bg-white
            transition-all duration-200 hover:shadow-md
          """
        ]
        [ div
            [ class """
                flex items-center justify-between p-6 cursor-pointer
                bg-gray-50 border-b border-gray-200
                transition-colors duration-200
              """
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


renderFormField : Model -> FormSection -> FormField -> Html Msg
renderFormField model section field =
    let
        baseInputClass =
            """
            w-full px-4 py-3 border border-gray-300 rounded-lg
            focus:outline-none focus:ring-2 focus:ring-purple-500 focus:border-purple-500
            text-gray-700 bg-white transition-all duration-200
            hover:border-gray-400 text-base
            """

        labelClass =
            "block text-sm font-medium text-gray-700 mb-2"

        displayLabel =
            case field.fieldType of
                TextBlockField _ ->
                    ""

                _ ->
                    field.displayLabel

        wrapperClass =
            case field.fieldType of
                HeadingField _ ->
                    "mb-8 mt-16"

                _ ->
                    "mb-6"
    in
    if isFieldVisible field section model.flatData then
        div [ class wrapperClass ]
            [ label [ class labelClass ]
                [ text displayLabel
                , case field.required of
                    RequiredBool bool ->
                        if bool then
                            span [ class "text-red-500 ml-1" ] [ text "*" ]

                        else
                            text ""

                    RequiredDependsOn _ ->
                        span [ class "text-blue-500 ml-1" ] [ text "*" ]
                ]
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
                        jValue =
                            Dict.get (section.id ++ "." ++ field.id) model.flatData
                                |> Maybe.withDefault NullValue

                        rawValue =
                            case jValue of
                                StringValue str ->
                                    str

                                _ ->
                                    ""

                        formattedValue =
                            CSGSchema.formatPhoneNumber rawValue
                    in
                    input
                        [ type_ "tel"
                        , class baseInputClass
                        , value formattedValue
                        , onInput
                            (\input ->
                                UpdateField (section.id ++ "." ++ field.id) (String.filter Char.isDigit input)
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
                                CSGSchema.calculateAge
                                    (Dict.get "applicant_info.applicant_dob" model.flatData)
                                    model.currentTime

                            ageString =
                                dobValue
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
