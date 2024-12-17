module ApplicationView exposing (Model, Msg(..), ApplicationWithSchema, init, update, view, applicationDecoder)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
-- import Debug

-- TYPES

type alias Model =
    { application : Maybe ApplicationWithSchema
    , formattedData : Maybe FormattedData
    , schema : Maybe Schema
    , isLoading : Bool
    , error : Maybe String
    }

type Msg
    = Close
    | GotFormattedData (Result String FormattedData)
    | GotError String
    | CloseModal

type alias ApplicationWithSchema =
    { id : String
    , data : Decode.Value
    , schema : Schema
    }

type alias Schema =
    { sections : List Section }

type alias Section =
    { title : Maybe String
    , body : List Field
    }

type alias Field =
    { id : String
    , type_ : FieldType
    , displayLabel : String
    , required : Bool
    , options : Maybe (List Option)
    , maxlength : Maybe Int
    , order : Int
    }

type FieldType
    = TextField
    | RadioField
    | DateField
    | ComplexDatePickerField
    | CheckboxField
    | SelectField
    | UnknownField

type alias Option =
    { value : String
    , key : String
    }

type alias FormattedData =
    { sections : List FormattedSection }

type alias FormattedSection =
    { title : String
    , fields : List FormattedField
    }

type alias FormattedField =
    { label : String
    , value : String
    }

-- INIT

init : Maybe ApplicationWithSchema -> ( Model, Cmd Msg )
init maybeApplication =
    case maybeApplication of
        Just app ->
            let
                formattedData = formatApplicationData app.data app.schema
            in
            ( { application = maybeApplication
              , formattedData = Just formattedData
              , schema = Just app.schema
              , isLoading = False
              , error = Nothing
              }
            , Cmd.none
            )
        Nothing ->
            ( { application = Nothing
              , formattedData = Nothing
              , schema = Nothing
              , isLoading = False
              , error = Nothing
              }
            , Cmd.none
            )

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- let
    --     _ = Debug.log "ApplicationView.update" msg
    -- in
    case msg of
        Close ->
            ( model, Cmd.none )

        GotFormattedData result ->
            -- let
            --     _ = Debug.log "GotFormattedData" result
            -- in
            case result of
                Ok data ->
                    ( { model | formattedData = Just data, isLoading = False }, Cmd.none )
                Err error ->
                    ( { model | error = Just error, isLoading = False }, Cmd.none )

        GotError error ->
            -- let
            --     _ = Debug.log "GotError" error
            -- in
            ( { model | error = Just error, isLoading = False }, Cmd.none )

        CloseModal ->
            ( { model | application = Nothing }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    -- let
    --     _ = Debug.log "ApplicationView.view" model
    -- in
    div [ class "p-6 relative" ]
        [ viewHeader model
        , viewContent model
        ]

viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "flex justify-between items-center mb-4 border-b pb-4" ]
        [ h2 [ class "text-xl font-bold" ]
            [ text "Application Details" ]
        , button
            [ class "text-gray-500 hover:text-gray-700 text-2xl"
            , onClick CloseModal
            ]
            [ text "×" ]
        ]

viewContent : Model -> Html Msg
viewContent model =
    case ( model.isLoading, model.error, model.formattedData ) of
        ( True, _, _ ) ->
            div [ class "flex justify-center items-center py-8" ]
                [ div [ class "animate-spin h-8 w-8 border-4 border-purple-600 border-t-transparent rounded-full" ] [] ]

        ( _, Just error, _ ) ->
            div [ class "text-red-500 p-4" ]
                [ text ("Error: " ++ error) ]

        ( _, _, Just formattedData ) ->
            viewFormattedData formattedData

        _ ->
            div [ class "text-gray-500 p-4" ]
                [ text "Loading application data..." ]

viewFormattedData : FormattedData -> Html Msg
viewFormattedData data =
    div [ class "space-y-6" ]
        (List.map viewSection data.sections)

viewSection : FormattedSection -> Html Msg
viewSection section =
    div [ class "border rounded-lg p-4" ]
        [ h3 [ class "text-lg font-semibold mb-3" ]
            [ text section.title ]
        , div [ class "space-y-3" ]
            (List.map viewField section.fields)
        ]

viewField : FormattedField -> Html Msg
viewField field =
    div [ class "grid grid-cols-3 gap-4" ]
        [ label [ class "text-gray-600" ]
            [ text field.label ]
        , div [ class "col-span-2" ]
            [ text field.value ]
        ]

-- HTTP

fetchFormattedData : String -> Cmd Msg
fetchFormattedData applicationId =
    Http.get
        { url = "/api/proxy-formatter/api/applications/" ++ applicationId ++ "/formatted"
        , expect = Http.expectJson (Result.mapError httpErrorToString >> GotFormattedData) formattedDataDecoder
        }

-- DECODERS

formattedDataDecoder : Decode.Decoder FormattedData
formattedDataDecoder =
    Decode.map FormattedData
        (Decode.field "sections" (Decode.list formattedSectionDecoder))

formattedSectionDecoder : Decode.Decoder FormattedSection
formattedSectionDecoder =
    Decode.map2 FormattedSection
        (Decode.field "title" Decode.string)
        (Decode.field "fields" (Decode.list formattedFieldDecoder))

formattedFieldDecoder : Decode.Decoder FormattedField
formattedFieldDecoder =
    Decode.map2 FormattedField
        (Decode.field "label" Decode.string)
        (Decode.field "value" Decode.string)

-- HELPERS

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


applicationDecoder : Decode.Decoder ApplicationWithSchema
applicationDecoder =
    -- let
    --     _ = Debug.log "Decoding application with schema" ()
    -- in
    Decode.succeed ApplicationWithSchema
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "data" Decode.value
        |> Pipeline.required "schema" schemaDecoder
        -- |> Debug.log "Application decoder result"

schemaDecoder : Decode.Decoder Schema
schemaDecoder =
    -- let
    --     _ = Debug.log "Decoding schema" ()
    -- in
    Decode.map Schema
        (Decode.field "sections" (Decode.list sectionDecoder))
        -- |> Debug.log "Schema decoder result"

sectionDecoder : Decode.Decoder Section
sectionDecoder =
    -- let
    --     _ = Debug.log "Decoding section" ()
    -- in
    Decode.map2 Section
        (Decode.field "title" (Decode.nullable Decode.string))
        (Decode.field "body" (Decode.list fieldDecoder))
        -- |> Debug.log "Section decoder result"

fieldDecoder : Decode.Decoder Field
fieldDecoder =
    let
        displayLabelDecoder = 
            Decode.oneOf
                [ Decode.field "displayLabel" Decode.string
                , Decode.field "display_label" Decode.string
                , Decode.succeed "Untitled Field"
                ]

        -- Make options decoder more lenient
        optionsDecoder =
            Decode.oneOf
                [ Decode.nullable (Decode.list optionDecoder)
                , Decode.succeed Nothing
                ]

        -- New: Super lenient required decoder that ignores complex objects
        requiredDecoder =
            Decode.oneOf
                [ Decode.bool
                , Decode.succeed False  -- Default to false for any non-boolean value
                ]
    in
    Decode.succeed Field
        |> Pipeline.required "id" Decode.string
        |> Pipeline.optional "type" fieldTypeDecoder UnknownField
        |> Pipeline.custom displayLabelDecoder
        |> Pipeline.optional "required" requiredDecoder False
        |> Pipeline.optional "options" optionsDecoder Nothing
        |> Pipeline.optional "maxlength" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "order" Decode.int 0

fieldTypeDecoder : Decode.Decoder FieldType
fieldTypeDecoder =
    let
        fromString str =
            case String.toLower str of
                "text" -> Decode.succeed TextField
                "text_field" -> Decode.succeed TextField
                "radio" -> Decode.succeed RadioField
                "radio_field" -> Decode.succeed RadioField
                "date" -> Decode.succeed DateField
                "date_field" -> Decode.succeed DateField
                "complexdatepicker" -> Decode.succeed ComplexDatePickerField
                "complex_date_picker" -> Decode.succeed ComplexDatePickerField
                "checkbox" -> Decode.succeed CheckboxField
                "checkbox_field" -> Decode.succeed CheckboxField
                "select" -> Decode.succeed SelectField
                "select_field" -> Decode.succeed SelectField
                _ -> Decode.succeed UnknownField
    in
    Decode.oneOf
        [ Decode.field "type" Decode.string |> Decode.andThen fromString
        , Decode.string |> Decode.andThen fromString
        ]

optionDecoder : Decode.Decoder Option
optionDecoder =
    Decode.oneOf
        [ -- Object format with key/value
          Decode.map2 Option
            (Decode.field "value" Decode.string)
            (Decode.field "key" Decode.string)
        , -- Number format (convert number to string for both key and value)
          Decode.int
            |> Decode.map String.fromInt
            |> Decode.map (\str -> Option str str)
        , -- String format (use same string for both key and value)
          Decode.string
            |> Decode.map (\str -> Option str str)
        ]

-- Helper to convert section title to data object key
sectionToDataKey : String -> String
sectionToDataKey section =
    case section of
        "Applicant Information" -> "applicant_info"
        "Medicare Information" -> "medicare_information"
        "Method of Payment" -> "payment"
        "Previous or Existing Coverage Information" -> "existing_coverage"
        _ -> String.toLower (String.replace " " "_" section)

-- Update extractFieldValue to handle nested data structure
extractFieldValue : Decode.Value -> String -> Field -> Maybe String
extractFieldValue data sectionTitle field =
    -- let
    --     _ = Debug.log "extractFieldValue input" 
    --         { data = data
    --         , sectionTitle = sectionTitle
    --         , fieldId = field.id
    --         , fieldType = field.type_
    --         , dataKey = sectionToDataKey sectionTitle
    --         }
            
    let
        decoder = 
            -- First get the correct section object (e.g., applicant_info)
            Decode.field (sectionToDataKey sectionTitle)
                -- Then get the field value
                (Decode.field field.id
                    (case field.type_ of
                        TextField -> Decode.string
                        DateField -> Decode.string
                        SelectField -> Decode.string
                        RadioField -> Decode.string
                        CheckboxField -> 
                            Decode.bool 
                                |> Decode.map (\b -> if b then "Yes" else "No")
                        ComplexDatePickerField -> Decode.string
                        UnknownField -> 
                            -- More lenient decoder for unknown types
                            Decode.oneOf
                                [ Decode.string
                                , Decode.bool |> Decode.map (\b -> if b then "Yes" else "No")
                                , Decode.int |> Decode.map String.fromInt
                                , Decode.null ""
                                ]
                    )
                )
        
        result = Decode.decodeValue decoder data
        -- _ = Debug.log "extractFieldValue result" result
    in
    Result.toMaybe result

-- Update formatSection to pass the section title instead of a generated ID
formatSection : Decode.Value -> Section -> FormattedSection
formatSection data section =
    let
        title = Maybe.withDefault "Untitled Section" section.title
                
        -- _ = Debug.log "formatSection" 
        --     { sectionTitle = section.title
        --     , fieldCount = List.length section.body
        --     , dataKey = sectionToDataKey title
        --     }
    in
    FormattedSection
        title
        (List.map (formatField data title) section.body)

formatField : Decode.Value -> String -> Field -> FormattedField
formatField data sectionTitle field =
    FormattedField
        field.displayLabel
        (Maybe.withDefault "" (extractFieldValue data sectionTitle field))

-- Add new function to format data according to schema
formatApplicationData : Decode.Value -> Schema -> FormattedData
formatApplicationData data schema =
    FormattedData
        (List.map (formatSection data) schema.sections)