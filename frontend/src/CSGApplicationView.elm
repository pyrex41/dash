module CSGApplicationView exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode



-- MODEL


type alias Model =
    { key : String
    , data : Maybe Decode.Value
    , error : Maybe String
    , isLoading : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { key = ""
      , data = Nothing
      , error = Nothing
      , isLoading = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyChanged String
    | FetchData
    | DataReceived (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyChanged newKey ->
            ( { model | key = String.trim newKey }, Cmd.none )

        FetchData ->
            ( { model | isLoading = True, error = Nothing }
            , Http.get
                { url = "/api/csg-application/" ++ String.trim model.key
                , expect = Http.expectString DataReceived
                }
            )

        DataReceived result ->
            case result of
                Ok jsonString ->
                    case Decode.decodeString Decode.value jsonString of
                        Ok value ->
                            ( { model | data = Just value, isLoading = False }, Cmd.none )

                        Err _ ->
                            ( { model
                                | error = Just "Invalid JSON response"
                                , isLoading = False
                                , data = Nothing
                              }
                            , Cmd.none
                            )

                Err error ->
                    ( { model
                        | error =
                            Just
                                (case error of
                                    Http.BadStatus 404 ->
                                        "CSG Application not found"

                                    Http.BadStatus 403 ->
                                        "Not authorized to access CSG API"

                                    Http.BadStatus 500 ->
                                        "Server error - check if CSG API key is configured"

                                    _ ->
                                        "Failed to fetch CSG application"
                                )
                        , isLoading = False
                        , data = Nothing
                      }
                    , Cmd.none
                    )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "max-w-4xl mx-auto p-8" ]
        [ h1 [ class "text-2xl font-bold mb-8" ] [ text "View CSG Application" ]
        , viewForm model
        , viewResult model
        ]


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ onSubmit FetchData, class "mb-8" ]
        [ div [ class "flex gap-4" ]
            [ div [ class "flex-1" ]
                [ label [ class "block text-sm font-medium text-gray-700 mb-1" ]
                    [ text "CSG Application Key" ]
                , input
                    [ type_ "text"
                    , class "w-full px-3 py-2 border rounded-md"
                    , placeholder "e.g. agZjc2dhcGlyIgsSFUVucm9sbG1lbnRBcHBsaWNhdGlvbhiAgKz6p8S7CgyiAQVlX2FwcA"
                    , value model.key
                    , onInput KeyChanged
                    ]
                    []
                ]
            , div [ class "flex items-end" ]
                [ button
                    [ type_ "submit"
                    , class "px-4 py-2 bg-purple-600 text-white rounded-md hover:bg-purple-700 disabled:opacity-50"
                    , disabled (String.isEmpty model.key || model.isLoading)
                    ]
                    [ text "View Application" ]
                ]
            ]
        ]


viewResult : Model -> Html Msg
viewResult model =
    if model.isLoading then
        div [ class "flex justify-center" ]
            [ div [ class "animate-spin h-8 w-8 border-4 border-purple-600 border-t-transparent rounded-full" ] [] ]

    else
        case model.error of
            Just error ->
                div [ class "p-4 bg-red-50 text-red-700 rounded-md" ]
                    [ text error ]

            Nothing ->
                case model.data of
                    Just value ->
                        let
                            formattedJson =
                                Encode.encode 2 value
                        in
                        div [ class "font-mono text-sm overflow-x-auto bg-gray-50 p-4 rounded-md" ]
                            [ pre [ class "whitespace-pre-wrap" ]
                                [ code []
                                    [ text formattedJson ]
                                ]
                            ]

                    Nothing ->
                        text ""
