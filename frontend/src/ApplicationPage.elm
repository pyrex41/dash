module ApplicationPage exposing (Model, Msg, init, update, view)

import ApplicationView
import CSGSchema
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode


type alias Model =
    { applicationView : Maybe ApplicationView.Model
    , error : Maybe String
    , loading : Bool
    }


type Msg
    = ApplicationReceived (Result Http.Error ApplicationView.Application)
    | ApplicationViewMsg ApplicationView.Msg


init : String -> ( Model, Cmd Msg )
init applicationId =
    ( { applicationView = Nothing
      , error = Nothing
      , loading = True
      }
    , Http.get
        { url = "/api/applications/" ++ applicationId
        , expect = Http.expectJson ApplicationReceived applicationViewDecoder
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApplicationReceived result ->
            case result of
                Ok application ->
                    let
                        ( viewModel, viewCmd ) =
                            ApplicationView.init application
                    in
                    ( { model
                        | applicationView = Just viewModel
                        , loading = False
                      }
                    , Cmd.map ApplicationViewMsg viewCmd
                    )

                Err error ->
                    ( { model
                        | error = Just (httpErrorToString error)
                        , loading = False
                      }
                    , Cmd.none
                    )

        ApplicationViewMsg viewMsg ->
            case model.applicationView of
                Just viewModel ->
                    let
                        ( newViewModel, viewCmd ) =
                            ApplicationView.update viewMsg viewModel
                    in
                    ( { model | applicationView = Just newViewModel }
                    , Cmd.map ApplicationViewMsg viewCmd
                    )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-gray-50 py-8" ]
        [ div [ class "max-w-4xl mx-auto bg-white rounded-lg shadow" ]
            [ if model.loading then
                div [ class "p-8 flex justify-center" ]
                    [ div [ class "animate-spin h-8 w-8 border-4 border-purple-600 border-t-transparent rounded-full" ] [] ]

              else
                case model.error of
                    Just error ->
                        div [ class "p-8 text-red-600" ] [ text error ]

                    Nothing ->
                        case model.applicationView of
                            Just viewModel ->
                                Html.map ApplicationViewMsg (ApplicationView.view viewModel)

                            Nothing ->
                                div [ class "p-8 text-gray-600" ] [ text "Application not found" ]
            ]
        ]



-- Helper functions


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
            "Server returned status: " ++ String.fromInt status

        Http.BadBody message ->
            "Failed to decode response: " ++ message


applicationViewDecoder : Decode.Decoder ApplicationView.Application
applicationViewDecoder =
    Decode.map3 ApplicationView.Application
        (Decode.field "id" Decode.string)
        (Decode.field "data" Decode.value)
        (Decode.field "schema" (Decode.field "sections" CSGSchema.formSchemaDecoder))
