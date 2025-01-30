module ApplicationPage exposing (Model, Msg(..), init, subscriptions, update, view)

import ApplicationView
import CSGSchema
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode
import Ports exposing (requestApplication, wsSubscribe, wsUnsubscribe)
import Producer


type alias Model =
    { applicationViewModel : Maybe ApplicationView.Model
    , selectedProducer : Maybe Producer.ProducerConfig
    , error : Maybe String
    , loading : Bool
    , applicationId : String
    }


type Msg
    = ApplicationReceived (Result Decode.Error ApplicationView.Application)
    | ApplicationViewMsg ApplicationView.Msg
    | Cleanup


init : String -> Decode.Value -> ( Model, Cmd Msg )
init applicationId producerConfig =
    let
        producerConfigDict =
            Decode.decodeValue Producer.producerConfigDecoder producerConfig
                |> Result.toMaybe
                |> Maybe.withDefault Dict.empty
    in
    ( { applicationViewModel = Nothing
      , selectedProducer = Dict.get 1 producerConfigDict
      , error = Nothing
      , loading = True
      , applicationId = applicationId
      }
    , Cmd.batch
        [ requestApplication { id = applicationId }
        , wsSubscribe [ applicationId ]
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApplicationReceived result ->
            case result of
                Ok application ->
                    let
                        ( viewModel, viewCmd ) =
                            ApplicationView.init model.selectedProducer application
                    in
                    ( { model
                        | applicationViewModel = Just viewModel
                        , loading = False
                      }
                    , Cmd.map ApplicationViewMsg viewCmd
                    )

                Err error ->
                    ( { model
                        | error = Just (Decode.errorToString error)
                        , loading = False
                      }
                    , Cmd.none
                    )

        ApplicationViewMsg viewMsg ->
            case model.applicationViewModel of
                Just viewModel ->
                    let
                        ( newViewModel, viewCmd ) =
                            ApplicationView.update viewMsg viewModel
                    in
                    ( { model | applicationViewModel = Just newViewModel }
                    , Cmd.map ApplicationViewMsg viewCmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        Cleanup ->
            ( model
            , wsUnsubscribe [ model.applicationId ]
            )


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
                        case model.applicationViewModel of
                            Just viewModel ->
                                Html.map ApplicationViewMsg (ApplicationView.view viewModel)

                            Nothing ->
                                div [ class "p-8 text-gray-600" ] [ text "Application not found" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.receiveApplication
            (\value ->
                ApplicationReceived (Decode.decodeValue ApplicationView.applicationViewDecoder value)
            )
        , case model.applicationViewModel of
            Just viewModel ->
                Sub.map ApplicationViewMsg (ApplicationView.subscriptions viewModel)

            Nothing ->
                Sub.none
        ]


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
