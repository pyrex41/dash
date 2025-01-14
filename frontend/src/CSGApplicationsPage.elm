module CSGApplicationsPage exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Time



-- MODEL


type alias Model =
    { applications : List CSGApplication
    , error : Maybe String
    , loading : Bool
    , shouldAutoRefresh : Bool
    }


type alias CSGApplication =
    { key : String
    , applicantSigned : Bool
    , applicationDate : Maybe String
    , approved : Bool
    , createdDate : String
    , naic : String
    , plan : String
    , policyNumber : Maybe String
    , submitted : Bool
    , toolType : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { applications = []
      , error = Nothing
      , loading = True
      , shouldAutoRefresh = True
      }
    , fetchApplications
    )



-- UPDATE


type Msg
    = GotApplications (Result Http.Error (List CSGApplication))
    | RefreshApplications Time.Posix
    | RetryFetch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotApplications result ->
            case result of
                Ok applications ->
                    ( { model
                        | applications = applications
                        , error = Nothing
                        , loading = False
                        , shouldAutoRefresh = True
                      }
                    , Cmd.none
                    )

                Err error ->
                    let
                        errorMsg =
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
                    in
                    ( { model
                        | error = Just ("Failed to load CSG applications: " ++ errorMsg)
                        , loading = False
                        , shouldAutoRefresh = False
                      }
                    , Cmd.none
                    )

        RefreshApplications _ ->
            if model.shouldAutoRefresh && not model.loading then
                ( { model | loading = True }
                , fetchApplications
                )

            else
                ( model, Cmd.none )

        RetryFetch ->
            ( { model
                | loading = True
                , error = Nothing
                , shouldAutoRefresh = True
              }
            , fetchApplications
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.shouldAutoRefresh then
        Time.every (30 * 1000) RefreshApplications

    else
        Sub.none



-- HTTP


fetchApplications : Cmd Msg
fetchApplications =
    Http.get
        { url = "/api/csg-applications?limit=10"
        , expect = Http.expectJson GotApplications applicationsDecoder
        }



-- DECODERS


applicationsDecoder : Decode.Decoder (List CSGApplication)
applicationsDecoder =
    Decode.list applicationDecoder


applicationDecoder : Decode.Decoder CSGApplication
applicationDecoder =
    Decode.succeed CSGApplication
        |> Pipeline.required "key" Decode.string
        |> Pipeline.required "applicant_signed" Decode.bool
        |> Pipeline.optional "application_date" (Decode.nullable Decode.string) Nothing
        |> Pipeline.required "approved" Decode.bool
        |> Pipeline.required "created_date" Decode.string
        |> Pipeline.required "naic" Decode.string
        |> Pipeline.required "plan"
            (Decode.oneOf
                [ Decode.at [ "tag_values", "plan" ] Decode.string
                , Decode.succeed "N/A"
                ]
            )
        |> Pipeline.optional "policy_number" (Decode.nullable Decode.string) Nothing
        |> Pipeline.required "submitted" Decode.bool
        |> Pipeline.required "tool_type" Decode.string



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container mx-auto px-4 py-8" ]
        [ h1 [ class "text-3xl font-bold mb-6" ]
            [ text "CSG Applications" ]
        , viewContent model
        ]


viewContent : Model -> Html Msg
viewContent model =
    if model.loading then
        div [ class "flex justify-center" ]
            [ div [ class "animate-spin rounded-full h-32 w-32 border-b-2 border-gray-900" ] [] ]

    else
        case model.error of
            Just error ->
                div [ class "text-center" ]
                    [ div [ class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded mb-4" ]
                        [ text error ]
                    , button
                        [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
                        , onClick RetryFetch
                        ]
                        [ text "Retry" ]
                    ]

            Nothing ->
                viewApplicationsTable model.applications


viewApplicationsTable : List CSGApplication -> Html Msg
viewApplicationsTable applications =
    if List.isEmpty applications then
        div [ class "text-gray-600 text-center py-8" ]
            [ text "No applications found" ]

    else
        div [ class "overflow-x-auto" ]
            [ table [ class "min-w-full bg-white" ]
                [ thead
                    [ class "bg-gray-100" ]
                    [ tr []
                        [ th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Created Date" ]
                        , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "NAIC" ]
                        , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Plan" ]
                        , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Status" ]
                        , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Policy Number" ]
                        ]
                    ]
                , tbody [ class "divide-y divide-gray-200" ]
                    (List.map viewApplicationRow applications)
                ]
            ]


viewApplicationRow : CSGApplication -> Html Msg
viewApplicationRow app =
    tr []
        [ td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-900" ]
            [ text (String.left 10 app.createdDate) ]
        , td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-900" ]
            [ text app.naic ]
        , td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-900" ]
            [ text app.plan ]
        , td [ class "px-6 py-4 whitespace-nowrap" ]
            [ viewStatus app ]
        , td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-900" ]
            [ text (Maybe.withDefault "N/A" app.policyNumber) ]
        ]


viewStatus : CSGApplication -> Html Msg
viewStatus app =
    let
        ( color, text_ ) =
            if app.approved then
                ( "bg-green-100 text-green-800", "Approved" )

            else if app.submitted then
                ( "bg-blue-100 text-blue-800", "Submitted" )

            else if app.applicantSigned then
                ( "bg-yellow-100 text-yellow-800", "Signed" )

            else
                ( "bg-gray-100 text-gray-800", "In Progress" )
    in
    span
        [ class ("px-2 inline-flex text-xs leading-5 font-semibold rounded-full " ++ color) ]
        [ text text_ ]
