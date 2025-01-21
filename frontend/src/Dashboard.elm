module Dashboard exposing (Model, Msg(..), init, subscriptions, update, view)

import ApplicationView exposing (Status(..), applicationViewDecoder)
import Basics
import Browser
import Browser.Events
import CSGSchema exposing (Carrier(..), carrierFromNaic, carrierToString)
import Date
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onInput, stopPropagationOn, targetValue)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Ports exposing (..)
import Producer
import Set exposing (Set)
import Task



-- PORTS


type alias PaginationInfo =
    { total : Int
    , page : Int
    , pageSize : Int
    , totalPages : Int
    }


type alias ApplicationRow =
    { id : String
    , naic : String
    , name : Maybe String
    , status : Status
    , phone : Maybe String
    , email : Maybe String
    , effectiveDate : Maybe String
    , dateStarted : String
    }


type alias ApplicationsResponse =
    { applications : List ApplicationRow
    , pagination : PaginationInfo
    }



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { applications : List ApplicationRow
    , applicationCache : Dict String ApplicationView.Application
    , searchTerm : String
    , hasContactFilter : Bool
    , isLoading : Bool
    , error : Maybe String
    , searchDebouncer : Debounce String
    , searchLoading : Bool
    , currentPage : Int
    , pageSize : Int
    , total : Int
    , totalPages : Int
    , applicationView : Maybe ApplicationView.Model
    , showApplicationModal : Bool
    , producerConfig : Decode.Value
    , selectedApplicationId : Maybe String
    , naicsFilter : List String
    }



-- INIT


init : Decode.Value -> ( Model, Cmd Msg )
init producerConfig =
    ( { applications = []
      , applicationCache = Dict.empty
      , searchTerm = ""
      , hasContactFilter = False
      , isLoading = True
      , error = Nothing
      , searchDebouncer = Debounce.init
      , searchLoading = False
      , currentPage = 0
      , pageSize = 20
      , total = 0
      , totalPages = 0
      , applicationView = Nothing
      , showApplicationModal = False
      , producerConfig = producerConfig
      , selectedApplicationId = Nothing
      , naicsFilter = []
      }
    , requestRefresh
        { page = 0
        , pageSize = 20
        , searchTerm = ""
        , hasContactFilter = False
        , naics = []
        }
    )



-- UPDATE


type Msg
    = NoOp
    | ViewApplication String
    | ApplicationReceived Decode.Value (Result Decode.Error ApplicationView.Application)
    | SearchTermChanged String
    | ToggleContactFilter Bool
    | RefreshApplications
    | ApplicationsReceived (Result Decode.Error ApplicationsResponse)
    | SearchDebouncerMsg Debounce.Msg
    | ChangePage Int
    | ApplicationViewMsg ApplicationView.Msg
    | CloseApplicationModal
    | HandleKeyPress String
    | ApplicationUpdated Decode.Value
    | StatusUpdate { id : String, status : String }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ViewApplication id ->
            let
                -- Check if application is in cache
                cmd =
                    case Dict.get id model.applicationCache of
                        Just application ->
                            -- If in cache, simulate the received message
                            ApplicationReceived model.producerConfig (Ok application)
                                |> Task.succeed
                                |> Task.perform identity

                        Nothing ->
                            -- If not in cache, request it
                            requestApplication { id = id }
            in
            ( { model
                | showApplicationModal = True
                , applicationView = Nothing
                , selectedApplicationId = Just id
              }
            , cmd
            )

        ApplicationReceived producerConfig result ->
            case result of
                Ok application ->
                    let
                        ( viewModel, viewCmd ) =
                            ApplicationView.init producerConfig application

                        -- Store in cache
                        newCache =
                            Dict.insert application.id application model.applicationCache
                    in
                    ( { model
                        | applicationView = Just viewModel
                        , isLoading = False
                        , applicationCache = newCache
                      }
                    , Cmd.map ApplicationViewMsg viewCmd
                    )

                Err error ->
                    ( { model
                        | error = Just (Decode.errorToString error)
                        , isLoading = False
                      }
                    , Cmd.none
                    )

        SearchTermChanged term ->
            let
                trimmedTerm =
                    String.trim term

                shouldSearch =
                    String.length trimmedTerm >= 3

                shouldRefresh =
                    String.length trimmedTerm == 0 && String.length (String.trim model.searchTerm) > 0

                ( debouncer, cmd ) =
                    if shouldSearch then
                        Debounce.push searchDebounceConfig trimmedTerm model.searchDebouncer

                    else if shouldRefresh then
                        ( model.searchDebouncer
                        , requestRefresh
                            { page = 0
                            , pageSize = 20
                            , searchTerm = ""
                            , hasContactFilter = False
                            , naics = model.naicsFilter
                            }
                        )

                    else
                        ( model.searchDebouncer, Cmd.none )
            in
            ( { model
                | searchTerm = term
                , searchDebouncer = debouncer
                , searchLoading = shouldSearch
              }
            , cmd
            )

        ToggleContactFilter value ->
            ( { model | hasContactFilter = value }
            , requestRefresh
                { page = 0
                , pageSize = 20
                , searchTerm = model.searchTerm
                , hasContactFilter = value
                , naics = model.naicsFilter
                }
            )

        RefreshApplications ->
            ( { model | isLoading = True }
            , requestRefresh
                { page = model.currentPage
                , pageSize = model.pageSize
                , searchTerm = model.searchTerm
                , hasContactFilter = model.hasContactFilter
                , naics = model.naicsFilter
                }
            )

        ApplicationsReceived result ->
            case result of
                Ok response ->
                    let
                        _ =
                            Debug.log "Applications received" response

                        -- If we're filtering, fetch full applications
                        shouldFetchFull =
                            not (String.isEmpty model.searchTerm) || model.hasContactFilter || not (List.isEmpty model.naicsFilter)

                        fetchFullCmd =
                            if shouldFetchFull then
                                response.applications
                                    |> List.map (\app -> requestApplication { id = app.id })
                                    |> Cmd.batch

                            else
                                Cmd.none
                    in
                    ( { model
                        | applications = response.applications
                        , total = response.pagination.total
                        , currentPage = response.pagination.page
                        , pageSize = response.pagination.pageSize
                        , totalPages = response.pagination.totalPages
                        , isLoading = False
                        , error = Nothing
                      }
                    , fetchFullCmd
                    )

                Err error ->
                    let
                        _ =
                            Debug.log "Applications decode error" error
                    in
                    ( { model
                        | error = Just (Decode.errorToString error)
                        , isLoading = False
                      }
                    , Cmd.none
                    )

        SearchDebouncerMsg debounceMsg ->
            let
                ( debouncer, cmd ) =
                    Debounce.update
                        searchDebounceConfig
                        (Debounce.takeLast performSearch)
                        debounceMsg
                        model.searchDebouncer
            in
            ( { model | searchDebouncer = debouncer }
            , cmd
            )

        ChangePage page ->
            ( { model | currentPage = page }
            , requestRefresh
                { page = page
                , pageSize = model.pageSize
                , searchTerm = model.searchTerm
                , hasContactFilter = model.hasContactFilter
                , naics = model.naicsFilter
                }
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

        CloseApplicationModal ->
            ( { model
                | showApplicationModal = False
                , applicationView = Nothing
                , selectedApplicationId = Nothing
              }
            , Cmd.none
            )

        HandleKeyPress key ->
            if key == "Escape" then
                update CloseApplicationModal model

            else
                ( model, Cmd.none )

        ApplicationUpdated value ->
            let
                _ =
                    Debug.log "Application updated" value

                maybeNewStatus =
                    Decode.decodeValue (Decode.field "status" statusDecoder) value

                maybeId =
                    Decode.decodeValue (Decode.field "id" Decode.string) value

                newApplications =
                    case ( maybeId, maybeNewStatus ) of
                        ( Ok id, Ok newStatus ) ->
                            model.applications
                                |> List.map
                                    (\app ->
                                        if app.id == id then
                                            { app | status = newStatus }

                                        else
                                            app
                                    )

                        _ ->
                            model.applications
            in
            ( { model | applications = newApplications }, Cmd.none )

        StatusUpdate { id, status } ->
            let
                maybeNewStatus =
                    case status of
                        "submitting" ->
                            Just Submitting

                        "awaiting_signature" ->
                            Just AwaitingSignature

                        "verified" ->
                            Just AwaitingSignature

                        "failed" ->
                            Just SubmissionIssue

                        "verifying" ->
                            Just Verifying

                        "submission_issue" ->
                            Just SubmissionIssue

                        _ ->
                            Nothing

                newApplications =
                    model.applications
                        |> List.map
                            (\app ->
                                if app.id == id then
                                    { app | status = maybeNewStatus |> Maybe.withDefault app.status }

                                else
                                    app
                            )
            in
            ( { model | applications = newApplications }, Cmd.none )



-- Add completion logic here
-- VIEW


view : Model -> Html Msg
view model =
    let
        applicationId =
            model.selectedApplicationId |> Maybe.withDefault ""
    in
    div [ class "min-h-screen bg-white relative" ]
        [ viewHeader
        , div [ class "max-w-7xl mx-auto" ]
            [ viewApplications model
            ]
        , if model.showApplicationModal then
            div
                [ class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50"
                , onClick CloseApplicationModal
                ]
                [ div
                    [ class "bg-white rounded-lg shadow-xl max-w-4xl w-full max-h-[90vh] overflow-y-auto"
                    , stopPropagation "click"
                    ]
                    [ div [ class "flex justify-between items-center p-4 border-b" ]
                        [ div [] []
                        , a
                            [ class "text-purple-600 hover:text-purple-700 text-sm flex items-center gap-1"
                            , href ("/application/" ++ applicationId)
                            , target "_blank"
                            ]
                            [ text "Open in new tab"
                            , span [ class "text-xs" ] [ text "↗" ]
                            ]
                        ]
                    , case model.applicationView of
                        Just viewModel ->
                            Html.map ApplicationViewMsg (ApplicationView.view viewModel)

                        Nothing ->
                            div [ class "p-4 flex justify-center items-center" ]
                                [ div [ class "animate-spin h-8 w-8 border-4 border-purple-600 border-t-transparent rounded-full" ] [] ]
                    ]
                ]

          else
            text ""
        ]


viewHeader : Html Msg
viewHeader =
    div [ class "bg-white shadow" ]
        [ div [ class "max-w-7xl mx-auto py-4 px-4 sm:px-6 lg:px-8" ]
            [ div [ class "flex justify-between items-center" ]
                [ h1 [ class "text-2xl font-semibold text-gray-900" ]
                    [ text "Applications" ]
                , div [ class "flex items-center gap-4" ]
                    [ button
                        [ class "bg-purple-600 hover:bg-purple-700 text-white px-4 py-2 rounded-md text-sm"
                        , onClick RefreshApplications
                        ]
                        [ text "Refresh" ]
                    ]
                ]
            ]
        ]


viewApplications : Model -> Html Msg
viewApplications model =
    div [ class "mt-8" ]
        [ div [ class "flex flex-col gap-4" ]
            [ div [ class "flex justify-between items-center" ]
                [ div [ class "flex items-center gap-4" ]
                    [ div [ class "relative" ]
                        [ input
                            [ type_ "text"
                            , class "w-96 px-4 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-purple-600 focus:border-transparent"
                            , placeholder "Search applications..."
                            , value model.searchTerm
                            , onInput SearchTermChanged
                            ]
                            []
                        , if model.searchLoading then
                            div [ class "absolute right-3 top-2.5" ]
                                [ div [ class "animate-spin h-5 w-5 border-2 border-purple-600 border-t-transparent rounded-full" ] [] ]

                          else
                            text ""
                        ]
                    , label [ class "flex items-center gap-2" ]
                        [ input
                            [ type_ "checkbox"
                            , class "rounded border-gray-300 text-purple-600 focus:ring-purple-600"
                            , checked model.hasContactFilter
                            , onCheck ToggleContactFilter
                            ]
                            []
                        , span [ class "text-sm text-gray-700" ] [ text "Has Contact Info" ]
                        ]
                    ]
                ]
            , div [ class "bg-white shadow rounded-lg overflow-hidden" ]
                [ if model.isLoading then
                    div [ class "p-4 flex justify-center items-center" ]
                        [ div [ class "animate-spin h-8 w-8 border-4 border-purple-600 border-t-transparent rounded-full" ] [] ]

                  else if List.isEmpty model.applications then
                    div [ class "p-4 text-center text-gray-500" ]
                        [ text "No applications found" ]

                  else
                    table [ class "min-w-full divide-y divide-gray-200" ]
                        [ thead [ class "bg-gray-50" ]
                            [ tr []
                                [ th [ class "py-3 px-4 w-8" ] []
                                , th [ class "py-3 px-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider w-48" ]
                                    [ text "Name" ]
                                , th [ class "py-3 px-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider w-32" ]
                                    [ text "Carrier" ]
                                , th [ class "py-3 px-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider w-32" ]
                                    [ text "Status" ]
                                , th [ class "py-3 px-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider w-36" ]
                                    [ text "Phone" ]
                                , th [ class "py-3 px-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider w-48" ]
                                    [ text "Email" ]
                                , th [ class "py-3 px-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider w-28" ]
                                    [ text "Effective Date" ]
                                , th [ class "py-3 px-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider w-28" ]
                                    [ text "Date Started" ]
                                , th [ class "py-3 px-4 w-24" ] []
                                ]
                            ]
                        , tbody [ class "bg-white divide-y divide-gray-200" ]
                            (List.map (viewApplicationRow model) model.applications)
                        ]
                ]
            , viewPagination model
            ]
        ]


viewApplicationRow : Model -> ApplicationRow -> Html Msg
viewApplicationRow model app =
    tr [ class "border-b hover:bg-gray-50" ]
        [ td [ class "py-3 px-4 w-8" ]
            [ input [ type_ "checkbox", class "rounded border-gray-300" ] [] ]
        , td [ class "py-3 px-4 w-48" ] [ text (app.name |> Maybe.withDefault "") ]
        , td [ class "py-3 px-4 w-32" ] [ text (app.naic |> carrierFromNaic |> Maybe.map carrierToString |> Maybe.withDefault app.naic) ]
        , td [ class "py-3 px-4 w-32" ] [ viewStatus app.status ]
        , td [ class "py-3 px-4 text-gray-600 w-36 whitespace-nowrap" ]
            [ text (app.phone |> Maybe.withDefault "") ]
        , td [ class "py-3 px-4 text-gray-600 w-48 truncate" ]
            [ text (app.email |> Maybe.withDefault "") ]
        , td [ class "py-3 px-4 text-gray-600 w-28 whitespace-nowrap" ]
            [ text (app.effectiveDate |> Maybe.withDefault "") ]
        , td [ class "py-3 px-4 text-gray-600 w-28 whitespace-nowrap" ]
            [ text (app.dateStarted |> String.slice 0 10) ]
        , td [ class "py-3 px-4 w-24 text-right" ]
            [ button
                [ class "text-purple-600 hover:text-purple-700 px-3 py-1 rounded-md text-sm hover:bg-purple-50"
                , onClick (ViewApplication app.id)
                ]
                [ text "View" ]
            ]
        ]


viewName : String -> String
viewName naic =
    naic |> carrierFromNaic |> Maybe.map carrierToString |> Maybe.withDefault naic


viewStatus : Status -> Html msg
viewStatus status =
    let
        ( statusText, statusColor ) =
            case status of
                CompletedApp ->
                    ( "Completed", "text-green-600 bg-green-50" )

                WaitingReview ->
                    ( "Waiting Review", "text-yellow-600 bg-yellow-50" )

                PartialApplication ->
                    ( "Partial", "text-gray-600 bg-gray-50" )

                SubmissionIssue ->
                    ( "Submission Issue", "text-red-600 bg-red-50" )

                IssuedPolicy ->
                    ( "Issued", "text-green-600 bg-green-50" )

                DeclinedPolicy ->
                    ( "Declined", "text-red-600 bg-red-50" )

                AwaitingSignature ->
                    ( "Awaiting Signature", "text-orange-600 bg-orange-50" )

                Submitting ->
                    ( "Submitting", "text-purple-600 bg-purple-50" )

                Verifying ->
                    ( "Verifying", "text-blue-600 bg-blue-50" )
    in
    div [ class ("flex items-center gap-2 " ++ statusColor ++ " px-3 py-1 rounded-full w-fit") ]
        [ div [ class "w-2 h-2 rounded-full bg-current" ] []
        , span [ class "text-sm" ] [ text statusText ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveApplications
            (\value ->
                let
                    _ =
                        Debug.log "Received applications" value
                in
                ApplicationsReceived (Decode.decodeValue applicationListDecoder value)
            )
        , statusUpdate StatusUpdate
        , receiveApplication
            (\value ->
                case model.selectedApplicationId of
                    Just id ->
                        -- If we have a selected application ID, treat it as a modal view response
                        ApplicationReceived model.producerConfig (Decode.decodeValue applicationViewDecoder value)

                    Nothing ->
                        -- Otherwise treat it as a general application update
                        ApplicationUpdated value
            )
        , if model.showApplicationModal then
            Browser.Events.onKeyDown (Decode.map HandleKeyPress (Decode.field "key" Decode.string))

          else
            Sub.none
        , case model.applicationView of
            Just viewModel ->
                Sub.map ApplicationViewMsg (ApplicationView.subscriptions viewModel)

            Nothing ->
                Sub.none
        ]



-- DECODERS


applicationDecoder : Decode.Decoder ApplicationRow
applicationDecoder =
    Decode.succeed ApplicationRow
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "naic" Decode.string
        |> Pipeline.required "name" (Decode.nullable Decode.string)
        |> Pipeline.required "status" statusDecoder
        |> Pipeline.optional "phone" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "email" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "effectiveDate" (Decode.nullable Decode.string) Nothing
        |> Pipeline.required "dateStarted" Decode.string


applicationListDecoder : Decode.Decoder ApplicationsResponse
applicationListDecoder =
    Decode.map2 ApplicationsResponse
        (Decode.field "applications" (Decode.list applicationDecoder))
        (Decode.field "pagination"
            (Decode.map4 PaginationInfo
                (Decode.field "total" Decode.int)
                (Decode.field "page" Decode.int)
                (Decode.field "pageSize" Decode.int)
                (Decode.field "totalPages" Decode.int)
            )
        )


statusDecoder : Decode.Decoder Status
statusDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "completed" ->
                        Decode.succeed CompletedApp

                    "waiting_review" ->
                        Decode.succeed WaitingReview

                    "partial" ->
                        Decode.succeed PartialApplication

                    "submission_issue" ->
                        Decode.succeed SubmissionIssue

                    "issued" ->
                        Decode.succeed IssuedPolicy

                    "declined" ->
                        Decode.succeed DeclinedPolicy

                    "awaiting_signature" ->
                        Decode.succeed AwaitingSignature

                    "submitting" ->
                        Decode.succeed Submitting

                    "verifying" ->
                        Decode.succeed Verifying

                    _ ->
                        let
                            _ =
                                Debug.log "Unknown status" str
                        in
                        Decode.succeed PartialApplication
            )


cleanCarrierName : String -> String
cleanCarrierName name =
    name
        |> String.replace " Application" ""



-- Debounce config


searchDebounceConfig : Debounce.Config Msg
searchDebounceConfig =
    { strategy = Debounce.later 300
    , transform = SearchDebouncerMsg
    }



-- Helper function to perform the actual search


performSearch : String -> Cmd Msg
performSearch term =
    requestRefresh
        { page = 0
        , pageSize = 20
        , searchTerm = term
        , hasContactFilter = False
        , naics = []
        }


viewPagination : Model -> Html Msg
viewPagination model =
    if model.totalPages <= 1 then
        text ""

    else
        div [ class "flex justify-between items-center mt-4" ]
            [ div [ class "text-sm text-gray-700" ]
                [ text
                    (String.fromInt (model.currentPage * model.pageSize + 1)
                        ++ "-"
                        ++ String.fromInt (Basics.min ((model.currentPage + 1) * model.pageSize) model.total)
                        ++ " of "
                        ++ String.fromInt model.total
                    )
                ]
            , div [ class "flex items-center gap-2" ]
                [ button
                    [ class "px-3 py-1 border border-gray-300 rounded-md text-sm disabled:opacity-50"
                    , disabled (model.currentPage == 0)
                    , onClick (ChangePage (model.currentPage - 1))
                    ]
                    [ text "Previous" ]
                , button
                    [ class "px-3 py-1 border border-gray-300 rounded-md text-sm disabled:opacity-50"
                    , disabled (model.currentPage >= model.totalPages - 1)
                    , onClick (ChangePage (model.currentPage + 1))
                    ]
                    [ text "Next" ]
                ]
            ]


stopPropagation : String -> Attribute Msg
stopPropagation event =
    Html.Events.stopPropagationOn event (Decode.succeed ( NoOp, True ))


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
