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
    , isLoading : Bool
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
    , producerConfigJSON : Decode.Value
    , producerConfig : Dict Int Producer.ProducerConfig
    , selectedProducerId : Int
    , selectedApplicationId : Maybe String
    , naicsFilter : List String
    , showProducerModal : Bool
    , stats : Maybe ApplicationStats
    , statusFilter : Maybe String
    , selectedStatsFilter : String
    }


type alias ApplicationStats =
    { total : Int
    , submitted : Int
    , waitingReview : Int
    , completed : Int
    }



-- INIT


init : Decode.Value -> ( Model, Cmd Msg )
init producerConfig =
    let
        producerConfigDict =
            Decode.decodeValue Producer.producerConfigDecoder producerConfig
                |> Result.toMaybe
                |> Maybe.withDefault Dict.empty
    in
    ( { applications = []
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
      , producerConfigJSON = producerConfig
      , producerConfig = producerConfigDict
      , selectedProducerId = 1
      , selectedApplicationId = Nothing
      , naicsFilter = []
      , showProducerModal = False
      , stats = Nothing
      , statusFilter = Nothing
      , selectedStatsFilter = "total"
      }
    , Cmd.batch
        [ requestRefresh
            { page = 0
            , pageSize = 20
            , searchTerm = ""
            , hasContactFilter = False
            , naics = []
            , status = Nothing
            }
        , requestApplicationStats ()
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | ViewApplication String
    | ApplicationReceived (Maybe Producer.ProducerConfig) (Result Decode.Error ApplicationView.Application)
    | SearchTermChanged String
    | ClearSearch
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
    | ToggleProducerModal
    | SelectProducer Int
    | CloseProducerModal
    | GotApplicationStats (Result Decode.Error ApplicationStats)
    | FilterByStatus String
    | ClearStatusFilter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ViewApplication id ->
            ( { model
                | showApplicationModal = True
                , applicationView = Nothing
                , selectedApplicationId = Just id
              }
            , requestApplication { id = id }
            )

        ApplicationReceived selectedProducer result ->
            case result of
                Ok application ->
                    let
                        ( viewModel, viewCmd ) =
                            ApplicationView.init selectedProducer application
                    in
                    ( { model
                        | applicationView = Just viewModel
                        , isLoading = False
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
                            , status = model.statusFilter
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
                , status = model.statusFilter
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
                , status = model.statusFilter
                }
            )

        ApplicationsReceived result ->
            case result of
                Ok response ->
                    let

                        -- No need to fetch full applications unless opening a modal
                        newModel =
                            { model
                                | applications = response.applications
                                , total = response.pagination.total
                                , currentPage = response.pagination.page
                                , pageSize = response.pagination.pageSize
                                , totalPages = response.pagination.totalPages
                                , isLoading = False
                                , searchLoading = False
                                , error = Nothing
                            }
                    in
                    ( newModel, Cmd.none )

                Err error ->
                    ( { model
                        | error = Just (Decode.errorToString error)
                        , isLoading = False
                        , searchLoading = False
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
                , status = model.statusFilter
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

        ToggleProducerModal ->
            ( { model | showProducerModal = True }, Cmd.none )

        SelectProducer id ->
            ( { model | selectedProducerId = id }, Cmd.none )

        CloseProducerModal ->
            ( { model | showProducerModal = False }, Cmd.none )

        GotApplicationStats result ->
            case result of
                Ok stats ->
                    ( { model | stats = Just stats }, Cmd.none )

                Err _ ->
                    ( { model | stats = Nothing }, Cmd.none )

        FilterByStatus status ->
            ( { model
                | statusFilter = Just status
                , currentPage = 0
                , selectedStatsFilter = status
                , searchTerm = ""
                , searchLoading = False
              }
            , requestRefresh
                { page = 0
                , pageSize = model.pageSize
                , searchTerm = ""
                , hasContactFilter = model.hasContactFilter
                , naics = model.naicsFilter
                , status = Just status
                }
            )

        ClearStatusFilter ->
            ( { model
                | statusFilter = Nothing
                , selectedStatsFilter = "total"
                , searchTerm = ""
                , searchLoading = False
              }
            , requestRefresh
                { page = 0
                , pageSize = model.pageSize
                , searchTerm = ""
                , hasContactFilter = model.hasContactFilter
                , naics = model.naicsFilter
                , status = Nothing
                }
            )

        ClearSearch ->
            ( { model
                | searchTerm = ""
                , searchLoading = False
                , selectedStatsFilter = "total"
                , statusFilter = Nothing
              }
            , requestRefresh
                { page = 0
                , pageSize = model.pageSize
                , searchTerm = ""
                , hasContactFilter = model.hasContactFilter
                , naics = model.naicsFilter
                , status = Nothing
                }
            )



-- Add completion logic here
-- VIEW


view : Model -> Html Msg
view model =
    let
        applicationId =
            model.selectedApplicationId |> Maybe.withDefault ""
    in
    div [ class "min-h-screen bg-white relative" ]
        [ viewHeader model
        , div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
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


viewHeader : Model -> Html Msg
viewHeader model =
    let
        selectedProducer =
            Dict.get model.selectedProducerId model.producerConfig
                |> Maybe.map (\p -> p.firstName ++ " " ++ p.lastName)
                |> Maybe.withDefault "Select Producer"
    in
    div [ class "bg-white border-b" ]
        [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
            [ div [ class "flex items-center justify-between h-16" ]
                [ div [ class "flex items-center gap-8" ]
                    [ div [ class "w-10 h-10 bg-gray-100 rounded flex items-center justify-center" ]
                        [ text "Logo" ]
                    , div [ class "flex items-center gap-6" ]
                        [ a [ class "text-gray-900 font-medium" ] [ text "Medicare Applications" ]
                        , a [ class "text-gray-500" ] [ text "Contacts" ]
                        ]
                    ]
                , div [ class "flex items-center gap-4" ]
                    [ button
                        [ class "flex items-center gap-2 px-3 py-2 rounded-md hover:bg-gray-50"
                        , onClick ToggleProducerModal
                        ]
                        [ div [ class "w-8 h-8 bg-gray-100 rounded-full flex items-center justify-center text-sm" ]
                            [ text (String.left 1 selectedProducer) ]
                        , span [ class "text-sm text-gray-700" ] [ text selectedProducer ]
                        , span [ class "text-gray-400" ] [ text "▼" ]
                        ]
                    ]
                ]
            ]
        , if model.showProducerModal then
            div
                [ class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50"
                , onClick CloseProducerModal
                ]
                [ div
                    [ class "bg-white rounded-lg shadow-xl w-96 max-h-[90vh] overflow-y-auto"
                    , stopPropagation "click"
                    ]
                    [ div [ class "p-4 border-b" ]
                        [ h3 [ class "text-lg font-medium" ] [ text "Select Producer" ]
                        ]
                    , div [ class "p-2" ]
                        [ div [ class "space-y-1" ]
                            (model.producerConfig
                                |> Dict.toList
                                |> List.map
                                    (\( id, producer ) ->
                                        button
                                            [ class
                                                ("w-full flex items-center gap-3 px-3 py-2 rounded-md text-left "
                                                    ++ (if id == model.selectedProducerId then
                                                            "bg-purple-50 text-purple-700"

                                                        else
                                                            "hover:bg-gray-50"
                                                       )
                                                )
                                            , onClick (SelectProducer id)
                                            ]
                                            [ div
                                                [ class
                                                    ("w-8 h-8 rounded-full flex items-center justify-center text-sm "
                                                        ++ (if id == model.selectedProducerId then
                                                                "bg-purple-100"

                                                            else
                                                                "bg-gray-100"
                                                           )
                                                    )
                                                ]
                                                [ text (String.left 1 (producer.firstName ++ " " ++ producer.lastName)) ]
                                            , div [ class "flex-1" ]
                                                [ div [ class "font-medium" ]
                                                    [ text (producer.firstName ++ " " ++ producer.lastName) ]
                                                , div [ class "text-sm text-gray-500" ]
                                                    [ text producer.email ]
                                                ]
                                            ]
                                    )
                            )
                        ]
                    ]
                ]

          else
            text ""
        ]


viewApplications : Model -> Html Msg
viewApplications model =
    div [ class "mt-8" ]
        [ viewStatistics model
        , div [ class "flex flex-col gap-4 mt-8" ]
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

                          else if not (String.isEmpty model.searchTerm) then
                            button
                                [ class "absolute right-3 top-2.5 text-gray-400 hover:text-gray-600"
                                , onClick ClearSearch
                                ]
                                [ text "×" ]

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
                , button
                    [ class "bg-purple-600 hover:bg-purple-700 text-white px-4 py-2 rounded-md text-sm"
                    , onClick RefreshApplications
                    ]
                    [ text "Refresh" ]
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
                ApplicationsReceived (Decode.decodeValue applicationListDecoder value)
            )
        , statusUpdate StatusUpdate
        , receiveApplication
            (\value ->
                case model.selectedApplicationId of
                    Just id ->
                        -- If we have a selected application ID, treat it as a modal view response
                        ApplicationReceived
                            (Dict.get model.selectedProducerId model.producerConfig)
                            (Decode.decodeValue applicationViewDecoder value)

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
        , receiveApplicationStats
            (\value ->
                GotApplicationStats (Decode.decodeValue applicationStatsDecoder value)
            )
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


paginationDecoder : Decode.Decoder PaginationInfo
paginationDecoder =
    Decode.succeed PaginationInfo
        |> Pipeline.required "total" Decode.int
        |> Pipeline.required "page" Decode.int
        |> Pipeline.required "pageSize" Decode.int
        |> Pipeline.required "totalPages" Decode.int


applicationListDecoder : Decode.Decoder ApplicationsResponse
applicationListDecoder =
    Decode.succeed ApplicationsResponse
        |> Pipeline.required "applications" (Decode.list applicationDecoder)
        |> Pipeline.required "pagination" paginationDecoder
        |> Pipeline.optional "isLoading" Decode.bool False


statusDecoder : Decode.Decoder Status
statusDecoder =
    Decode.oneOf
        [ Decode.null PartialApplication
        , Decode.string
            |> Decode.andThen
                (\str ->
                    case str of
                        "completed" ->
                            Decode.succeed CompletedApp

                        "waiting_review" ->
                            Decode.succeed WaitingReview

                        "partial" ->
                            Decode.succeed PartialApplication

                        "partial_application" ->
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
                            Decode.fail ("Unknown status: " ++ str)
                )
        ]


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
        , status = Nothing
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


viewStatistics : Model -> Html Msg
viewStatistics model =
    let
        -- Don't show any card as selected if there's a search term
        isSelected status =
            String.isEmpty model.searchTerm && model.selectedStatsFilter == status
    in
    div [ class "grid grid-cols-4 gap-6" ]
        [ viewStatCard "Total Applications"
            (model.stats |> Maybe.map (.total >> String.fromInt) |> Maybe.withDefault "-")
            ""
            ""
            ""
            (onClick ClearStatusFilter)
            (isSelected "total")
        , viewStatCard "Application Submissions"
            (model.stats |> Maybe.map (.submitted >> String.fromInt) |> Maybe.withDefault "-")
            ""
            ""
            ""
            (onClick (FilterByStatus "awaiting_signature"))
            (isSelected "awaiting_signature")
        , viewStatCard "Waiting Review"
            (model.stats |> Maybe.map (.waitingReview >> String.fromInt) |> Maybe.withDefault "-")
            ""
            ""
            ""
            (onClick (FilterByStatus "waiting_review"))
            (isSelected "waiting_review")
        , viewStatCard "Completed Apps"
            (model.stats |> Maybe.map (.completed >> String.fromInt) |> Maybe.withDefault "-")
            ""
            ""
            ""
            (onClick (FilterByStatus "completed"))
            (isSelected "completed")
        ]


viewStatCard : String -> String -> String -> String -> String -> Attribute Msg -> Bool -> Html Msg
viewStatCard title value changeValue changeColor comparisonText clickHandler isSelected =
    div
        [ class
            (if isSelected then
                "bg-gray-900 text-white rounded-lg p-6 shadow-sm hover:shadow-md cursor-pointer transition-all duration-200"

             else
                "bg-white text-gray-900 rounded-lg p-6 shadow-sm hover:shadow-md cursor-pointer transition-all duration-200"
            )
        , clickHandler
        ]
        [ div
            [ class
                (if isSelected then
                    "text-gray-300"

                 else
                    "text-gray-600"
                )
            ]
            [ text title ]
        , div [ class "mt-2 flex items-baseline gap-2" ]
            [ div [ class "text-3xl font-semibold" ] [ text value ]
            , if not (String.isEmpty changeValue) then
                div [ class ("text-sm font-medium " ++ changeColor) ]
                    [ text ("↑ " ++ changeValue) ]

              else
                text ""
            ]
        , if not (String.isEmpty comparisonText) then
            div
                [ class
                    (if isSelected then
                        "text-gray-300"

                     else
                        "text-gray-500"
                    )
                ]
                [ text comparisonText ]

          else
            text ""
        ]


applicationStatsDecoder : Decode.Decoder ApplicationStats
applicationStatsDecoder =
    Decode.map4 ApplicationStats
        (Decode.field "total" Decode.int)
        (Decode.field "submitted" Decode.int)
        (Decode.field "waitingReview" Decode.int)
        (Decode.field "completed" Decode.int)
