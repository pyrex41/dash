port module Dashboard exposing (Model, Msg, init, update, view)

import ApplicationView
import Browser
import Debounce exposing (Debounce)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import SchemaDecoder as SD


-- PORTS


type alias PaginationInfo =
    { total : Int
    , page : Int
    , pageSize : Int
    , totalPages : Int
    }


type alias ApplicationsResponse =
    { applications : List Application
    , pagination : PaginationInfo
    }


port receiveApplications : (Decode.Value -> msg) -> Sub msg


port requestRefresh : { page : Int, pageSize : Int, searchTerm : String, hasContactFilter : Bool } -> Cmd msg


port exportToCsv : { searchTerm : String, hasContactFilter : Bool, hasCSGFilter : Bool } -> Cmd msg


port requestApplication : { id : String } -> Cmd msg


port receiveApplication : (Decode.Value -> msg) -> Sub msg



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
    { quoteSent : Int
    , submissions : Int
    , waitingReview : Int
    , completedApps : Int
    , applications : List Application
    , searchTerm : String
    , hasContactFilter : Bool
    , hasCSGFilter : Bool
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
    }


type alias Application =
    { id : String
    , userId : String
    , userEmail : Maybe String
    , createdAt : String
    , dateStarted : String
    , status : Status
    , state : Maybe String
    , data : Decode.Value
    , carrier : String
    , booking : Maybe Booking
    , csgApplication : Maybe CsgApplication
    }


type alias CsgApplication =
    { key : String
    , brokerEmail : Maybe String
    }


type alias Booking =
    { email : String
    , phone : Maybe String
    , url : String
    , status : String
    }


type Status
    = CompletedApp
    | WaitingReview
    | QuoteSent
    | SubmittedToCSG
    | CallBooked



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { quoteSent = 0
      , submissions = 0
      , waitingReview = 0
      , completedApps = 0
      , applications = []
      , searchTerm = ""
      , hasContactFilter = False
      , hasCSGFilter = False
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
      }
    , requestRefresh
        { page = 0
        , pageSize = 20
        , searchTerm = ""
        , hasContactFilter = False
        }
    )



-- UPDATE


type Msg
    = NoOp
    | ViewApplication String
    | ApplicationReceived (Result Decode.Error SD.ApplicationWithSchema)
    | CompleteApplication String
    | SearchTermChanged String
    | ToggleContactFilter Bool
    | ToggleCSGFilter Bool
    | RefreshApplications
    | ApplicationsReceived (Result Decode.Error ApplicationsResponse)
    | ExportToCsv
    | SearchDebouncerMsg Debounce.Msg
    | PerformSearch String
    | ChangePage Int
    | ApplicationViewMsg ApplicationView.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ViewApplication id ->
            {-- let
                _ = Debug.log "ViewApplication" id
            in --}
            ( { model 
                | showApplicationModal = True
                , applicationView = Nothing
              }
            , requestApplication { id = id }
            )

        ApplicationReceived result ->
            {-- let
                _ = Debug.log "ApplicationReceived" result
            in --}
            case result of
                Ok application ->
                    {-- let
                        _ = Debug.log "Creating ApplicationView with" application
                        (viewModel, viewCmd) =
                            ApplicationView.init (Just application)
                    in --}
                    let
                        (viewModel, viewCmd) =
                            ApplicationView.init (Just application)
                    in
                    ( { model 
                        | applicationView = Just viewModel
                        , isLoading = False 
                      }
                    , Cmd.map ApplicationViewMsg viewCmd
                    )

                Err error ->
                    {-- let
                        _ = Debug.log "ApplicationReceived Error" (Decode.errorToString error)
                    in --}
                    ( { model 
                        | error = Just (Decode.errorToString error)
                        , isLoading = False 
                      }
                    , Cmd.none
                    )

        CompleteApplication id ->
            ( model, Cmd.none )

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
                            }
                        )

                    else
                        ( model.searchDebouncer, Cmd.none )
            in
            ( { model
                | searchTerm = term
                , searchDebouncer = debouncer
                , searchLoading = shouldSearch
                , isLoading = shouldRefresh
              }
            , cmd
            )

        SearchDebouncerMsg debouncerMsg ->
            let
                ( debouncer, cmd ) =
                    Debounce.update
                        searchDebounceConfig
                        (Debounce.takeLast performSearch)
                        debouncerMsg
                        model.searchDebouncer
            in
            ( { model | searchDebouncer = debouncer }
            , cmd
            )

        ToggleContactFilter value ->
            ( { model | hasContactFilter = value, isLoading = True }
            , requestRefresh
                { page = model.currentPage
                , pageSize = model.pageSize
                , searchTerm = model.searchTerm
                , hasContactFilter = value
                }
            )

        ToggleCSGFilter value ->
            ( { model | hasCSGFilter = value }, Cmd.none )

        RefreshApplications ->
            ( { model | isLoading = True }, requestRefresh { page = 0, pageSize = 20, searchTerm = "", hasContactFilter = False } )

        ApplicationsReceived (Ok response) ->
            ( { model
                | applications = response.applications
                , isLoading = False
                , searchLoading = False
                , quoteSent = List.length (List.filter (\a -> a.status == QuoteSent) response.applications)
                , submissions = List.length (List.filter (\a -> a.status == QuoteSent) response.applications)
                , waitingReview = List.length (List.filter (\a -> a.status == WaitingReview) response.applications)
                , completedApps = List.length (List.filter (\a -> a.status == CompletedApp) response.applications)
                , total = response.pagination.total
                , totalPages = response.pagination.totalPages
                , currentPage = response.pagination.page
                , pageSize = response.pagination.pageSize
              }
            , Cmd.none
            )

        ApplicationsReceived (Err _) ->
            ( { model | error = Just "Failed to load applications", isLoading = False, searchLoading = False }, Cmd.none )

        ExportToCsv ->
            ( model
            , exportToCsv
                { searchTerm = model.searchTerm
                , hasContactFilter = model.hasContactFilter
                , hasCSGFilter = model.hasCSGFilter
                }
            )

        PerformSearch term ->
            ( model
            , requestRefresh
                { page = 0
                , pageSize = 20
                , searchTerm = term
                , hasContactFilter = False
                }
            )

        ChangePage page ->
            ( { model | currentPage = page, isLoading = True }
            , requestRefresh
                { page = page
                , pageSize = model.pageSize
                , searchTerm = model.searchTerm
                , hasContactFilter = False
                }
            )

        ApplicationViewMsg viewMsg ->
            case viewMsg of
                ApplicationView.CloseModal ->
                    ( { model 
                        | applicationView = Nothing
                        , showApplicationModal = False
                      }
                    , Cmd.none
                    )
                _ ->
                    case model.applicationView of
                        Just viewModel ->
                            let
                                (newViewModel, viewCmd) = 
                                    ApplicationView.update viewMsg viewModel
                            in
                            ( { model | applicationView = Just newViewModel }
                            , Cmd.map ApplicationViewMsg viewCmd
                            )
                        Nothing ->
                            ( model, Cmd.none )



-- Add completion logic here
-- VIEW


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-white relative" ]
        [ viewHeader
        , div [ class "max-w-7xl mx-auto" ]
            [ viewMetrics model
            , viewApplications model
            ]
        , if model.showApplicationModal then
            div [ 
                class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50"
                , onClick (ApplicationViewMsg ApplicationView.CloseModal)
                ]
                [ div [ 
                    class "bg-white rounded-lg shadow-xl max-w-4xl w-full max-h-[90vh] overflow-y-auto"
                    , stopPropagation "click"
                    ]
                    [ case model.applicationView of
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
    header [ class "border-b" ]
        [ div [ class "max-w-7xl mx-auto flex justify-between items-center p-4" ]
            [ div [ class "flex items-center gap-4" ]
                [ h1 [ class "text-xl font-bold" ] [ text "White Labeled Logo" ]
                , nav [ class "flex gap-4" ]
                    [ a [ href "#" ] [ text "Medigap Applications" ]
                    , a [ href "#" ] [ text "Contacts" ]
                    ]
                ]
            , div [ class "flex items-center gap-4" ]
                [ button [ class "bg-black text-white px-4 py-2 rounded" ]
                    [ text "Send Something" ]
                , div [ class "flex items-center gap-2" ]
                    [ span [ class "w-8 h-8 bg-gray-200 rounded-full flex items-center justify-center" ]
                        [ text "J" ]
                    , text "John Doe"
                    ]
                ]
            ]
        ]


viewMetrics : Model -> Html Msg
viewMetrics model =
    div [ class "grid grid-cols-4 gap-4 p-4" ]
        [ viewMetricCard "Quote Sent" model.quoteSent "80" "up"
        , viewMetricCard "Submissions" model.submissions "20" "up"
        , viewMetricCard "Waiting Review" model.waitingReview "10" "down"
        , viewMetricCard "Completed Apps" model.completedApps "" ""
        ]


viewMetricCard : String -> Int -> String -> String -> Html Msg
viewMetricCard title value percentage direction =
    div [ class "p-4 rounded-lg border" ]
        [ div [ class "text-sm text-gray-600" ] [ text title ]
        , div [ class "text-3xl font-bold" ] [ text (String.fromInt value) ]
        , if percentage /= "" then
            div
                [ class
                    ("text-sm "
                        ++ (if direction == "up" then
                                "text-green-500"

                            else
                                "text-red-500"
                           )
                    )
                ]
                [ text (percentage ++ "% vs last month") ]

          else
            text ""
        ]


viewApplications : Model -> Html Msg
viewApplications model =
    div [ class "p-4" ]
        [ div [ class "flex justify-between items-center mb-4" ]
            [ h2 [ class "text-xl" ] [ text "Applications" ]
            , div [ class "flex gap-4 items-center" ]
                [ label [ class "flex items-center gap-2 text-sm text-gray-600" ]
                    [ input
                        [ type_ "checkbox"
                        , class "rounded border-gray-300"
                        , checked model.hasContactFilter
                        , onCheck ToggleContactFilter
                        ]
                        []
                    , text "Has contact info"
                    ]
                , div [ class "relative" ]
                    [ input
                        [ class "px-3 py-1 border rounded"
                        , placeholder "Search"
                        , type_ "search"
                        , value model.searchTerm
                        , onInput SearchTermChanged
                        ]
                        []
                    , if model.searchLoading && String.length model.searchTerm >= 3 then
                        div
                            [ class "absolute right-2 top-1/2 transform -translate-y-1/2" ]
                            [ div
                                [ class "animate-spin h-4 w-4 border-2 border-gray-300 border-t-purple-600 rounded-full" ]
                                []
                            ]

                      else
                        text ""
                    ]
                ]
            ]
        , if model.isLoading then
            div [ class "flex justify-center items-center py-8" ]
                [ div [ class "animate-spin rounded-full h-12 w-12 border-b-2 border-purple-600" ] []
                , span [ class "sr-only" ] [ text "Loading..." ]
                ]

          else
            div []
                [ table [ class "w-full border-collapse" ]
                    [ thead []
                        [ tr [ class "border-b text-left" ]
                            [ th [ class "w-8 py-3 px-4" ]
                                [ input [ type_ "checkbox", class "rounded border-gray-300" ] [] ]
                            , th [ class "py-3 px-4 font-medium text-sm text-gray-600 w-48" ] [ text "Name" ]
                            , th [ class "py-3 px-4 font-medium text-sm text-gray-600 w-32" ] [ text "Carrier" ]
                            , th [ class "py-3 px-4 font-medium text-sm text-gray-600 w-32" ] [ text "Status" ]
                            , th [ class "py-3 px-4 font-medium text-sm text-gray-600 w-36" ] [ text "Phone Number" ]
                            , th [ class "py-3 px-4 font-medium text-sm text-gray-600 w-48" ] [ text "Email address" ]
                            , th [ class "py-3 px-4 font-medium text-sm text-gray-600 w-28" ] [ text "Effective Date" ]
                            , th [ class "py-3 px-4 font-medium text-sm text-gray-600 w-28" ] [ text "Date Started" ]
                            , th [ class "py-3 px-4 font-medium text-sm text-gray-600 w-24" ] [ text "" ]
                            ]
                        ]
                    , tbody []
                        (List.map viewApplicationRow model.applications)
                    ]
                , viewPagination model
                ]
        ]


viewApplicationRow : Application -> Html Msg
viewApplicationRow app =
    let
        getName =
            let
                firstName =
                    app.data
                        |> Decode.decodeValue (Decode.at [ "applicant_info", "f_name" ] Decode.string)
                        |> Result.toMaybe
                        |> Maybe.withDefault ""

                lastName =
                    app.data
                        |> Decode.decodeValue (Decode.at [ "applicant_info", "l_name" ] Decode.string)
                        |> Result.toMaybe
                        |> Maybe.withDefault ""
            in
            String.trim (firstName ++ " " ++ lastName)

        getApplicantInfo field =
            app.data
                |> Decode.decodeValue (Decode.at [ "applicant_info", field ] Decode.string)
                |> Result.toMaybe

        getEmail =
            [ app.userEmail -- From user
            , getApplicantInfo "email" -- From application data
            , Maybe.map .email app.booking -- From booking
            ]
                |> List.filterMap identity
                |> List.head
                |> Maybe.withDefault ""

        getPhone =
            [ getApplicantInfo "phone" -- From application data
            , Maybe.andThen .phone app.booking -- From booking
            ]
                |> List.filterMap identity
                |> List.head
                |> Maybe.withDefault ""

        getEffectiveDate =
            app.data
                |> Decode.decodeValue (Decode.at [ "applicant_info", "effective_date" ] Decode.string)
                |> Result.toMaybe

        formatDate dateString =
            dateString
                |> String.split "T"
                |> List.head
                |> Maybe.withDefault dateString
    in
    tr [ class "border-b hover:bg-gray-50" ]
        [ td [ class "py-3 px-4 w-8" ]
            [ input [ type_ "checkbox", class "rounded border-gray-300" ] [] ]
        , td [ class "py-3 px-4 w-48" ] [ text getName ]
        , td [ class "py-3 px-4 w-32" ] [ text app.carrier ]
        , td [ class "py-3 px-4 w-32" ] [ viewStatus app.status ]
        , td [ class "py-3 px-4 text-gray-600 w-36 whitespace-nowrap" ] [ text getPhone ]
        , td [ class "py-3 px-4 text-gray-600 w-48 truncate" ] [ text getEmail ]
        , td [ class "py-3 px-4 text-gray-600 w-28 whitespace-nowrap" ]
            [ text (getEffectiveDate |> Maybe.map formatDate |> Maybe.withDefault "") ]
        , td [ class "py-3 px-4 text-gray-600 w-28 whitespace-nowrap" ] [ text (formatDate app.dateStarted) ]
        , td [ class "py-3 px-4 w-24" ]
            [ button
                [ class "bg-purple-600 hover:bg-purple-700 text-white px-4 py-2 rounded-md text-sm"
                , onClick (ViewApplication app.id)
                ]
                [ text "View" ]
            ]
        ]


viewStatus : Status -> Html Msg
viewStatus status = 
    let
        ( statusText, statusColor ) =
            case status of
                CompletedApp ->
                    ( "Completed App", "text-green-600 bg-green-50" )

                WaitingReview ->
                    ( "Waiting Review", "text-red-600 bg-red-50" )

                QuoteSent ->
                    ( "Started", "text-blue-600 bg-blue-50" )

                SubmittedToCSG ->
                    ( "Submitted to CSG", "text-purple-600 bg-purple-50" )

                CallBooked ->
                    ( "Intro Call", "text-orange-600 bg-orange-50" )
    in
    div [ class ("flex items-center gap-2 " ++ statusColor ++ " px-3 py-1 rounded-full w-fit") ]
        [ div [ class "w-2 h-2 rounded-full bg-current" ] []
        , span [ class "text-sm" ] [ text statusText ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveApplications
            (Decode.decodeValue applicationListDecoder >> ApplicationsReceived)
        , receiveApplication
            (Decode.decodeValue SD.applicationDecoder >> ApplicationReceived)
        ]



-- DECODERS


-- jdebug : String -> Decode.Decoder a -> Decode.Decoder a
-- jdebug message decoder =
--     Decode.value
--         |> Decode.andThen (debugHelper message decoder)


-- debugHelper : String -> Decode.Decoder a -> Decode.Value -> Decode.Decoder a
-- debugHelper message decoder value =
--     let
--         _ =
--             Debug.log message (Decode.decodeValue decoder value)
--     in
--     decoder


applicationDecoder : Decode.Decoder Application
applicationDecoder =
    Decode.succeed Application
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "userId" Decode.string
        |> Pipeline.optional "userEmail" (Decode.nullable Decode.string) Nothing
        |> Pipeline.required "createdAt" Decode.string
        |> Pipeline.required "dateStarted" Decode.string
        |> Pipeline.required "status" statusDecoder
        |> Pipeline.optional "state" (Decode.nullable Decode.string) Nothing
        |> Pipeline.required "data" Decode.value
        |> Pipeline.required "name" (Decode.map cleanCarrierName Decode.string)
        |> Pipeline.optional "booking" (Decode.nullable bookingDecoder) Nothing
        |> Pipeline.optional "csgApplication" (Decode.nullable csgApplicationDecoder) Nothing
        --|> jdebug "Application Decoder"


applicationListDecoder : Decode.Decoder ApplicationsResponse
applicationListDecoder =
    Decode.map2 ApplicationsResponse
        (Decode.field "applications" (Decode.list applicationDecoder))
        (Decode.field "pagination" paginationDecoder)


paginationDecoder : Decode.Decoder PaginationInfo
paginationDecoder =
    Decode.map4 PaginationInfo
        (Decode.field "total" Decode.int)
        (Decode.field "page" Decode.int)
        (Decode.field "pageSize" Decode.int)
        (Decode.field "totalPages" Decode.int)


statusDecoder : Decode.Decoder Status
statusDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "completed" ->
                        Decode.succeed CompletedApp

                    "review" ->
                        Decode.succeed WaitingReview

                    "quote" ->
                        Decode.succeed QuoteSent

                    "submitted_to_csg" ->
                        Decode.succeed SubmittedToCSG

                    "call_booked" ->
                        Decode.succeed CallBooked

                    _ ->
                        Decode.succeed QuoteSent
            )


bookingDecoder : Decode.Decoder Booking
bookingDecoder =
    Decode.succeed Booking
        |> Pipeline.required "email" Decode.string
        |> Pipeline.optional "phone" (Decode.nullable Decode.string) Nothing
        |> Pipeline.required "url" Decode.string
        |> Pipeline.required "status" Decode.string


csgApplicationDecoder : Decode.Decoder CsgApplication
csgApplicationDecoder =
    Decode.succeed CsgApplication
        |> Pipeline.required "key" Decode.string
        |> Pipeline.optional "brokerEmail" (Decode.nullable Decode.string) Nothing


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
        }


viewPagination : Model -> Html Msg
viewPagination model =
    div [ class "mt-4 flex items-center justify-between border-t border-gray-200 px-4 py-3 sm:px-6" ]
        [ div [ class "flex flex-1 justify-between sm:hidden" ]
            [ button
                [ class "relative inline-flex items-center rounded-md border border-gray-300 bg-white px-4 py-2 text-sm font-medium text-gray-700 hover:bg-gray-50"
                , onClick (ChangePage (model.currentPage - 1))
                , disabled (model.currentPage <= 0)
                ]
                [ text "Previous" ]
            , button
                [ class "relative ml-3 inline-flex items-center rounded-md border border-gray-300 bg-white px-4 py-2 text-sm font-medium text-gray-700 hover:bg-gray-50"
                , onClick (ChangePage (model.currentPage + 1))
                , disabled (model.currentPage >= model.totalPages - 1)
                ]
                [ text "Next" ]
            ]
        , div [ class "hidden sm:flex sm:flex-1 sm:items-center sm:justify-between" ]
            [ div [ class "text-sm text-gray-700" ]
                [ span [] [ text "Showing " ]
                , span [ class "font-medium" ]
                    [ text (String.fromInt (model.currentPage * model.pageSize + 1))
                    , text " to "
                    , text (String.fromInt (Basics.min ((model.currentPage + 1) * model.pageSize) model.total))
                    ]
                , span [] [ text " of " ]
                , span [ class "font-medium" ] [ text (String.fromInt model.total) ]
                , span [] [ text " results" ]
                ]
            , div [ class "flex items-center gap-2" ]
                [ viewPageButtons model ]
            ]
        ]


viewPageButtons : Model -> Html Msg
viewPageButtons model =
    let
        pageNumbers =
            List.range 0 (model.totalPages - 1)
                |> List.filter
                    (\page ->
                        page
                            == 0
                            || page
                            == model.totalPages
                            - 1
                            || abs (page - model.currentPage)
                            <= 1
                    )
                |> List.sort
    in
    div [ class "flex gap-1" ]
        (List.map (viewPageButton model.currentPage) pageNumbers)


viewPageButton : Int -> Int -> Html Msg
viewPageButton currentPage page =
    button
        [ class
            (if page == currentPage then
                "relative inline-flex items-center px-4 py-2 text-sm font-semibold text-white bg-purple-600 focus:z-20 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-purple-600"

             else
                "relative inline-flex items-center px-4 py-2 text-sm font-semibold text-gray-900 ring-1 ring-inset ring-gray-300 hover:bg-gray-50 focus:z-20 focus:outline-offset-0"
            )
        , onClick (ChangePage page)
        ]
        [ text (String.fromInt (page + 1)) ]


stopPropagation : String -> Attribute Msg
stopPropagation event =
    Html.Events.stopPropagationOn event (Decode.succeed ( NoOp, True ))
