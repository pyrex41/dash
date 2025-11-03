module Main exposing (main)

import AdminPage
import ApplicationPage
import Browser
import Browser.Navigation as Nav
import CSGApplicationView
import CSGApplicationsPage
import Dashboard
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Producer exposing (producerConfigDecoder)
import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf)
import Ports



-- ADMIN DECODERS


syncErrorDecoder : Decode.Decoder AdminPage.SyncError
syncErrorDecoder =
    Decode.map2 AdminPage.SyncError
        (Decode.field "bookingId" Decode.string)
        (Decode.field "error" Decode.string)


syncStatsDecoder : Decode.Decoder AdminPage.SyncStats
syncStatsDecoder =
    Decode.map4 AdminPage.SyncStats
        (Decode.field "totalProcessed" Decode.int)
        (Decode.field "successCount" Decode.int)
        (Decode.field "failureCount" Decode.int)
        (Decode.field "errors" (Decode.list syncErrorDecoder))


syncStatusDecoder : Decode.Decoder AdminPage.SyncStatus
syncStatusDecoder =
    Decode.map5 AdminPage.SyncStatus
        (Decode.field "isRunning" Decode.bool)
        (Decode.field "intervalMs" Decode.int)
        (Decode.maybe (Decode.field "lastSyncTime" Decode.string))
        (Decode.field "nextSyncIn" Decode.int)
        (Decode.maybe (Decode.field "lastSyncStats" syncStatsDecoder))


connectionTestDecoder : Decode.Decoder AdminPage.ConnectionTestResult
connectionTestDecoder =
    Decode.map4 AdminPage.ConnectionTestResult
        (Decode.field "success" Decode.bool)
        (Decode.field "message" Decode.string)
        (Decode.field "configured" Decode.bool)
        (Decode.maybe (Decode.field "error" Decode.string))


manualSyncResultDecoder : Decode.Decoder AdminPage.ManualSyncResult
manualSyncResultDecoder =
    Decode.map3 AdminPage.ManualSyncResult
        (Decode.field "success" Decode.bool)
        (Decode.maybe (Decode.field "stats" syncStatsDecoder))
        (Decode.maybe (Decode.field "error" Decode.string))



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    , flags : Decode.Value
    , authenticated : Bool
    , loginUsername : String
    , loginPassword : String
    , loginError : Maybe String
    , loggingIn : Bool
    }


type Page
    = NotFound
    | DashboardPage Dashboard.Model
    | CSGApplicationPage CSGApplicationView.Model
    | CSGApplicationsPage CSGApplicationsPage.Model
    | ApplicationPage ApplicationPage.Model
    | AdminPage AdminPage.Model



-- INIT


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { key = key
            , url = url
            , page = NotFound
            , flags = flags
            , authenticated = False
            , loginUsername = ""
            , loginPassword = ""
            , loginError = Nothing
            , loggingIn = False
            }
    in
    ( model
    , Http.get
        { url = "/api/session"
        , expect =
            Http.expectJson SessionCheckResponse
                (Decode.field "authenticated" Decode.bool)
        }
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | DashboardMsg Dashboard.Msg
    | CSGApplicationMsg CSGApplicationView.Msg
    | CSGApplicationsMsg CSGApplicationsPage.Msg
    | ApplicationMsg ApplicationPage.Msg
    | AdminMsg AdminPage.Msg
    | UpdateLoginUsername String
    | UpdateLoginPassword String
    | SubmitLogin
    | LoginResponse (Result Http.Error Bool)
    | SessionCheckResponse (Result Http.Error Bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            routeUrl url model

        ( DashboardMsg subMsg, DashboardPage subModel ) ->
            let
                ( newSubModel, subCmd ) =
                    Dashboard.update subMsg subModel
            in
            ( { model | page = DashboardPage newSubModel }
            , Cmd.map DashboardMsg subCmd
            )

        ( CSGApplicationMsg subMsg, CSGApplicationPage subModel ) ->
            let
                ( newSubModel, subCmd ) =
                    CSGApplicationView.update subMsg subModel
            in
            ( { model | page = CSGApplicationPage newSubModel }
            , Cmd.map CSGApplicationMsg subCmd
            )

        ( CSGApplicationsMsg subMsg, CSGApplicationsPage subModel ) ->
            let
                ( newSubModel, subCmd ) =
                    CSGApplicationsPage.update subMsg subModel
            in
            ( { model | page = CSGApplicationsPage newSubModel }
            , Cmd.map CSGApplicationsMsg subCmd
            )

        ( ApplicationMsg subMsg, ApplicationPage subModel ) ->
            let
                ( newSubModel, subCmd ) =
                    ApplicationPage.update subMsg subModel
            in
            ( { model | page = ApplicationPage newSubModel }
            , Cmd.map ApplicationMsg subCmd
            )

        ( AdminMsg subMsg, AdminPage subModel ) ->
            let
                ( newSubModel, subCmd ) =
                    AdminPage.update subMsg subModel
            in
            ( { model | page = AdminPage newSubModel }
            , Cmd.map AdminMsg subCmd
            )

        ( UpdateLoginUsername username, _ ) ->
            ( { model | loginUsername = username, loginError = Nothing }, Cmd.none )

        ( UpdateLoginPassword password, _ ) ->
            ( { model | loginPassword = password, loginError = Nothing }, Cmd.none )

        ( SubmitLogin, _ ) ->
            ( { model | loggingIn = True, loginError = Nothing }
            , Http.post
                { url = "/api/login"
                , body =
                    Http.jsonBody
                        (Encode.object
                            [ ( "username", Encode.string model.loginUsername )
                            , ( "password", Encode.string model.loginPassword )
                            ]
                        )
                , expect =
                    Http.expectJson LoginResponse
                        (Decode.field "success" Decode.bool)
                }
            )

        ( LoginResponse result, _ ) ->
            case result of
                Ok True ->
                    routeUrl model.url { model | authenticated = True, loginError = Nothing, loggingIn = False }

                Ok False ->
                    ( { model | loginError = Just "Invalid username or password", loginPassword = "", loggingIn = False }, Cmd.none )

                Err _ ->
                    ( { model | loginError = Just "Connection error. Please try again.", loginPassword = "", loggingIn = False }, Cmd.none )

        ( SessionCheckResponse result, _ ) ->
            case result of
                Ok True ->
                    -- Session is valid, route to the URL
                    routeUrl model.url { model | authenticated = True }

                Ok False ->
                    -- No valid session, stay on login page
                    ( model, Cmd.none )

                Err _ ->
                    -- Error checking session, stay on login page
                    ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        DashboardPage subModel ->
            Sub.map DashboardMsg (Dashboard.subscriptions subModel)

        CSGApplicationPage _ ->
            Sub.none

        CSGApplicationsPage subModel ->
            Sub.map CSGApplicationsMsg (CSGApplicationsPage.subscriptions subModel)

        ApplicationPage subModel ->
            Sub.map ApplicationMsg (ApplicationPage.subscriptions subModel)

        AdminPage _ ->
            Sub.batch
                [ Ports.receiveSyncStatus (\value -> AdminMsg (AdminPage.SyncStatusReceived (Decode.decodeValue syncStatusDecoder value)))
                , Ports.receiveConnectionTest (\value -> AdminMsg (AdminPage.ConnectionTestReceived (Decode.decodeValue connectionTestDecoder value)))
                , Ports.receiveManualSyncResult (\value -> AdminMsg (AdminPage.ManualSyncReceived (Decode.decodeValue manualSyncResultDecoder value)))
                ]

        NotFound ->
            Sub.none



-- ROUTING


type Route
    = DashboardRoute
    | CSGApplicationRoute
    | CSGApplicationsRoute
    | ApplicationRoute String
    | AdminRoute


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map DashboardRoute Parser.top
        , Parser.map DashboardRoute (Parser.s "dashboard")
        , Parser.map CSGApplicationRoute (Parser.s "csg-application")
        , Parser.map CSGApplicationsRoute (Parser.s "csg-applications")
        , Parser.map ApplicationRoute (Parser.s "application" </> Parser.string)
        , Parser.map AdminRoute (Parser.s "admin")
        ]


routeUrl : Url.Url -> Model -> ( Model, Cmd Msg )
routeUrl url model =
    let
        parsedRoute =
            Parser.parse routeParser url

        cleanupCmd =
            case model.page of
                ApplicationPage subModel ->
                    Cmd.map ApplicationMsg (ApplicationPage.update ApplicationPage.Cleanup subModel |> Tuple.second)

                _ ->
                    Cmd.none
    in
    case parsedRoute of
        Just DashboardRoute ->
            let
                ( pageModel, pageCmd ) =
                    Dashboard.init model.flags
            in
            ( { model | url = url, page = DashboardPage pageModel }
            , Cmd.batch
                [ Cmd.map DashboardMsg pageCmd
                , cleanupCmd
                ]
            )

        Just CSGApplicationRoute ->
            let
                ( pageModel, pageCmd ) =
                    CSGApplicationView.init ()
            in
            ( { model | url = url, page = CSGApplicationPage pageModel }
            , Cmd.batch
                [ Cmd.map CSGApplicationMsg pageCmd
                , cleanupCmd
                ]
            )

        Just CSGApplicationsRoute ->
            let
                ( pageModel, pageCmd ) =
                    CSGApplicationsPage.init ()
            in
            ( { model | url = url, page = CSGApplicationsPage pageModel }
            , Cmd.batch
                [ Cmd.map CSGApplicationsMsg pageCmd
                , cleanupCmd
                ]
            )

        Just (ApplicationRoute id) ->
            let
                ( pageModel, pageCmd ) =
                    ApplicationPage.init id model.flags
            in
            ( { model | url = url, page = ApplicationPage pageModel }
            , Cmd.batch
                [ Cmd.map ApplicationMsg pageCmd
                , cleanupCmd
                ]
            )

        Just AdminRoute ->
            let
                pageModel =
                    AdminPage.init
            in
            ( { model | url = url, page = AdminPage pageModel }
            , Cmd.batch
                [ Cmd.map AdminMsg (Tuple.second (AdminPage.update AdminPage.RefreshSyncStatus pageModel))
                , cleanupCmd
                ]
            )

        Nothing ->
            ( { model | url = url, page = NotFound }
            , cleanupCmd
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Dashboard"
    , body =
        [ if not model.authenticated then
            viewLogin model

          else
            case model.page of
                NotFound ->
                    viewNotFound

                DashboardPage subModel ->
                    Html.map DashboardMsg (Dashboard.view subModel)

                CSGApplicationPage subModel ->
                    Html.map CSGApplicationMsg (CSGApplicationView.view subModel)

                CSGApplicationsPage subModel ->
                    Html.map CSGApplicationsMsg (CSGApplicationsPage.view subModel)

                ApplicationPage subModel ->
                    Html.map ApplicationMsg (ApplicationPage.view subModel)

                AdminPage subModel ->
                    Html.map AdminMsg (AdminPage.view subModel)
        ]
    }


viewLogin : Model -> Html Msg
viewLogin model =
    div [ class "min-h-screen flex items-center justify-center bg-gray-50" ]
        [ div [ class "max-w-md w-full space-y-8 p-8 bg-white rounded-lg shadow-md" ]
            [ div [ class "text-center" ]
                [ h2 [ class "text-3xl font-bold text-gray-900 mb-2" ]
                    [ text "Dashboard Login" ]
                , p [ class "text-gray-600" ]
                    [ text "Enter your credentials to continue" ]
                ]
            , Html.form
                [ class "mt-8 space-y-6"
                , onSubmit SubmitLogin
                ]
                [ div [ class "space-y-4" ]
                    [ div []
                        [ label
                            [ for "username"
                            , class "block text-sm font-medium text-gray-700 mb-1"
                            ]
                            [ text "Username" ]
                        , input
                            [ type_ "text"
                            , id "username"
                            , class "appearance-none rounded-md relative block w-full px-3 py-2 border border-gray-300 placeholder-gray-500 text-gray-900 focus:outline-none focus:ring-blue-500 focus:border-blue-500 focus:z-10 sm:text-sm"
                            , placeholder "Enter username"
                            , value model.loginUsername
                            , onInput UpdateLoginUsername
                            ]
                            []
                        ]
                    , div []
                        [ label
                            [ for "password"
                            , class "block text-sm font-medium text-gray-700 mb-1"
                            ]
                            [ text "Password" ]
                        , input
                            [ type_ "password"
                            , id "password"
                            , class "appearance-none rounded-md relative block w-full px-3 py-2 border border-gray-300 placeholder-gray-500 text-gray-900 focus:outline-none focus:ring-blue-500 focus:border-blue-500 focus:z-10 sm:text-sm"
                            , placeholder "Enter password"
                            , value model.loginPassword
                            , onInput UpdateLoginPassword
                            ]
                            []
                        ]
                    ]
                , case model.loginError of
                    Just error ->
                        div [ class "rounded-md bg-red-50 p-4" ]
                            [ div [ class "flex" ]
                                [ div [ class "ml-3" ]
                                    [ p [ class "text-sm font-medium text-red-800" ]
                                        [ text error ]
                                    ]
                                ]
                            ]

                    Nothing ->
                        text ""
                , div []
                    [ button
                        [ type_ "submit"
                        , class "group relative w-full flex justify-center py-2 px-4 border border-transparent text-sm font-medium rounded-md text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 disabled:opacity-50 disabled:cursor-not-allowed"
                        , onClick SubmitLogin
                        , disabled model.loggingIn
                        ]
                        [ text
                            (if model.loggingIn then
                                "Signing in..."

                             else
                                "Sign in"
                            )
                        ]
                    ]
                ]
            ]
        ]


viewNotFound : Html msg
viewNotFound =
    div [ class "min-h-screen flex items-center justify-center" ]
        [ div [ class "text-center" ]
            [ h1 [ class "text-4xl font-bold text-gray-900 mb-4" ]
                [ text "404" ]
            , p [ class "text-gray-600" ]
                [ text "Page not found" ]
            ]
        ]
