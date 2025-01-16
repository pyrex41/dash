module Main exposing (main)

import ApplicationPage
import Browser
import Browser.Navigation as Nav
import CSGApplicationView
import CSGApplicationsPage
import Dashboard
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Producer exposing (producerConfigDecoder)
import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf)



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
    }


type Page
    = NotFound
    | DashboardPage Dashboard.Model
    | CSGApplicationPage CSGApplicationView.Model
    | CSGApplicationsPage CSGApplicationsPage.Model
    | ApplicationPage ApplicationPage.Model



-- INIT


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { key = key
            , url = url
            , page = NotFound
            , flags = flags
            }
    in
    routeUrl url model



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | DashboardMsg Dashboard.Msg
    | CSGApplicationMsg CSGApplicationView.Msg
    | CSGApplicationsMsg CSGApplicationsPage.Msg
    | ApplicationMsg ApplicationPage.Msg


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

        NotFound ->
            Sub.none



-- ROUTING


type Route
    = DashboardRoute
    | CSGApplicationRoute
    | CSGApplicationsRoute
    | ApplicationRoute String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map DashboardRoute Parser.top
        , Parser.map DashboardRoute (Parser.s "dashboard")
        , Parser.map CSGApplicationRoute (Parser.s "csg-application")
        , Parser.map CSGApplicationsRoute (Parser.s "csg-applications")
        , Parser.map ApplicationRoute (Parser.s "application" </> Parser.string)
        ]


routeUrl : Url.Url -> Model -> ( Model, Cmd Msg )
routeUrl url model =
    let
        parsedRoute =
            Parser.parse routeParser url
    in
    case parsedRoute of
        Just DashboardRoute ->
            let
                ( pageModel, pageCmd ) =
                    Dashboard.init model.flags
            in
            ( { model | url = url, page = DashboardPage pageModel }
            , Cmd.map DashboardMsg pageCmd
            )

        Just CSGApplicationRoute ->
            let
                ( pageModel, pageCmd ) =
                    CSGApplicationView.init ()
            in
            ( { model | url = url, page = CSGApplicationPage pageModel }
            , Cmd.map CSGApplicationMsg pageCmd
            )

        Just CSGApplicationsRoute ->
            let
                ( pageModel, pageCmd ) =
                    CSGApplicationsPage.init ()
            in
            ( { model | url = url, page = CSGApplicationsPage pageModel }
            , Cmd.map CSGApplicationsMsg pageCmd
            )

        Just (ApplicationRoute id) ->
            let
                ( pageModel, pageCmd ) =
                    ApplicationPage.init id model.flags
            in
            ( { model | url = url, page = ApplicationPage pageModel }
            , Cmd.map ApplicationMsg pageCmd
            )

        Nothing ->
            ( { model | url = url, page = NotFound }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Dashboard"
    , body =
        [ case model.page of
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
        ]
    }


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
