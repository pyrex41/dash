module Main exposing (main)

import ApplicationPage
import Browser
import Browser.Navigation as Nav
import Dashboard
import Html exposing (..)
import Url
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = DashboardRoute
    | ApplicationRoute String
    | NotFound


type alias Model =
    { key : Nav.Key
    , route : Route
    , page : Page
    }


type Page
    = DashboardPage Dashboard.Model
    | ApplicationPage ApplicationPage.Model


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | DashboardMsg Dashboard.Msg
    | ApplicationMsg ApplicationPage.Msg


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map DashboardRoute Parser.top
        , Parser.map ApplicationRoute (Parser.s "application" </> Parser.string)
        ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Maybe.withDefault NotFound (Parser.parse routeParser url)

        ( dashboardModel, dashboardCmd ) =
            Dashboard.init ()
    in
    initCurrentPage { key = key, route = route, page = DashboardPage dashboardModel }


initCurrentPage : Model -> ( Model, Cmd Msg )
initCurrentPage model =
    case model.route of
        DashboardRoute ->
            let
                ( dashboardModel, dashboardCmd ) =
                    Dashboard.init ()
            in
            ( { model | page = DashboardPage dashboardModel }
            , Cmd.map DashboardMsg dashboardCmd
            )

        ApplicationRoute id ->
            let
                ( pageModel, pageCmd ) =
                    ApplicationPage.init id
            in
            ( { model | page = ApplicationPage pageModel }
            , Cmd.map ApplicationMsg pageCmd
            )

        NotFound ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Maybe.withDefault NotFound (Parser.parse routeParser url)
            in
            initCurrentPage { model | route = newRoute }

        ( DashboardMsg subMsg, DashboardPage pageModel ) ->
            let
                ( newPageModel, pageCmd ) =
                    Dashboard.update subMsg pageModel
            in
            ( { model | page = DashboardPage newPageModel }
            , Cmd.map DashboardMsg pageCmd
            )

        ( ApplicationMsg subMsg, ApplicationPage pageModel ) ->
            let
                ( newPageModel, pageCmd ) =
                    ApplicationPage.update subMsg pageModel
            in
            ( { model | page = ApplicationPage newPageModel }
            , Cmd.map ApplicationMsg pageCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model.page of
        DashboardPage dashboardModel ->
            { title = "Dashboard"
            , body = [ Html.map DashboardMsg (Dashboard.view dashboardModel) ]
            }

        ApplicationPage applicationModel ->
            { title = "Application Details"
            , body = [ Html.map ApplicationMsg (ApplicationPage.view applicationModel) ]
            }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        DashboardPage dashboardModel ->
            Sub.map DashboardMsg (Dashboard.subscriptions dashboardModel)

        ApplicationPage applicationModel ->
            Sub.none



--Sub.map ApplicationMsg (ApplicationPage.subscriptions applicationModel)
-- Add other necessary functions (update, subscriptions, etc.)
