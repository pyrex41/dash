module AdminPage exposing (Model, Msg(..), SyncStats, SyncError, SyncStatus, ConnectionTestResult, ManualSyncResult, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Ports


-- MODEL


type alias SyncStats =
    { totalProcessed : Int
    , successCount : Int
    , failureCount : Int
    , errors : List SyncError
    }


type alias SyncError =
    { bookingId : String
    , error : String
    }


type alias SyncStatus =
    { isRunning : Bool
    , intervalMs : Int
    , lastSyncTime : Maybe String
    , nextSyncIn : Int
    , lastSyncStats : Maybe SyncStats
    }


type ConnectionStatus
    = NotTested
    | Testing
    | Connected String
    | Error String


type SyncTriggerStatus
    = Idle
    | Triggering
    | Triggered
    | TriggerFailed String


type alias Model =
    { syncStatus : Maybe SyncStatus
    , connectionStatus : ConnectionStatus
    , syncTriggerStatus : SyncTriggerStatus
    , loadingStatus : Bool
    }


init : Model
init =
    { syncStatus = Nothing
    , connectionStatus = NotTested
    , syncTriggerStatus = Idle
    , loadingStatus = False
    }



-- UPDATE


type Msg
    = RefreshSyncStatus
    | SyncStatusReceived (Result Decode.Error SyncStatus)
    | TestConnection
    | ConnectionTestReceived (Result Decode.Error ConnectionTestResult)
    | TriggerManualSync
    | ManualSyncReceived (Result Decode.Error ManualSyncResult)


type alias ConnectionTestResult =
    { success : Bool
    , message : String
    , configured : Bool
    , error : Maybe String
    }


type alias ManualSyncResult =
    { success : Bool
    , stats : Maybe SyncStats
    , error : Maybe String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RefreshSyncStatus ->
            ( { model | loadingStatus = True }
            , Ports.getSyncStatus ()
            )

        SyncStatusReceived (Ok status) ->
            ( { model | syncStatus = Just status, loadingStatus = False }
            , Cmd.none
            )

        SyncStatusReceived (Err _) ->
            ( { model | loadingStatus = False }
            , Cmd.none
            )

        TestConnection ->
            ( { model | connectionStatus = Testing }
            , Ports.testHubSpotConnection ()
            )

        ConnectionTestReceived (Ok result) ->
            if result.success then
                ( { model | connectionStatus = Connected result.message }
                , Cmd.none
                )

            else
                ( { model | connectionStatus = Error (result.error |> Maybe.withDefault result.message) }
                , Cmd.none
                )

        ConnectionTestReceived (Err _) ->
            ( { model | connectionStatus = Error "Failed to parse connection test result" }
            , Cmd.none
            )

        TriggerManualSync ->
            ( { model | syncTriggerStatus = Triggering }
            , Ports.triggerManualSync ()
            )

        ManualSyncReceived (Ok result) ->
            if result.success then
                ( { model | syncTriggerStatus = Triggered }
                , Ports.getSyncStatus ()
                )

            else
                ( { model | syncTriggerStatus = TriggerFailed (result.error |> Maybe.withDefault "Unknown error") }
                , Cmd.none
                )

        ManualSyncReceived (Err _) ->
            ( { model | syncTriggerStatus = TriggerFailed "Failed to parse sync result" }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-gray-50 py-8" ]
        [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
            [ viewHeader
            , div [ class "mt-8 grid grid-cols-1 gap-6 lg:grid-cols-2" ]
                [ viewSyncStatusCard model
                , viewConnectionCard model
                ]
            , viewManualActionsCard model
            ]
        ]


viewHeader : Html Msg
viewHeader =
    div [ class "mb-8" ]
        [ h1 [ class "text-3xl font-bold text-gray-900" ] [ text "Admin Settings" ]
        , p [ class "mt-2 text-sm text-gray-600" ]
            [ text "Manage HubSpot sync configuration and monitor system status" ]
        ]


viewSyncStatusCard : Model -> Html Msg
viewSyncStatusCard model =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ div [ class "flex justify-between items-center mb-4" ]
            [ h2 [ class "text-lg font-semibold text-gray-900" ] [ text "Sync Scheduler Status" ]
            , button
                [ onClick RefreshSyncStatus
                , disabled model.loadingStatus
                , class "px-3 py-1 text-sm bg-blue-600 text-white rounded hover:bg-blue-700 disabled:opacity-50"
                ]
                [ text (if model.loadingStatus then "Loading..." else "Refresh") ]
            ]
        , case model.syncStatus of
            Nothing ->
                p [ class "text-gray-500 text-sm" ] [ text "Loading status..." ]

            Just status ->
                div [ class "space-y-3" ]
                    [ viewStatusRow "Status" (if status.isRunning then "Running" else "Stopped") (if status.isRunning then "text-green-600" else "text-red-600")
                    , viewStatusRow "Sync Interval" (formatInterval status.intervalMs) "text-gray-700"
                    , viewStatusRow "Last Sync" (status.lastSyncTime |> Maybe.withDefault "Never") "text-gray-700"
                    , viewStatusRow "Next Sync In" (formatDuration status.nextSyncIn) "text-gray-700"
                    , case status.lastSyncStats of
                        Just stats ->
                            div [ class "mt-4 pt-4 border-t border-gray-200" ]
                                [ p [ class "text-sm font-medium text-gray-700 mb-2" ] [ text "Last Sync Results:" ]
                                , viewStatusRow "Total Processed" (String.fromInt stats.totalProcessed) "text-gray-700"
                                , viewStatusRow "Successful" (String.fromInt stats.successCount) "text-green-600"
                                , viewStatusRow "Failed" (String.fromInt stats.failureCount) (if stats.failureCount > 0 then "text-red-600" else "text-gray-700")
                                , if List.isEmpty stats.errors |> not then
                                    div [ class "mt-3" ]
                                        [ p [ class "text-sm font-medium text-red-600 mb-1" ] [ text "Errors:" ]
                                        , div [ class "space-y-1" ]
                                            (List.map viewSyncError stats.errors)
                                        ]

                                  else
                                    text ""
                                ]

                        Nothing ->
                            text ""
                    ]
        ]


viewConnectionCard : Model -> Html Msg
viewConnectionCard model =
    div [ class "bg-white shadow rounded-lg p-6" ]
        [ h2 [ class "text-lg font-semibold text-gray-900 mb-4" ] [ text "HubSpot Connection" ]
        , div [ class "space-y-4" ]
            [ button
                [ onClick TestConnection
                , disabled (model.connectionStatus == Testing)
                , class "w-full px-4 py-2 bg-purple-600 text-white rounded hover:bg-purple-700 disabled:opacity-50"
                ]
                [ text (if model.connectionStatus == Testing then "Testing..." else "Test Connection") ]
            , case model.connectionStatus of
                NotTested ->
                    p [ class "text-sm text-gray-500" ] [ text "Click the button above to test your HubSpot connection" ]

                Testing ->
                    p [ class "text-sm text-blue-600" ] [ text "Testing connection..." ]

                Connected msg ->
                    div [ class "p-3 bg-green-50 rounded border border-green-200" ]
                        [ p [ class "text-sm text-green-800 font-medium" ] [ text "✓ Connected" ]
                        , p [ class "text-sm text-green-700 mt-1" ] [ text msg ]
                        ]

                Error err ->
                    div [ class "p-3 bg-red-50 rounded border border-red-200" ]
                        [ p [ class "text-sm text-red-800 font-medium" ] [ text "✗ Connection Failed" ]
                        , p [ class "text-sm text-red-700 mt-1" ] [ text err ]
                        ]
            ]
        ]


viewManualActionsCard : Model -> Html Msg
viewManualActionsCard model =
    div [ class "mt-6 bg-white shadow rounded-lg p-6" ]
        [ h2 [ class "text-lg font-semibold text-gray-900 mb-4" ] [ text "Manual Actions" ]
        , div [ class "space-y-4" ]
            [ div []
                [ button
                    [ onClick TriggerManualSync
                    , disabled (model.syncTriggerStatus == Triggering)
                    , class "px-4 py-2 bg-green-600 text-white rounded hover:bg-green-700 disabled:opacity-50"
                    ]
                    [ text (if model.syncTriggerStatus == Triggering then "Triggering..." else "Trigger Manual Sync") ]
                , p [ class "text-sm text-gray-500 mt-2" ]
                    [ text "Manually trigger a sync cycle for all pending bookings" ]
                , case model.syncTriggerStatus of
                    Idle ->
                        text ""

                    Triggering ->
                        p [ class "text-sm text-blue-600 mt-2" ] [ text "Triggering sync cycle..." ]

                    Triggered ->
                        div [ class "mt-2 p-3 bg-green-50 rounded border border-green-200" ]
                            [ p [ class "text-sm text-green-800" ] [ text "✓ Sync triggered successfully" ] ]

                    TriggerFailed err ->
                        div [ class "mt-2 p-3 bg-red-50 rounded border border-red-200" ]
                            [ p [ class "text-sm text-red-800" ] [ text ("✗ Failed: " ++ err) ] ]
                ]
            ]
        ]


viewStatusRow : String -> String -> String -> Html Msg
viewStatusRow label value colorClass =
    div [ class "flex justify-between items-center text-sm" ]
        [ span [ class "text-gray-600" ] [ text label ]
        , span [ class colorClass ] [ text value ]
        ]


viewSyncError : SyncError -> Html Msg
viewSyncError error =
    div [ class "text-xs text-red-600 bg-red-50 p-2 rounded" ]
        [ span [ class "font-medium" ] [ text (error.bookingId ++ ": ") ]
        , text error.error
        ]


formatInterval : Int -> String
formatInterval ms =
    let
        seconds =
            ms // 1000

        minutes =
            seconds // 60
    in
    if minutes > 0 then
        String.fromInt minutes ++ " minutes"

    else
        String.fromInt seconds ++ " seconds"


formatDuration : Int -> String
formatDuration ms =
    let
        seconds =
            ms // 1000

        minutes =
            seconds // 60
    in
    if minutes > 0 then
        String.fromInt minutes ++ "m " ++ String.fromInt (modBy 60 seconds) ++ "s"

    else if seconds > 0 then
        String.fromInt seconds ++ "s"

    else
        "Now"
