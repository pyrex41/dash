module BookingDecoder exposing (BookingRow, BookingsResponse, HubSpotSyncStatus(..), bookingRowDecoder, bookingsResponseDecoder, hubSpotSyncStatusDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type alias BookingRow =
    { id : String
    , email : String
    , phone : Maybe String
    , status : String
    , hubspotContactId : Maybe String
    , hubspotSyncStatus : HubSpotSyncStatus
    , hubspotLastSyncedAt : Maybe Int
    , hubspotSyncError : Maybe String
    , createdAt : String
    , application : Maybe BookingApplication
    }


type alias BookingApplication =
    { id : String
    , name : Maybe String
    }


type HubSpotSyncStatus
    = Pending
    | Syncing
    | Synced
    | Failed


type alias BookingsResponse =
    { bookings : List BookingRow
    , totalCount : Int
    , page : Int
    , pageSize : Int
    , totalPages : Int
    }


hubSpotSyncStatusDecoder : Decoder HubSpotSyncStatus
hubSpotSyncStatusDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "pending" ->
                        Decode.succeed Pending

                    "syncing" ->
                        Decode.succeed Syncing

                    "synced" ->
                        Decode.succeed Synced

                    "failed" ->
                        Decode.succeed Failed

                    _ ->
                        Decode.succeed Pending
            )


bookingApplicationDecoder : Decoder BookingApplication
bookingApplicationDecoder =
    Decode.succeed BookingApplication
        |> Pipeline.required "id" Decode.string
        |> Pipeline.optional "name" (Decode.maybe Decode.string) Nothing


bookingRowDecoder : Decoder BookingRow
bookingRowDecoder =
    Decode.succeed BookingRow
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.optional "phone" (Decode.maybe Decode.string) Nothing
        |> Pipeline.required "status" Decode.string
        |> Pipeline.optional "hubspotContactId" (Decode.maybe Decode.string) Nothing
        |> Pipeline.optional "hubspotSyncStatus" hubSpotSyncStatusDecoder Pending
        |> Pipeline.optional "hubspotLastSyncedAt" (Decode.maybe Decode.int) Nothing
        |> Pipeline.optional "hubspotSyncError" (Decode.maybe Decode.string) Nothing
        |> Pipeline.required "createdAt" Decode.string
        |> Pipeline.optional "application" (Decode.maybe bookingApplicationDecoder) Nothing


bookingsResponseDecoder : Decoder BookingsResponse
bookingsResponseDecoder =
    Decode.succeed BookingsResponse
        |> Pipeline.required "bookings" (Decode.list bookingRowDecoder)
        |> Pipeline.required "totalCount" Decode.int
        |> Pipeline.required "page" Decode.int
        |> Pipeline.required "pageSize" Decode.int
        |> Pipeline.required "totalPages" Decode.int
