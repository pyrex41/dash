port module Ports exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode



-- Application Data Ports


port receiveApplications : (Decode.Value -> msg) -> Sub msg


port requestRefresh : { page : Int, pageSize : Int, searchTerm : String, hasContactFilter : Bool, naics : List String } -> Cmd msg



-- Single Application Management


port requestApplication : { id : String } -> Cmd msg


port receiveApplication : (Decode.Value -> msg) -> Sub msg


port saveApplication : { id : String, data : Encode.Value, medications : Encode.Value } -> Cmd msg


port saveApplicationResponse : ({ success : Bool, error : Maybe String } -> msg) -> Sub msg


port statusUpdate : ({ id : String, status : String } -> msg) -> Sub msg



-- WebSocket Ports


port wsSubscribe : List String -> Cmd msg


port wsUnsubscribe : List String -> Cmd msg


port wsSubscribed : (List String -> msg) -> Sub msg


port wsUnsubscribed : (List String -> msg) -> Sub msg


port wsError : (String -> msg) -> Sub msg



-- CSG Integration Ports


port submitToCSG : ( String, Int ) -> Cmd msg


port submitToCSGResponse : ({ success : Bool, error : Maybe String, existingSubmission : Maybe Bool, key : Maybe String, verificationStatus : Maybe String } -> msg) -> Sub msg



-- Token Management


port forceRefreshLAProToken : () -> Cmd msg


port getLAProTokenResponse : (String -> msg) -> Sub msg



-- Export


port exportToCsv : { searchTerm : String, hasContactFilter : Bool, hasCSGFilter : Bool } -> Cmd msg
