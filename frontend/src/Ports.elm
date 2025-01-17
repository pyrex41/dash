port module Ports exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


port receiveApplications : (Decode.Value -> msg) -> Sub msg


port requestRefresh : { page : Int, pageSize : Int, searchTerm : String, hasContactFilter : Bool, naics : List String } -> Cmd msg


port exportToCsv : { searchTerm : String, hasContactFilter : Bool, hasCSGFilter : Bool } -> Cmd msg


port requestApplication : { id : String } -> Cmd msg


port receiveApplication : (Decode.Value -> msg) -> Sub msg


port verifyCSGApplication : ( String, String ) -> Cmd msg


port verificationReceived : (Decode.Value -> msg) -> Sub msg



-- Port for saving application data


port saveApplication : { id : String, data : Encode.Value, medications : Encode.Value } -> Cmd msg



-- Port for receiving save response


port saveApplicationResponse : ({ success : Bool, error : Maybe String } -> msg) -> Sub msg



-- Add at the top with other ports


port forceRefreshLAProToken : () -> Cmd msg


port getLAProTokenResponse : (String -> msg) -> Sub msg



-- Port for submitting to CSG


port submitToCSG : ( String, Int ) -> Cmd msg



-- Port for receiving CSG submission response


port submitToCSGResponse : ({ success : Bool, error : Maybe String, existingSubmission : Maybe Bool, key : Maybe String, verificationStatus : Maybe String } -> msg) -> Sub msg
