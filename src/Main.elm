module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, pre)
import Html.Events exposing (onClick)
import Bytes.Encode as Encode exposing (encode)
import Bytes exposing (..)
import Bytes.Decode exposing (Decoder)
import Http
import Json.Decode as D


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


type alias Model =
    { status : String }


initialModel : Model
initialModel =
    { status = "Starting up" }


imageUrl =
    "https://natgeo.imgix.net/factsheets/thumbnails/01-frog-day-gallery.adapt.1900.1.jpg?auto=compress,format&w=1024&h=560&fit=crop"


type Msg
    = GetData
    | GotData (Result Http.Error Bytes)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetData ->
            ( model, getData imageUrl )

        GotData result ->
            case result of
                Ok data ->
                    ( { model
                        | status =
                            "Bytes received = " ++ (String.fromInt (Bytes.width data))
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | status = "Invalid data" }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick GetData ] [ text "Get image" ]
        , pre [] [ text <| "status: " ++ model.status ]
        ]


getData : String -> Cmd Msg
getData url =
    Http.get
        { url = url
        , expect = expectBytes GotData
        }


expectBytes : (Result Http.Error Bytes -> msg) -> Http.Expect msg
expectBytes toMsg =
    Http.expectBytesResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    Ok body
