module Pages.Home_ exposing (Model, Msg, page)

import Html exposing (Html, div, text)
import Html.Keyed exposing (node)
import Html.Lazy exposing (lazy)
import Http
import Json.Decode as JsonDecoder exposing (Decoder, field)
import Page exposing (Page)
import Request exposing (Request)
import Shared
import View exposing (View)



-- TYPES


type alias CoinData =
    { id : String, symbol : String, price_usd : String, percent_change_24h : String }


type alias Data =
    { data : List CoinData }



-- MODEL


type Model
    = Failure
    | Loading
    | Success Data


init : ( Model, Cmd Msg )
init =
    ( Loading, getPrices )


page : Shared.Model -> Request -> Page.With Model Msg
page _ _ =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- UPDATE


type Msg
    = GotPrices (Result Http.Error Data)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotPrices result ->
            case result of
                Ok coinsData ->
                    ( Success coinsData, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


view : Model -> View Msg
view model =
    { title = "Homepage"
    , body =
        [ div []
            [ viewPricesBar model
            ]
        ]
    }


viewPricesBar : Model -> Html Msg
viewPricesBar model =
    case model of
        Failure ->
            text "Failed."

        Loading ->
            text "Loading..."

        Success data ->
            div [] [ lazy viewData data ]


viewData : Data -> Html Msg
viewData data =
    node "div" [] (List.map viewRowItem data.data)


viewRowItem : CoinData -> ( String, Html Msg )
viewRowItem coinData =
    ( coinData.id ++ ". " ++ coinData.symbol, div [] [ text (coinData.id ++ ". " ++ coinData.symbol ++ ": $" ++ coinData.price_usd) ] )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- HELPERS


getPrices : Cmd Msg
getPrices =
    Http.get { url = "https://api.coinlore.net/api/tickers/", expect = Http.expectJson GotPrices dataDecoder }


dataDecoder : Decoder Data
dataDecoder =
    JsonDecoder.map Data (field "data" priceListDecoder)


priceListDecoder : Decoder (List CoinData)
priceListDecoder =
    JsonDecoder.list priceDecoder


priceDecoder : Decoder CoinData
priceDecoder =
    JsonDecoder.map4 CoinData (field "id" JsonDecoder.string) (field "symbol" JsonDecoder.string) (field "price_usd" JsonDecoder.string) (field "percent_change_24h" JsonDecoder.string)
