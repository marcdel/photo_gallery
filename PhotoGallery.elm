port module PhotoGallery exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes as Attr exposing (id, checked, class, classList, src, max, name, type_, title)
import Html.Events exposing (onClick, on)
import Http
import Json.Decode exposing (string, int, list, Decoder, at)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Random


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


port setFilters : FilterOptions -> Cmd msg


port statusChanges : (String -> msg) -> Sub msg


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


type alias Model =
    { photos : List Photo
    , status : String
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


type Msg
    = SelectByUrl String
    | SelectByIndex Int
    | SetStatus String
    | SurpriseMe
    | SetSize ThumbnailSize
    | LoadPhotos (Result Http.Error (List Photo))
    | SetHue Int
    | SetRipple Int
    | SetNoise Int


type ThumbnailSize
    = Small
    | Medium
    | Large


model : Model
model =
    { photos = []
    , status = ""
    , selectedUrl = Nothing
    , loadingError = Nothing
    , chosenSize = Medium
    , hue = 0
    , ripple = 0
    , noise = 0
    }


photoDecoder : Decoder Photo
photoDecoder =
    decode Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just errorMessage ->
            div [ class "error-message" ]
                [ h1 [] [ text "Photo Gallery" ]
                , p [] [ text errorMessage ]
                ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Gallery" ]
        , button
            [ onClick SurpriseMe ]
            [ text "Surprise Me!" ]
        , div [ class "status" ] [ text model.status ]
        , div [ class "filters" ]
            [ viewFilter "Hue" SetHue model.hue
            , viewFilter "Ripple" SetRipple model.ripple
            , viewFilter "Noise" SetNoise model.noise
            ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , viewLarge model.selectedUrl
        ]


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , title (thumbnail.title ++ " [" ++ toString thumbnail.size ++ " Kb]")
        , classList [ ( "selected", selectedUrl == Just thumbnail.url ) ]
        , onClick (SelectByUrl thumbnail.url)
        ]
        []


viewLarge : Maybe String -> Html Msg
viewLarge maybeUrl =
    case maybeUrl of
        Nothing ->
            text ""

        Just url ->
            canvas [ id "main-canvas", class "large" ] []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input
            [ type_ "radio", name "size", checked (model.chosenSize == size), onClick (SetSize size) ]
            []
        , text (sizeToString size)
        ]


viewFilter : String -> (Int -> Msg) -> Int -> Html Msg
viewFilter name toMsg magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , paperSlider [ Attr.max "11", onImmediateValueChange toMsg ] []
        , label [] [ text (toString magnitude) ]
        ]


paperSlider : List (Attribute msg) -> List (Html msg) -> Html msg
paperSlider =
    node "paper-slider"


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


loadPhotosCmd : Cmd Msg
loadPhotosCmd =
    list photoDecoder
        |> Http.get (urlPrefix ++ "photos/list.json")
        |> Http.send LoadPhotos


onImmediateValueChange : (Int -> msg) -> Attribute msg
onImmediateValueChange toMsg =
    at [ "target", "immediateValue" ] int
        |> Json.Decode.map toMsg
        |> on "immediate-value-changed"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetStatus status ->
            ( { model | status = status }, Cmd.none )

        SelectByUrl selectedUrl ->
            applyFilters { model | selectedUrl = Just selectedUrl }

        SelectByIndex index ->
            let
                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url
            in
                applyFilters { model | selectedUrl = newSelectedUrl }

        SurpriseMe ->
            let
                randomPhotoPicker =
                    Random.int 0 (List.length model.photos - 1)
            in
                ( model, Random.generate SelectByIndex randomPhotoPicker )

        SetSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        LoadPhotos (Ok photos) ->
            applyFilters
                { model
                    | photos = photos
                    , selectedUrl = Maybe.map .url (List.head photos)
                }

        LoadPhotos (Err _) ->
            ( { model
                | loadingError = Just "Error! (Try turning it off and on again?)"
              }
            , Cmd.none
            )

        SetHue hue ->
            applyFilters { model | hue = hue }

        SetRipple ripple ->
            applyFilters { model | ripple = ripple }

        SetNoise noise ->
            applyFilters { model | noise = noise }


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.selectedUrl of
        Just selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
                ( model, setFilters { url = url, filters = filters } )

        Nothing ->
            ( model, Cmd.none )


init : Float -> ( Model, Cmd Msg )
init flags =
    let
        status =
            "Initializing Pasta v" ++ toString flags
    in
        ( { model | status = status }, loadPhotosCmd )


main : Program Float Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = viewOrError
        , update = update
        , subscriptions = (\_ -> statusChanges SetStatus)
        }
