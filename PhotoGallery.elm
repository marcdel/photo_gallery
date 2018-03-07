module PhotoGallery exposing (..)

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


type alias Model =
    { photos : List Photo
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
            img [ class "large", src (urlPrefix ++ "large/" ++ url) ] []


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
        SelectByUrl url ->
            ( { model | selectedUrl = Just url }, Cmd.none )

        SelectByIndex index ->
            let
                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url
            in
                ( { model | selectedUrl = newSelectedUrl }, Cmd.none )

        SurpriseMe ->
            let
                randomPhotoPicker =
                    Random.int 0 (List.length model.photos - 1)
            in
                ( model, Random.generate SelectByIndex randomPhotoPicker )

        SetSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        LoadPhotos (Ok photos) ->
            ( { model
                | photos = photos
                , selectedUrl = Maybe.map .url (List.head photos)
              }
            , Cmd.none
            )

        LoadPhotos (Err _) ->
            ( { model
                | loadingError = Just "Error! (Try turning it off and on again?)"
              }
            , Cmd.none
            )

        SetHue hue ->
            ( { model | hue = hue }, Cmd.none )

        SetRipple ripple ->
            ( { model | ripple = ripple }, Cmd.none )

        SetNoise noise ->
            ( { model | noise = noise }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, loadPhotosCmd )
        , view = viewOrError
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
