module PhotoGalleryTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, attribute)
import PhotoGallery exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (untitled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue photoDecoder
                |> Result.map .title
                |> Expect.equal (Ok "(untitled)")


stateTransitions : Test
stateTransitions =
    describe "state transitions"
        [ fuzz string "SelectByUrl selects the given photo by URL" <|
            \url ->
                PhotoGallery.model
                    |> PhotoGallery.update (SelectByUrl url)
                    |> Tuple.first
                    |> .selectedUrl
                    |> Expect.equal (Just url)
        , fuzz (list string) "LoadPhotos selects the first photo" <|
            \urls ->
                let
                    photos =
                        List.map photoFromUrl urls
                in
                    PhotoGallery.model
                        |> PhotoGallery.update (LoadPhotos (Ok photos))
                        |> Tuple.first
                        |> .selectedUrl
                        |> Expect.equal (List.head urls)
        ]


doesNotRenderThumbnailsWhenListIsEmpty : Test
doesNotRenderThumbnailsWhenListIsEmpty =
    test "No thumbnails render when there are no photos to render." <|
        \_ ->
            PhotoGallery.model
                |> PhotoGallery.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)


rendersEachThumbnailInTheList : Test
rendersEachThumbnailInTheList =
    fuzz urlFuzzer "URLs render as thumbnails" <|
        \urls ->
            let
                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
                { model | photos = List.map photoFromUrl urls }
                    |> PhotoGallery.view
                    |> Query.fromHtml
                    |> Expect.all thumbnailChecks


clickThumbnail : Test
clickThumbnail =
    fuzz3 urlFuzzer string urlFuzzer "clicking a thumbnail selects it" <|
        \urlsBefore urlToClick urlsAfter ->
            let
                url =
                    urlToClick ++ ".jpeg"

                photos =
                    (urlsBefore ++ url :: urlsAfter)
                        |> List.map photoFromUrl

                srcToClick =
                    urlPrefix ++ url
            in
                { model | photos = photos }
                    |> PhotoGallery.view
                    |> Query.fromHtml
                    |> Query.find [ tag "img", attribute <| Attr.src (srcToClick) ]
                    |> Event.simulate Event.click
                    |> Event.expect (SelectByUrl url)


urlFuzzer : Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount


urlsFromCount : Int -> List String
urlsFromCount urlCount =
    List.range 1 urlCount
        |> List.map (\num -> toString num ++ ".png")


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll [ tag "img", attribute <| Attr.src (urlPrefix ++ url) ]
        |> Query.count (Expect.atLeast 1)


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }
