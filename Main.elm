module Main exposing (..)

import Browser
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, a, audio, button, div, h1, h2, input, li, p, section, source, text, ul)
import Html.Attributes exposing (autofocus, class, controls, href, placeholder, src, value)
import Html.Events exposing (on, onClick, onInput)
import Http
import Json.Decode as Decode
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string)


-- Models


type alias Model =
    { feeds : List Feed
    , stories : List Story
    , currentFeed : Maybe Feed
    , currentStory : Maybe Story
    , url : Url
    , key : Key
    , currentTime : Float
    , message : String
    }


type alias Feed =
    { id : String
    , title : String
    , itunes_image : String
    , itunes_author : String
    }


type alias FullFeed =
    { feed : Feed
    , stories : List Story
    }


type alias FullStory =
    { feed : Feed
    , story : Story
    }


type alias Story =
    { id : String
    , title : String
    , enclosure_url : String
    }


type alias Flags =
    {}


type Page
    = Home
    | FeedShow String
    | StoryShow String



-- MESSAGE


type Msg
    = None
    | LoadFeeds (Result Http.Error (List Feed))
    | LoadFeed (Result Http.Error FullFeed)
    | LoadFullStory (Result Http.Error FullStory)
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | TimeUpdate Float
    | MessageChange String
    | SubmitMessage


currentPage : Url.Url -> Page
currentPage url =
    let
        parser =
            oneOf
                [ map FeedShow (s "f" </> string)
                , map StoryShow (s "s" </> string)
                ]
    in
    case Url.Parser.parse parser url of
        Just page ->
            page

        Nothing ->
            Home



-- INIT


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    case currentPage url of
        Home ->
            ( Model [] [] Nothing Nothing url key 0 "", fetchFeeds )

        FeedShow id ->
            ( Model [] [] Nothing Nothing url key 0 "", fetchFeed id )

        StoryShow id ->
            ( Model [] [] Nothing Nothing url key 0 "", fetchFullStory id )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case currentPage model.url of
        Home ->
            { title = "Reada"
            , body =
                [ div []
                    [ div [ class "container" ]
                        [ h1 [] [ text "Reada" ]
                        ]
                    ]
                , div [ class "container", class "flex" ] (viewFeeds model.feeds)
                ]
            }

        FeedShow id ->
            { title = "Reada | Feed"
            , body =
                [ div [ class "container" ]
                    ([ feedHeader model ] ++ List.map viewStory model.stories)
                ]
            }

        StoryShow id ->
            { title = "Reada | Story"
            , body =
                [ div [ class "container" ]
                    [ div [] [ feedHeader model ]
                    , viewFullStory model
                    ]
                ]
            }


feedHeader : Model -> Html Msg
feedHeader model =
    case model.currentFeed of
        Just feed ->
            div []
                [ div [] [ a [ href "/" ] [ text "Home" ] ]
                , Html.img [ src feed.itunes_image ] []
                , h1 [] [ a [ href ("/f/" ++ feed.id) ] [ text feed.title ] ]
                ]

        Nothing ->
            div [ class "container" ] [ text "unknown!" ]


viewFullStory : Model -> Html Msg
viewFullStory model =
    case model.currentStory of
        Just story ->
            div []
                [ div [ href ("/s/" ++ story.id), class "story-container" ]
                    [ div []
                        [ div [ class "story-title" ]
                            [ text story.title
                            , div [ class "input" ]
                                [ input [ placeholder "Message...", value model.message, onInput MessageChange ] []
                                , button [ onClick SubmitMessage ] [ text "Comment" ]
                                ]
                            ]
                        , div [ class "comments" ] (viewComments model.currentTime)
                        , audio
                            [ controls True, class "story-title", onTimeUpdate TimeUpdate ]
                            [ source [ src story.enclosure_url ] [] ]
                        ]
                    ]
                ]

        Nothing ->
            div [ class "container" ] [ text "unknown!" ]


viewComments : Float -> List (Html Msg)
viewComments currentTime =
    let
        visibleComments =
            List.reverse (List.filter (\comment -> comment.time < currentTime) fakeComments)
    in
    List.map viewComment visibleComments


viewComment : Comment -> Html Msg
viewComment comment =
    let
        timestamp =
            formatTime comment.time
    in
    div [ class "comment" ] [ text (timestamp ++ ": " ++ comment.message) ]


formatTime : Float -> String
formatTime seconds =
    let
        minutes =
            Basics.floor seconds // 60

        remainingSeconds =
            remainderBy (Basics.floor seconds) 60
    in
    String.fromInt minutes ++ ":" ++ String.fromFloat seconds


viewStory : Story -> Html Msg
viewStory story =
    a [ href ("/s/" ++ story.id), class "story-container" ]
        [ div [ class "subtext-container" ]
            [ div [ class "story-title" ] [ text story.title ] ]
        ]


viewFeeds : List Feed -> List (Html Msg)
viewFeeds feeds =
    List.map viewFeed feeds


viewFeed : Feed -> Html Msg
viewFeed feed =
    a [ href ("/f/" ++ feed.id), class "feed-container" ]
        [ Html.img [ src feed.itunes_image ] []
        , div [ class "subtext-container" ]
            [ div [ class "feed-title" ] [ text (String.left 20 feed.title) ]
            , div [ class "feed-author" ] [ text (String.left 20 feed.itunes_author) ]
            ]
        ]



-- HTTP


fetchFeeds : Cmd Msg
fetchFeeds =
    Http.send LoadFeeds getFeeds


getFeeds : Http.Request (List Feed)
getFeeds =
    Http.get "/feeds.json" decodeFeeds


decodeFeeds : Decode.Decoder (List Feed)
decodeFeeds =
    Decode.list
        decodeFeed


fetchFeed : String -> Cmd Msg
fetchFeed id =
    Http.send LoadFeed (getFeed id)


getFeed : String -> Http.Request FullFeed
getFeed id =
    Http.get ("/feeds/" ++ id ++ ".json") decodeFullFeed


decodeFullFeed : Decode.Decoder FullFeed
decodeFullFeed =
    Decode.map2 FullFeed
        (Decode.field "feed" decodeFeed)
        (Decode.field "stories" decodeStories)


decodeFeed : Decode.Decoder Feed
decodeFeed =
    Decode.map4 Feed
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "itunes_image" Decode.string)
        (Decode.field "itunes_author" Decode.string)


fetchFullStory : String -> Cmd Msg
fetchFullStory id =
    Http.send LoadFullStory (getFullStory id)


getFullStory : String -> Http.Request FullStory
getFullStory id =
    Http.get ("/stories/" ++ id ++ ".json") decodeFullStory


decodeFullStory : Decode.Decoder FullStory
decodeFullStory =
    Decode.map2 FullStory
        (Decode.field "feed" decodeFeed)
        (Decode.field "story" decodeStory)


decodeStories : Decode.Decoder (List Story)
decodeStories =
    Decode.list
        decodeStory


decodeStory : Decode.Decoder Story
decodeStory =
    Decode.map3 Story
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "enclosure_url" Decode.string)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        TimeUpdate time ->
            ( { model | currentTime = time }, Cmd.none )

        LoadFeeds response ->
            case response of
                Ok feeds ->
                    ( { model | feeds = feeds }, Cmd.none )

                Err err ->
                    let
                        useless =
                            Debug.log "error" err
                    in
                    Debug.log "could not load feeds" ( model, Cmd.none )

        LoadFeed response ->
            case response of
                Ok fullFeed ->
                    ( { model
                        | stories = fullFeed.stories
                        , currentFeed = Just fullFeed.feed
                      }
                    , Cmd.none
                    )

                Err err ->
                    let
                        useless =
                            Debug.log "error" err
                    in
                    Debug.log "could not load full feed" ( model, Cmd.none )

        LoadFullStory response ->
            case response of
                Ok fullStory ->
                    ( { model | currentFeed = Just fullStory.feed, currentStory = Just fullStory.story }, Cmd.none )

                Err err ->
                    let
                        useless =
                            Debug.log "error" err
                    in
                    Debug.log "could not load story" ( model, Cmd.none )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        OnUrlChange url ->
            case currentPage url of
                Home ->
                    ( { model | url = url }, fetchFeeds )

                FeedShow feedId ->
                    ( { model | url = url }, fetchFeed feedId )

                StoryShow storyId ->
                    ( { model | url = url }, fetchFullStory storyId )

        MessageChange messageString ->
            ( { model | message = messageString }, Cmd.none )

        SubmitMessage ->
            ( model, Cmd.none )

        None ->
            ( model, Cmd.none )



-- Custom event handler


onTimeUpdate : (Float -> msg) -> Html.Attribute msg
onTimeUpdate msg =
    on "timeupdate" (Decode.map msg targetCurrentTime)


targetCurrentTime : Decode.Decoder Float
targetCurrentTime =
    Decode.at [ "target", "currentTime" ] Decode.float



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
