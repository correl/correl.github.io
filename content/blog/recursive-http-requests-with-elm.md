+++
title = "Recursive HTTP Requests with Elm"
author = ["Correl Roush"]
date = 2018-01-22T00:00:00-05:00
keywords = ["emacs", "org-mode", "themes"]
tags = ["programming", "elm"]
draft = false
+++

So I got the idea in my head that I wanted to pull data from the
GitLab / GitHub APIs in my Elm app. This seemed straightforward
enough; just wire up an HTTP request and a JSON decoder, and off I go.
Then I remember, oh crap... like any sensible API with a potentially
huge amount of data behind it, the results come back _paginated_. For
anyone unfamiliar, this means that a single API request for a list of,
say, repositories, is only going to return up to some maximum number
of results. If there are more results available, there will be a
reference to additional _pages_ of results, that you can then fetch
with _another_ API request. My single request decoding only the
results returned _from_ that single request wasn't going to cut it.

I had a handful of problems to solve. I needed to:

-   Detect when additional results were available.
-   Parse out the URL to use to fetch the next page of results.
-   Continue fetching results until none remained.
-   Combine all of the results, maintaining their order.


## Are there more results? {#are-there-more-results}

The first two bullet points can be dealt with by parsing and
inspecting the response header. Both GitHub and GitLab embed
pagination links in the [HTTP Link header](https://www.w3.org/wiki/LinkHeader). As I'm interested in
consuming pages until no further results remain, I'll be looking for a
link in the header with the relationship "next". If I find one, I know
I need to hit the associated URL to fetch more results. If I don't
find one, I'm done!

```http
Link: <https://api.github.com/user/repos?page=3&per_page=100>; rel="next",
  <https://api.github.com/user/repos?page=50&per_page=100>; rel="last"
```

<div class="src-block-caption">
  <span class="src-block-number">Code Snippet 1</span>:
  Example GitHub Link header
</div>

Parsing this stuff out went straight into a utility module.

```elm
module Paginated.Util exposing (links)

import Dict exposing (Dict)
import Maybe.Extra
import Regex


{-| Parse an HTTP Link header into a dictionary. For example, to look
for a link to additional results in an API response, you could do the
following:

    Dict.get "Link" response.headers
        |> Maybe.map links
        |> Maybe.andThen (Dict.get "next")

-}
links : String -> Dict String String
links s =
    let
        toTuples xs =
            case xs of
                [ Just a, Just b ] ->
                    Just ( b, a )

                _ ->
                    Nothing
    in
        Regex.find
            Regex.All
            (Regex.regex "<(.*?)>; rel=\"(.*?)\"")
            s
            |> List.map .submatches
            |> List.map toTuples
            |> Maybe.Extra.values
            |> Dict.fromList
```

A little bit of regular expression magic, tuples, and
`Maybe.Extra.values` to keep the matches, and now I've got my
(`Maybe`) URL.


## Time to make some requests {#time-to-make-some-requests}

Now's the time to define some types. I'll need a `Request`, which will
be similar to a standard `Http.Request`, with a _slight_ difference.

```elm
type alias RequestOptions a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , decoder : Decoder a
    , timeout : Maybe Time.Time
    , withCredentials : Bool
    }


type Request a
    = Request (RequestOptions a)
```

What separates it from a basic `Http.Request` is the `decoder` field
instead of an `expect` field. The `expect` field in an HTTP request is
responsible for parsing the full response into whatever result the
caller wants. For my purposes, I always intend to be hitting a JSON
API returning a list of items, and I have my own designs on parsing
bits of the request to pluck out the headers. Therefore, I expose only
a slot for including a JSON decoder representing the type of item I'll
be getting a collection of.

I'll also need a `Response`, which will either be `Partial`
(containing the results from the response, plus a `Request` for
getting the next batch), or `Complete`.

```elm
type Response a
    = Partial (Request a) (List a)
    | Complete (List a)
```

Sending the request isn't too bad. I can just convert my request into
an `Http.Request`, and use `Http.send`.

```elm
send :
    (Result Http.Error (Response a) -> msg)
    -> Request a
    -> Cmd msg
send resultToMessage request =
    Http.send resultToMessage <|
        httpRequest request


httpRequest : Request a -> Http.Request (Response a)
httpRequest (Request options) =
    Http.request
        { method = options.method
        , headers = options.headers
        , url = options.url
        , body = options.body
        , expect = expect options
        , timeout = options.timeout
        , withCredentials = options.withCredentials
        }


expect : RequestOptions a -> Http.Expect (Response a)
expect options =
    Http.expectStringResponse (fromResponse options)
```

All of my special logic for handling the headers, mapping the decoder
over the results, and packing them up into a `Response` is baked into
my `Http.Request` via a private `fromResponse` translator:

```elm
fromResponse :
    RequestOptions a
    -> Http.Response String
    -> Result String (Response a)
fromResponse options response =
    let
        items : Result String (List a)
        items =
            Json.Decode.decodeString
                (Json.Decode.list options.decoder)
                response.body

        nextPage =
            Dict.get "Link" response.headers
                |> Maybe.map Paginated.Util.links
                |> Maybe.andThen (Dict.get "next")
    in
        case nextPage of
            Nothing ->
                Result.map Complete items

            Just url ->
                Result.map
                    (Partial (request { options | url = url }))
                    items
```


## Putting it together {#putting-it-together}

Now, I can make my API request, and get back a response with
potentially partial results. All that needs to be done now is to make
my request, and iterate on the results I get back in my `update`
method.

To make things a bit easier, I add a method for concatenating two
responses:

```elm
update : Response a -> Response a -> Response a
update old new =
    case ( old, new ) of
        ( Complete items, _ ) ->
            Complete items

        ( Partial _ oldItems, Complete newItems ) ->
            Complete (oldItems ++ newItems)

        ( Partial _ oldItems, Partial request newItems ) ->
            Partial request (oldItems ++ newItems)
```

Putting it all together, I get a fully functional test app that
fetches a paginated list of repositories from GitLab, and renders them
when I've fetched them all:

```elm
module Example exposing (..)

import Html exposing (Html)
import Http
import Json.Decode exposing (field, string)
import Paginated exposing (Response(..))


type alias Model =
    { repositories : Maybe (Response String) }


type Msg
    = GotRepositories (Result Http.Error (Paginated.Response String))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( { repositories = Nothing }
    , getRepositories
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRepositories (Ok response) ->
            ( { model
                | repositories =
                    case model.repositories of
                        Nothing ->
                            Just response

                        Just previous ->
                            Just (Paginated.update previous response)
              }
            , case response of
                Partial request _ ->
                    Paginated.send GotRepositories request

                Complete _ ->
                    Cmd.none
            )

        GotRepositories (Err _) ->
            ( { model | repositories = Nothing }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.repositories of
        Nothing ->
            Html.div [] [ Html.text "Loading" ]

        Just (Partial _ _) ->
            Html.div [] [ Html.text "Loading..." ]

        Just (Complete repos) ->
            Html.ul [] <|
                List.map
                    (\x -> Html.li [] [ Html.text x ])
                    repos


getRepositories : Cmd Msg
getRepositories =
    Paginated.send GotRepositories <|
        Paginated.get
            "http://git.phoenixinquis.net/api/v4/projects?per_page=5"
            (field "name" string)
```


## There's got to be a better way {#there-s-got-to-be-a-better-way}

I've got it working, and it's working well. However, it's kind of a
pain to use. It's nice that I can play with the results as they come
in by peeking into the `Partial` structure, but it's a real chore to
have to stitch the results together in my application's `update`
method. It'd be nice if I could somehow encapsulate that behavior in
my request and not have to worry about the pagination at all in my
app.

It just so happens that, with Tasks, I can.

_Feel free to check out the full library documentation and code
referenced in this post [here](http://package.elm-lang.org/packages/correl/elm-paginated/1.0.1)._

_Continue on with part two, [Cleaner Recursive HTTP Requests with Elm
Tasks]({{< relref "cleaner-recursive-http-with-elm-tasks.md" >}})._