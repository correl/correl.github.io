+++
title = "Cleaner Recursive HTTP Requests with Elm Tasks"
author = ["Correl Roush"]
date = 2018-01-23T00:00:00-05:00
keywords = ["emacs", "org-mode", "themes"]
tags = ["programming", "elm"]
draft = false
+++

_Continued from part one, [Recursive HTTP Requests with Elm]({{< relref "recursive-http-requests-with-elm.md" >}})._

In [my last post]({{< relref "recursive-http-requests-with-elm.md" >}}), I described my first pass at building a library to
fetch data from a paginated JSON REST API. It worked, but it wasn't
too clean. In particular, the handling of the multiple pages and
concatenation of results was left up to the calling code. Ideally,
both of these concerns should be handled by the library, letting the
application focus on working with a full result set. Using Elm's
Tasks, we can achieve exactly that!


## What's a Task? {#what-s-a-task}

A [Task](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Task) is a data structure in Elm which represents an asynchronous
operation that may fail, which can be mapped and **chained**. What this
means is, we can create an action, transform it, and chain it with
additional actions, building up a complex series of things to do into
a single `Task`, which we can then package up into a [Cmd](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Cmd#Cmd) and hand to
the Elm runtime to perform. You can think of it like building up a
[Future or Promise](https://en.wikipedia.org/wiki/Futures%5Fand%5Fpromises), setting up a sort of [callback](https://en.wikipedia.org/wiki/Callback%5F(computer%5Fprogramming)) chain of mutations
and follow-up actions to be taken. The Elm runtime will work its way
through the chain and hand your application back the result in the
form of a `Msg`.

So, tasks sound great!


## Moving to Tasks {#moving-to-tasks}

Just to get things rolling, let's quit using `Http.send`, and instead
prepare a simple `toTask` function leveraging the very handy
`Http.toTask`. This'll give us a place to start building up some more
complex behavior.

```elm
send :
    (Result Http.Error (Response a) -> msg)
    -> Request a
    -> Cmd msg
send resultToMessage request =
        toTask request
        |> Task.attempt resultToMessage


toTask : Request a -> Task Http.Error (Response a)
toTask =
    httpRequest >> Http.toTask
```


## Shifting the recursion {#shifting-the-recursion}

Now, for the fun bit. We want, when a request completes, to inspect
the result. If the task failed, we do nothing. If it succeeded, we
move on to checking the response. If we have a `Complete` response,
we're done. If we do not, we want to build another task for the next
request, and start a new iteration on that.

All that needs to be done here is to chain our response handling using
`Task.andThen`, and either recurse to continue the chain with the next
`Task`, or wrap up the final results with `Task.succeed`!

```elm
recurse :
    Task Http.Error (Response a)
    -> Task Http.Error (Response a)
recurse =
    Task.andThen
        (\response ->
            case response of
                Partial request _ ->
                    httpRequest request
                        |> Http.toTask
                        |> recurse

                Complete _ ->
                    Task.succeed response
        )
```

That wasn't so bad. The function recursion almost seems like cheating:
I'm able to build up a whole chain of requests _based_ on the results
without actually _having_ the results yet! The `Task` lets us define a
complete plan for what to do with the results, using what we know
about the data structures flowing through to make decisions and tack
on additional things to do.


## Accumulating results {#accumulating-results}

There's just one thing left to do: we're not accumulating results yet.
We're just handing off the results of the final request, which isn't
too helpful to the caller. We're also still returning our Response
structure, which is no longer necessary, since we're not bothering
with returning incomplete requests anymore.

Cleaning up the types is pretty easy. It's just a matter of switching
out some instances of `Response a` with `List a` in our type
declarations...

```elm
send :
    (Result Http.Error (List a) -> msg)
    -> Request a
    -> Cmd msg


toTask : Request a -> Task Http.Error (List a)


recurse :
    Task Http.Error (Response a)
    -> Task Http.Error (List a)
```

...then changing our `Complete` case to return the actual items:

```elm
Complete xs ->
    Task.succeed xs
```

The final step, then, is to accumulate the results. Turns out this is
**super** easy. We already have an `update` function that combines two
responses, so we can map _that_ over our next request task so that it
incorporates the previous request's results!

```elm
Partial request _ ->
    httpRequest request
        |> Http.toTask
        |> Task.map (update response)
        |> recurse
```


## Tidying up {#tidying-up}

Things are tied up pretty neatly, now! Calling code no longer needs to
care whether the JSON endpoints its calling paginate their results,
they'll receive everything they asked for as though it were a single
request. Implementation details like the `Response` structure,
`update` method, and `httpRequest` no longer need to be exposed.
`toTask` can be exposed now as a convenience to anyone who wants to
perform further chaining on their calls.

Now that there's a cleaner interface to the module, the example app is
looking a lot cleaner now, too:

```elm
module Example exposing (..)

import Html exposing (Html)
import Http
import Json.Decode exposing (field, string)
import Paginated


type alias Model =
    { repositories : Maybe (List String) }


type Msg
    = GotRepositories (Result Http.Error (List String))


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
        GotRepositories result ->
            ( { model | repositories = Result.toMaybe result }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.repositories of
        Nothing ->
            Html.div [] [ Html.text "Loading" ]

        Just repos ->
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

So, there we have it! Feel free to check out the my complete
`Paginated` library on the [Elm package index](http://package.elm-lang.org/packages/correl/elm-paginated/latest), or on [GitHub](https://github.com/correl/elm-paginated). Hopefully
you'll find it or this post useful. I'm still finding my way around
Elm, so any and all feedback is quite welcome :)