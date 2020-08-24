port module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Html
import Html.Attributes
import Html.Entity
import Html.Events
import Json.Decode
import Json.Encode
import NonEmpty exposing (NonEmpty(..))
import Random
import Task
import Url


port debug : String -> Cmd msg


port exportFlashcardsJson : Json.Encode.Value -> Cmd msg


type alias Flashcard =
    { concept : String
    , definition : String
    , id : Int
    }


type ModelState
    = TakingNotes
    | FlashcardQuiz (NonEmpty Flashcard) (NonEmpty Flashcard) Int Int
    | QuizComplete Int Int
    | Fatal String


type alias Model =
    { flashcards : List Flashcard
    , key : Browser.Navigation.Key
    , concept : String
    , definition : String
    , state : ModelState
    , seed : Random.Seed
    , flashcardId : Int
    }


type Msg
    = NoOp
    | AddFlashcardClicked
    | ConceptChanged String
    | DefinitionChanged String
    | StartQuizClicked (NonEmpty Flashcard)
    | GotFocusResult (Result Browser.Dom.Error ())
    | EndQuizClicked
    | DefinitionClicked Flashcard
    | QuizCompleteOkClicked
    | ExportClicked
    | DeleteFlashcardClicked Flashcard


flagsDecoder : Json.Decode.Decoder (List Flashcard)
flagsDecoder =
    Json.Decode.oneOf
        [ Json.Decode.list flashcardDecoder
        ]


init : Json.Decode.Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags _ key =
    let
        concept =
            "Elm"

        definition =
            "A purely functional programming language that compiles to JavaScript"

        decodeResult =
            Json.Decode.decodeValue flagsDecoder flags

        ( state, modelFlashcards ) =
            case decodeResult of
                Ok flashcards ->
                    ( TakingNotes, flashcards )

                Err err ->
                    ( Fatal (Json.Decode.errorToString err), [] )
    in
    ( { flashcards =
            Result.withDefault [] decodeResult
      , key = key
      , concept = concept
      , definition = definition
      , state = state
      , seed = Random.initialSeed 42
      , flashcardId =
            List.foldl max 0 (List.map .id modelFlashcards)
      }
    , Cmd.none
    )


viewFlashcard : Flashcard -> Html.Html Msg
viewFlashcard flashcard =
    Html.div [ Html.Attributes.class "flashcard" ]
        [ Html.div [ Html.Attributes.class "flashcard-concept" ] [ Html.text flashcard.concept ]
        , Html.div [ Html.Attributes.class "flashcard-definition" ] [ Html.text flashcard.definition ]
        , Html.div [ Html.Attributes.class "flashcard-delete", Html.Events.onClick (DeleteFlashcardClicked flashcard) ] [ Html.text Html.Entity.times ]
        ]


viewFlashcardQuiz (NonEmpty question _) flashcards numCorrect numAsked =
    [ Html.div [ Html.Attributes.id "forms" ]
        [ Html.div []
            [ Html.div [ Html.Attributes.style "font-size" "24px" ]
                [ Html.text "What is the definition of ", Html.strong [] [ Html.text question.concept ], Html.text "?" ]
            , Html.div [ Html.Attributes.class "quiz-results" ]
                [ Html.text <| String.fromInt numCorrect
                , Html.text "/"
                , Html.text <| String.fromInt numAsked
                , Html.text " correct"
                ]
            ]
        ]
    , Html.div [ Html.Attributes.id "items" ] (List.map viewFlashcardQuizChoice <| NonEmpty.toList flashcards)
    , Html.div [ Html.Attributes.id "navbar" ] [ Html.a [ Html.Events.onClick EndQuizClicked ] [ Html.text "End Quiz" ] ]
    ]


viewFlashcardQuizChoice : Flashcard -> Html.Html Msg
viewFlashcardQuizChoice flashcard =
    Html.div [ Html.Attributes.class "quiz-definition", Html.Events.onClick (DefinitionClicked flashcard) ] [ Html.text flashcard.definition ]


viewTakingNotes model =
    let
        ok =
            (String.length model.concept > 0)
                && (String.length model.definition > 0)
                && List.all (.concept >> (/=) model.concept) model.flashcards

        onSubmit =
            if ok then
                AddFlashcardClicked

            else
                NoOp
    in
    [ Html.div [ Html.Attributes.id "forms" ]
        [ Html.form [ Html.Events.onSubmit AddFlashcardClicked, Html.Attributes.class "add-flashcard-form" ]
            [ Html.div [] [ Html.h1 [ Html.Attributes.style "margin" "0" ] [ Html.text "Add a Flashcard" ] ]
            , Html.div [] [ Html.text "Concept" ]
            , Html.input
                [ Html.Events.onInput ConceptChanged
                , Html.Attributes.value model.concept
                , Html.Attributes.id "concept"
                ]
                []
            , Html.div [] [ Html.text "Definition" ]
            , Html.textarea [ Html.Events.onInput DefinitionChanged, Html.Attributes.value model.definition ] []
            , Html.button [ Html.Attributes.id "add-flashcard-button", Html.Attributes.disabled (not ok) ] [ Html.text "Add Flashcard" ]
            ]
        ]
    ]
        ++ (case NonEmpty.fromList model.flashcards of
                Just flashcards ->
                    [ Html.div [ Html.Attributes.id "navbar" ]
                        [ Html.a [ Html.Events.onClick (StartQuizClicked flashcards) ] [ Html.text "Start Quiz" ]
                        , Html.a [ Html.Events.onClick ExportClicked ] [ Html.text "Export Quiz" ]
                        ]
                    , Html.div [ Html.Attributes.id "items" ] (List.map viewFlashcard model.flashcards)
                    ]

                Nothing ->
                    [ Html.div [ Html.Attributes.id "item" ] [ Html.text "No Flashcards" ] ]
           )


viewFatal err =
    [ Html.div [ Html.Attributes.id "fatal-error" ] [ Html.h1 [] [ Html.text "A fatal error occurred" ], Html.p [] [ Html.pre [] [ Html.text err ] ] ] ]


view : Model -> Browser.Document Msg
view model =
    { title = "Flashcards"
    , body =
        case model.state of
            Fatal err ->
                viewFatal err

            TakingNotes ->
                viewTakingNotes model

            FlashcardQuiz questions choices numCorrect numAsked ->
                viewFlashcardQuiz questions choices numCorrect numAsked

            QuizComplete numCorrect numAsked ->
                [ Html.div [ Html.Attributes.id "quiz-complete-dialog" ]
                    [ Html.h1 [] [ Html.text "Quiz Complete" ]
                    , Html.sub []
                        [ Html.text "You got "
                        , Html.text <| String.fromInt numCorrect
                        , Html.text " out of "
                        , Html.text <| String.fromInt numAsked
                        , Html.text " correct"
                        ]
                    ]
                , Html.div [ Html.Attributes.id "navbar" ] [ Html.a [ Html.Events.onClick QuizCompleteOkClicked ] [ Html.text "Ok" ] ]
                ]
    }


encodeFlashcard : Flashcard -> Json.Encode.Value
encodeFlashcard flashcard =
    Json.Encode.object
        [ ( "id", Json.Encode.int flashcard.id )
        , ( "concept", Json.Encode.string flashcard.concept )
        , ( "definition", Json.Encode.string flashcard.definition )
        ]


flashcardDecoder : Json.Decode.Decoder Flashcard
flashcardDecoder =
    Json.Decode.map3 (\id concept definition -> { id = id, concept = concept, definition = definition })
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "concept" Json.Decode.string)
        (Json.Decode.field "definition" Json.Decode.string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddFlashcardClicked ->
            let
                flashcard : Flashcard
                flashcard =
                    { concept = model.concept
                    , definition = model.definition
                    , id = model.flashcardId
                    }

                flashcards : List Flashcard
                flashcards =
                    flashcard :: model.flashcards

                newFlashcardId : Int
                newFlashcardId =
                    model.flashcardId + 1
            in
            ( { model | flashcards = flashcards, flashcardId = newFlashcardId }, Cmd.none )

        ConceptChanged string ->
            ( { model | concept = string }, Cmd.none )

        DefinitionChanged string ->
            ( { model | definition = string }, Cmd.none )

        StartQuizClicked flashcards ->
            let
                ( choices, seed1 ) =
                    Random.step (NonEmpty.shuffle flashcards) model.seed

                ( questions, seed2 ) =
                    Random.step (NonEmpty.shuffle flashcards) seed1
            in
            ( { model | state = FlashcardQuiz questions choices 0 0, seed = seed2 }
            , Browser.Dom.focus "concept"
                |> Task.attempt GotFocusResult
            )

        GotFocusResult result ->
            case result of
                Ok () ->
                    ( model, Cmd.none )

                Err (Browser.Dom.NotFound id) ->
                    ( model, debug ("element does not exist: '" ++ id ++ "'") )

        EndQuizClicked ->
            ( { model | state = TakingNotes }, Cmd.none )

        DefinitionClicked choice ->
            case model.state of
                FlashcardQuiz (NonEmpty question questions) choices numCorrect numAsked ->
                    let
                        ( newChoices, newSeed ) =
                            Random.step (NonEmpty.shuffle choices) model.seed

                        newNumCorrect =
                            if choice.id == question.id then
                                numCorrect + 1

                            else
                                numCorrect

                        newState =
                            case questions of
                                nextQuestion :: remainingQuestions ->
                                    FlashcardQuiz (NonEmpty nextQuestion remainingQuestions) newChoices newNumCorrect (numAsked + 1)

                                [] ->
                                    QuizComplete newNumCorrect (numAsked + 1)
                    in
                    ( { model | state = newState, seed = newSeed }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        QuizCompleteOkClicked ->
            ( { model | state = TakingNotes }, Cmd.none )

        ExportClicked ->
            ( model, exportFlashcards model )

        DeleteFlashcardClicked flashcard ->
            let
                newFlashcards =
                    List.filter ((/=) flashcard.id << .id) model.flashcards
            in
            ( { model | flashcards = newFlashcards }, Cmd.none )


exportFlashcards : Model -> Cmd Msg
exportFlashcards model =
    exportFlashcardsJson (Json.Encode.list encodeFlashcard model.flashcards)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    NoOp


onUrlChange : Url.Url -> Msg
onUrlChange _ =
    NoOp


updateAndExport : Msg -> Model -> ( Model, Cmd Msg )
updateAndExport msg model =
    let
        ( newModel, cmd ) =
            update msg model
    in
    ( newModel, Cmd.batch [ exportFlashcards newModel, cmd ] )


main =
    Browser.application
        { init = init
        , view = view
        , update = updateAndExport
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }
