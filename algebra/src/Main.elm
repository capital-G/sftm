module Main exposing (..)

import Dict exposing (Dict)
import Dom.Scroll
import Html exposing (Html)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode exposing (Value)
import Navigation exposing (Location)
import Page.About
import Page.Introduction
import Page.List
import Page.Proof
import Page.Solve
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Ports
import Problem exposing (Problem)
import Problems
import Task
import Utilities exposing (..)


type alias Model =
    { problems : List Problem
    , scrollY : Float
    , page : Page
    , origin : String
    }


type Page
    = ListPage
    | IntroductionPage
    | AboutPage
    | SolvePage Int Page.Solve.Model
    | ProofPage Page.Proof.Model


type Msg
    = IgnoreMsg
    | LocalStorageChangedMsg (Maybe String)
    | GoToProblemMsg Int
    | GoToIntroductionMsg
    | GoToAboutMsg
    | OnScrollMsg
    | SaveScrollMsg Float
    | LocationUpdateMsg Location
    | IntroductionBackButtonClickedMsg
    | AboutBackButtonClickedMsg
    | SolvePageMsg Page.Solve.Msg


type alias Flags =
    { local : Maybe String }


version =
    35


fullPath model =
    model.origin


mainPath model =
    fullPath model ++ "/"


decodeLocalStorage : Model -> Json.Decode.Decoder Model
decodeLocalStorage model =
    let
        f version1 ps =
            if version1 == version then
                { model | problems = List.map2 Problem.restoreProblem ps model.problems }
            else
                model
    in
        Json.Decode.Pipeline.decode f
            |> Json.Decode.Pipeline.required "version" Json.Decode.int
            |> Json.Decode.Pipeline.required "problems" (Json.Decode.list Problem.localStorageDecoder)


localStorageValue : Model -> Value
localStorageValue model =
    Json.Encode.object
        [ ( "version", Json.Encode.int version )
        , ( "problems", Json.Encode.list (List.map Problem.encoder model.problems) )
        ]


encodeLocalStorage : Model -> String
encodeLocalStorage model =
    Json.Encode.encode 4 (localStorageValue model)


currentSolvePageModel model =
    case model.page of
        ListPage ->
            Debug.crash "impossible"

        IntroductionPage ->
            Debug.crash "impossible"

        AboutPage ->
            Debug.crash "impossible"

        SolvePage _ model1 ->
            model1

        ProofPage _ ->
            Debug.crash "impossible"


problemIndex model =
    case model.page of
        ListPage ->
            Debug.crash "impossible"

        IntroductionPage ->
            Debug.crash "impossible"

        AboutPage ->
            Debug.crash "impossible"

        SolvePage i1 _ ->
            i1

        ProofPage _ ->
            Debug.crash "impossible"


setCurrentProblem : Problem -> Model -> Model
setCurrentProblem problem model =
    { model | problems = set (problemIndex model) problem model.problems }


currentProblem : Model -> Problem
currentProblem model =
    unsafeGet (problemIndex model) model.problems


setLocalStorage : Model -> Cmd msg
setLocalStorage =
    encodeLocalStorage >> Ports.setLocalStorage


setPage : Bool -> String -> Cmd msg
setPage isInit path =
    if isInit then
        Ports.setPage path
    else
        Cmd.none


{-| This occurs when localStorage changes.
-}
modelUpdated : Model -> Model -> Model
modelUpdated newModel currentModel =
    case currentModel.page of
        SolvePage i _ ->
            let
                currentProblem =
                    unsafeGet i currentModel.problems

                newProblem =
                    unsafeGet i newModel.problems
            in
                if currentProblem == newProblem then
                    newModel
                else
                    { newModel | page = SolvePage i (Page.Solve.init i newProblem) }

        _ ->
            newModel


setCurrentSolvePageModel solvePageModel model =
    case model.page of
        ListPage ->
            Debug.crash "impossible"

        IntroductionPage ->
            Debug.crash "impossible"

        AboutPage ->
            Debug.crash "impossible"

        SolvePage i1 _ ->
            { model | page = SolvePage i1 solvePageModel }

        ProofPage _ ->
            Debug.crash "impossible"


aboutLocation : Bool -> Model -> ( Model, Cmd Msg )
aboutLocation isInit model =
    case model.page of
        AboutPage ->
            ( model, Cmd.none )

        _ ->
            let
                newModel =
                    { model | page = AboutPage }
            in
                ( newModel
                , Cmd.batch
                    [ setPage isInit "/about/"
                    , setLocalStorage newModel
                    ]
                )


introductionLocation isInit model =
    case model.page of
        IntroductionPage ->
            ( model, Cmd.none )

        _ ->
            let
                newModel =
                    { model | page = IntroductionPage }
            in
            ( newModel, Cmd.batch [ setPage isInit "/introduction/", setLocalStorage newModel ] )


initialModel location =
    { problems = Problem.init Problems.problems
    , page = ListPage
    , scrollY = 0.0
    , origin = location.origin
    }


decodeModel storage location =
    let
        model =
            Maybe.withDefault model0 (Maybe.map decodeLocal storage.local)

        decodeLocal str =
            case Json.Decode.decodeString (decodeLocalStorage model0) str of
                Ok value ->
                    value

                Err error ->
                    Debug.log error model0

        model0 =
            initialModel location
    in
    model


listMsg msg =
    case msg of
        Page.List.GoToProblemMsg i1 ->
            GoToProblemMsg i1

        Page.List.GoToIntroductionMsg ->
            GoToIntroductionMsg

        Page.List.GoToAboutMsg ->
            GoToAboutMsg

        Page.List.OnScrollMsg ->
            OnScrollMsg


view model =
    case model.page of
        ListPage ->
            Html.map listMsg (Page.List.view model.problems)

        IntroductionPage ->
            Page.Introduction.view IntroductionBackButtonClickedMsg version

        AboutPage ->
            Page.About.view AboutBackButtonClickedMsg version

        SolvePage _ model1 ->
            Html.map SolvePageMsg (Page.Solve.view (currentProblem model) model1)

        ProofPage model2 ->
            Page.Proof.view (GoToProblemMsg model2.index) model2


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.onLocalStorageChange LocalStorageChangedMsg


rootLocation : Bool -> Model -> ( Model, Cmd Msg )
rootLocation isInit model =
    case model.page of
        ListPage ->
            ( model, Cmd.none )

        _ ->
            let
                newModel =
                    { model | page = ListPage }
            in
                ( newModel
                , Cmd.batch
                    [ setPage isInit "/"
                    , setLocalStorage newModel
                    , Task.attempt (\result -> IgnoreMsg) (Dom.Scroll.toY "scrolling" newModel.scrollY)
                    ]
                )


problemLocation : Int -> Bool -> Model -> ( Model, Cmd Msg )
problemLocation i isInit model =
    let
        idx =
            i - 1

        isSame =
            case model.page of
                SolvePage j _ ->
                    idx == j

                _ ->
                    False
    in
        if isSame then
            ( model, Cmd.none )
        else
            let
                newModel =
                    { model | page = SolvePage idx (Page.Solve.init idx (unsafeGet idx model.problems)) }
            in
                ( newModel
                , Cmd.batch
                    [ setPage isInit ("/problem-" ++ toString i ++ "/")
                    , setLocalStorage newModel
                    , Task.attempt (\result -> IgnoreMsg) (Dom.Scroll.toY "scrolling" 0.0)
                    ]
                )


proofLocation i isInit model =
    let
        problem =
            unsafeGet idx model.problems

        isSame =
            case model.page of
                ProofPage model1 ->
                    idx == model1.index

                _ ->
                    False

        idx =
            i - 1
    in
    if isSame then
        ( model, Cmd.none )
    else
        case problem.proof of
            Just x1 ->
                let
                    newModel =
                        { model | page = ProofPage proofModel }

                    proofModel =
                        { name = problem.description
                        , index = idx
                        , start = problem.start
                        , steps = x1
                        , rules = problem.rules
                        }
                in
                ( newModel
                , Cmd.batch
                      [ setPage isInit ("/problem-" ++ toString i ++ "/proof/")
                      , setLocalStorage newModel
                      , Task.attempt (\result -> IgnoreMsg) (Dom.Scroll.toTop "scrolling")
                      , Task.attempt (\result -> IgnoreMsg) (Dom.Scroll.toLeft "scrolling")
                      ]
                )

            Nothing ->
                ( model, Navigation.newUrl (mainPath model) )


paths =
    Dict.fromList
        ([ ( "/", rootLocation ), ( "/introduction/", introductionLocation ), ( "/about/", aboutLocation ) ]
             ++ List.map
                    (\i -> ( "/problem-" ++ toString i ++ "/", problemLocation i ))
                    (List.range 1 (List.length Problems.problems + 1))
                    ++ List.map
                           (\i -> ( "/problem-" ++ toString i ++ "/proof/", proofLocation i ))
                           (List.range 1 (List.length Problems.problems + 1))
        )


locationUpdate isInit location model =
    let
        setPage =
            if isInit then
                Ports.setPage
            else
                \ignore -> Cmd.none

        setLocalStorage p28 =
            Ports.setLocalStorage (encodeLocalStorage p28)

        shortPath =
            location.pathname
    in
    case Dict.get shortPath paths of
        Nothing ->
            ( model, Navigation.newUrl (mainPath model) )

        Just x1 ->
            x1 isInit model


init flags location =
    let
        model =
            decodeModel flags location
    in
    case locationUpdate False location model of
        ( model2, cmd ) ->
            ( model2, Cmd.batch [ cmd, Ports.initialize () ] )


update msg model =
    case msg of
        IgnoreMsg ->
            ( model, Cmd.none )

        LocalStorageChangedMsg x1 ->
            case x1 of
                Nothing ->
                    ( model, Cmd.none )

                Just x2 ->
                    case Json.Decode.decodeString (decodeLocalStorage model) x2 of
                        Ok value ->
                            ( modelUpdated value model, Cmd.none )

                        Err error ->
                            Debug.log error ( model, Cmd.none )

        GoToProblemMsg i1 ->
            ( model, Navigation.newUrl (fullPath model ++ "/problem-" ++ toString (1 + i1) ++ "/") )

        GoToIntroductionMsg ->
            ( model, Navigation.newUrl (fullPath model ++ "/introduction/") )

        GoToAboutMsg ->
            ( model, Navigation.newUrl (fullPath model ++ "/about/") )

        SolvePageMsg msg1 ->
            case Page.Solve.update (currentProblem model) msg1 (currentSolvePageModel model) of
                Page.Solve.UnchangedOutMsg ->
                    ( model, Cmd.none )

                Page.Solve.ModelChangedOutMsg model1 ->
                    let
                        newModel =
                            setCurrentSolvePageModel model1 model
                    in
                    ( newModel, Cmd.none )

                Page.Solve.ProblemChangedOutMsg problem model2 ->
                    let
                        newModel =
                            setCurrentSolvePageModel model2 (setCurrentProblem problem model)
                    in
                    ( newModel, Ports.setLocalStorage (encodeLocalStorage newModel) )

                Page.Solve.SolvedOutMsg x3 problem3 ->
                    let
                        newModel1 =
                            setCurrentProblem problem2 model

                        problem2 =
                            { problem1 | proof = Just steps }

                        steps =
                            case problem3.proof of
                                Nothing ->
                                    x3

                                Just x4 ->
                                    if List.length x4 < List.length x3 then
                                        x4
                                    else
                                        x3

                        problem1 =
                            Problem.reset problem3

                        solvedCmd =
                            if isInitial then
                                Ports.problemSolved
                                    { problemIndex = index
                                    , stepCount = List.length x3
                                    , remaining = List.length (List.filter (\p36 -> not (.solved p36)) model.problems) - 1
                                    }
                            else
                                Cmd.none

                        index =
                            problemIndex model

                        isInitial =
                            case problem3.proof of
                                Nothing ->
                                    True

                                Just _ ->
                                    False
                    in
                    ( newModel1
                    , Cmd.batch
                          [ Ports.setLocalStorage (encodeLocalStorage newModel1)
                          , solvedCmd
                          , Navigation.newUrl (mainPath model)
                          ]
                    )

                Page.Solve.ModelCmdChangedOutMsg model3 x5 ->
                    let
                        newModel =
                            setCurrentSolvePageModel model3 model
                    in
                    ( newModel, Cmd.map SolvePageMsg x5 )

                Page.Solve.ExitOutMsg ->
                    ( model, Navigation.newUrl (mainPath model) )

                Page.Solve.ShowProofOutMsg ->
                    ( model
                    , Navigation.newUrl (fullPath model ++ "/problem-" ++ toString (problemIndex model + 1) ++ "/proof/")
                    )

        IntroductionBackButtonClickedMsg ->
            ( model, Navigation.newUrl (mainPath model) )

        AboutBackButtonClickedMsg ->
            ( model, Navigation.newUrl (mainPath model) )

        OnScrollMsg ->
            ( model
            , Task.attempt (Result.withDefault IgnoreMsg << Result.map SaveScrollMsg) (Dom.Scroll.y "scrolling")
            )

        SaveScrollMsg f1 ->
            let
                newModel =
                    { model | scrollY = f1 }
            in
            ( newModel, Cmd.none )

        LocationUpdateMsg location ->
            locationUpdate True location model


main : Program Flags Model Msg
main =
    Navigation.programWithFlags
        LocationUpdateMsg
        { subscriptions = subscriptions, init = init, view = view, update = update }
