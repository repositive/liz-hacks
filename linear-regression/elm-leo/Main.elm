port module Main exposing (main)

import Platform
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Json.Decode exposing (Decoder)


type alias Thetas =
    ( Float, Float )


type alias Observation =
    ( Float, Float, Float )


type alias StringDecoder =
    Decoder String


learningRate : Float
learningRate =
    0.01


stepSum : List Observation -> Thetas -> (Observation -> Float) -> Float
stepSum listObservations ( thetaN0, thetaN1 ) fromObsToD =
    let
        m =
            List.length listObservations

        term di (( xi, yi, zi ) as obs) =
            (thetaN0 * zi + thetaN1 * xi + yi) * (di obs) * learningRate / (toFloat m)
    in
        listObservations
            |> List.map (term fromObsToD)
            |> List.sum


nextTheta : List Observation -> Thetas -> Thetas
nextTheta observations (( thetaN0, thetaN1 ) as thetasN) =
    ( thetaN0 - (stepSum observations thetasN (\( _, _, z ) -> z))
    , thetaN1
        - (stepSum observations
            thetasN
            (\( x, _, _ ) -> x)
          )
    )


parseObservations : String -> Result String (List Observation)
parseObservations data =
    data
        |> String.lines
        |> (Maybe.withDefault [] << List.tail)
        |> List.map (String.split ",")
        |> List.filter ((<) 1 << (List.length))
        |> List.map
            (\obsAsList ->
                case Debug.log "observation -> " obsAsList of
                    [ _, x, y ] ->
                        Ok ( String.toFloat x |> Result.withDefault 0, String.toFloat y |> Result.withDefault 0, 1.0 )

                    _ ->
                        Err ("the given observation wasn't composed of 3 items" ++ toString obsAsList)
            )
        |> List.foldr
            (\obsAsResTuple resList ->
                case resList of
                    Ok list ->
                        case obsAsResTuple of
                            Ok ( x, y, z ) ->
                                Ok (( x, y, z ) :: list)

                            _ ->
                                Err ("something was amiss in this observation " ++ (toString obsAsResTuple))

                    Err _ ->
                        resList
            )
            (Ok [])


linearRegression : String -> Result String Thetas
linearRegression data =
    data
        |> parseObservations
        |> Debug.log "observations"
        |> Result.andThen (Ok << compute)


compute : List Observation -> Thetas
compute observations =
    let
        theta1 =
            nextTheta observations ( 0.0, 0.0 )
    in
        computeUntilStable observations theta1


computeUntilStable : List Observation -> Thetas -> Thetas
computeUntilStable observations previousThetas =
    let
        thetas =
            nextTheta observations previousThetas

        _ =
            Debug.log "thetas " thetas
    in
        if (previousThetas == thetas) then
            thetas
        else
            computeUntilStable observations thetas


main : Platform.Program Never () Msg
main =
    Platform.program
        { init = ( (), Cmd.none )
        , update = update
        , subscriptions = always <| incomingData IncomingData
        }


type Msg
    = IncomingData String


port incomingData : (String -> msg) -> Sub msg


port result : ( Float, Float ) -> Cmd msg


update : Msg -> () -> ( (), Cmd Msg )
update msg () =
    case msg of
        IncomingData data ->
            ( (), result <| Result.withDefault ( 666, 666 ) <| linearRegression data )
