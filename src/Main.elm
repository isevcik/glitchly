import Browser
import Css exposing (..)
import Css.Animations
import Html
import Html.Attributes
import Html.Events
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing(..)
import Html.Styled.Events exposing(..)
import Json.Decode as Decode

main =
    Browser.element
        { init = init
        , subscriptions = (\model -> Sub.none)
        , update = update
        , view = view >> toUnstyled
        }

type alias Glitch =
    { color: String
    , angle: Float
    , offset: Float
    , duration: Float
    , delay: Float
    }

setOffset : Float -> Glitch -> Glitch
setOffset offset glitch =
    { glitch | offset = offset }

type alias Model =
    { text: String
    , glitch1: Glitch
    , glitch2: Glitch
    , glitch: Glitch
    }

type Msg 
    = TextChanged String
    | Slider1 Int
    | Slider2 Int

initText = "Glitch Effect Generator"

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model initText (Glitch "#ecf" 45 50 2 0) (Glitch "#2fc" 180 50 3 500) (Glitch "#000" 0 0 0 0) , Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextChanged text ->
            ( { model | text = text }, Cmd.none )

        Slider1 v ->
            ( { model | glitch1 = setOffset (toFloat v) model.glitch1 }, Cmd.none )

        Slider2 v ->
            ( { model | glitch2 = setOffset (toFloat v) model.glitch2 }, Cmd.none )

view : Model -> Html Msg
view model =
    div
        [ css
            [ displayFlex
            , flexDirection column
            , alignItems center

            ]

        ]
        [ div [ css [ margin2 (px 40) (px 0) ] ] [ glitchView model.text ( List.map (glitchView model.text [ ]) [ model.glitch1, model.glitch2 ] ) model.glitch ]
        , div [ css [ displayFlex, flexWrap Css.wrap, justifyContent center ] ] [ sliderView model.glitch1 Slider1, sliderView model.glitch2 Slider2 ]
        ]

glitchView : String -> List (Html Msg) -> Glitch -> Html Msg
glitchView text childs def =
    div
        [ css
            [ position ( if List.isEmpty childs then absolute else relative )
            , top (px 0)
            , left (px 0)
            , zIndex ( if List.isEmpty childs then (int -1) else (int 0) )
            , fontFamilies ["Montserrat", "sans-serif"]
            , fontSize (rem 3) 
            , color (hex def.color)
            , textAlign center
            , animationName (glitchAnim def)
            , animationDuration (sec def.duration)
            , animationDelay (ms def.delay)
            , Css.property "animation-iteration-count" "infinite"
            -- , Css.property "animation-timing-function" "cubic-bezier(0, 2.13, 1,-1.13)"
            -- , Css.property "animation-direction" "alternate"
            ]
        ]
        ( [ Html.Styled.text text ] ++ childs ) 

glitchAnim def =
    let
        x = def.offset / 100 * cos (degrees def.angle)
        y = def.offset / 100 * sin (degrees def.angle)
    in
    Css.Animations.keyframes 
        [ ( 0, [ Css.Animations.transform [ translate3d zero zero zero ] ] )
        , ( 25, [ Css.Animations.transform [ translate3d (rem x) (rem y) zero ] ] )
        , ( 30, [ Css.Animations.transform [ translate3d (rem -x) (rem -y) zero ] ] )
        , ( 35, [ Css.Animations.transform [ translate3d zero zero zero ] ] )
        ]

sliderView : Glitch -> (Int -> Msg) -> Html Msg
sliderView def slider =
    input 
        [ type_ "range"
        , css
            [ Css.property "-webkit-appearance" "none"
            , Css.height (px 1)
            , Css.width (px 300)
            , margin2 (px 40) (px 20)
            , backgroundColor (hex "#000")
            , outline none
            , pseudoClass ":-webkit-slider-thumb" 
                [ Css.property "-webkit-appearance" "none"
                , Css.height (px 35)
                , Css.width (px 35)
                , backgroundColor (hex "#000")
                , borderRadius (pct 50)
                ]
            , pseudoClass ":-webkit-slider-thumb:hover"
                [ 
                    backgroundColor (hex def.color)
                ] 
            ]
        , on "input" (Decode.map slider onChangeDecoder)
        ] []

onChangeDecoder : Decode.Decoder Int
onChangeDecoder =
     Decode.at ["target", "valueAsNumber"] Decode.int

-- directionView : Int -> Html Msg
-- directionView direction =
--     div
--         [ ]
--         [ text ( String.fromInt direction ) ]