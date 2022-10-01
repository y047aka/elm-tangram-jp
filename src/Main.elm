module Main exposing (main)

import Browser
import Browser.Events as E
import Cycle
import Element as E
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as D
import Logo exposing (Pattern, Target)



-- MAIN


main : Program { height : Int, width : Int } Model Msg
main =
    Browser.element
        { init = \flags -> ( init flags, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { window : { width : Int, height : Int }
    , patterns : Cycle.Cycle Logo.Pattern
    , time : Float
    , logo : Logo.Model
    }


init : { width : Int, height : Int } -> Model
init window =
    { window = window
    , time = 0
    , logo = Logo.start
    , patterns = Cycle.init japan [ goat, samurai, Logo.bird, Logo.child, Logo.house, Logo.cat, Logo.camel, Logo.logo ]
    }



-- UPDATE


type Msg
    = OnResize Int Int
    | MouseMoved Float Float Float Float Float
    | MouseClicked
    | TimeDelta Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnResize width height ->
            { model | window = { width = width, height = height } }

        MouseMoved t x y dx dy ->
            { model
                | time = t
                , logo = Logo.perturb (t - model.time) x y dx dy model.logo
            }

        MouseClicked ->
            { model
                | patterns = Cycle.step model.patterns
                , logo = Logo.setPattern (Cycle.next model.patterns) model.logo
            }

        TimeDelta timeDelta ->
            { model
                | logo =
                    if Logo.isMoving model.logo then
                        Logo.step timeDelta model.logo

                    else
                        model.logo
            }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ E.onResize OnResize
        , if Logo.isMoving model.logo then
            E.onAnimationFrameDelta TimeDelta

          else
            Sub.none
        ]



-- VIEW


view : Model -> Html Msg
view model =
    E.layout [ E.width E.fill ] (tangram model)



-- CONTENT / TANGRAM


tangram : Model -> E.Element Msg
tangram model =
    E.html <|
        Logo.view
            [ style "max-height" "500px"
            , style "max-width" "500px"
            , onMouseMove
            , onClick MouseClicked
            ]
            model.logo


onMouseMove : Attribute Msg
onMouseMove =
    on "mousemove" <|
        D.map7 (\t x y dx dy w h -> MouseMoved t (x / w - 0.5) (0.5 - y / h) (dx / w) (-dy / h))
            (D.field "timeStamp" D.float)
            (D.field "offsetX" D.float)
            (D.field "offsetY" D.float)
            (D.field "movementX" D.float)
            (D.field "movementY" D.float)
            (D.field "currentTarget" (D.field "clientWidth" D.float))
            (D.field "currentTarget" (D.field "clientHeight" D.float))



-- PATTERNS


japan : Pattern
japan =
    { tb1 = Target 250 -40 135
    , tb2 = Target -85 -290 180
    , tm = Target -500 -220 315
    , sqr = Target 450 330 0
    , par = Target -200 -110 0
    , ts1 = Target 280 -260 315
    , ts2 = Target -380 -320 0
    }


goat : Pattern
goat =
    { tb1 = Target 250 -100 45
    , tb2 = Target -80 -120 0
    , tm = Target -435 130 315
    , sqr = Target -225 85 0
    , par = Target -350 340 135
    , ts1 = Target -240 -290 45
    , ts2 = Target 450 0 90
    }


samurai : Pattern
samurai =
    { tb1 = Target 250 -40 135
    , tb2 = Target -220 -220 -45
    , tm = Target 240 -310 165
    , sqr = Target 50 50 0
    , par = Target -530 -110 325
    , ts1 = Target 160 230 10
    , ts2 = Target 470 140 225
    }
