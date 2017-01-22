module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, keyCode)
import Json.Decode as Json


type alias Todo =
    { title : String
    , completed : Bool
    , editing : Bool
    }


type FilterState
    = All
    | Active
    | Completed


type alias Model =
    { todos : List Todo
    , todo : Todo
    , filter : FilterState
    }


type Msg
    = Add Todo
    | Complete Todo
    | Delete Todo
    | Filter FilterState


mockTodo : Todo
mockTodo =
    { title = "A mock todo..."
    , completed = False
    , editing = False
    }


initialModel : Model
initialModel =
    { todos =
        [ { title = "The first todo"
          , completed = False
          , editing = False
          }
        ]
    , todo =
        { title = ""
        , completed = False
        , editing = False
        }
    , filter = All
    }


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not the right keycode"
    in
        on "keydown" (keyCode |> Json.andThen isEnter)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add todo ->
            { model | todos = todo :: model.todos }

        Complete todo ->
            model

        Delete todo ->
            model

        Filter filterState ->
            model


view : Model -> Html Msg
view model =
    section [ class "todoapp" ]
        [ header [ class "header" ]
            [ h1 [] [ text "todos" ]
            , input
                [ class "new-todo"
                , placeholder "What needs to be done?"
                , autofocus True
                , value model.todo.title
                , onEnter (Add mockTodo)
                ]
                []
            ]
        , section [ class "main" ]
            [ ul [ class "todo-list" ]
                (model.todos |> List.map todoView)
            ]
        ]


todoView : Todo -> Html Msg
todoView todo =
    li [ classList [ ( "completed", todo.completed ) ] ]
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox", checked todo.completed ] []
            , label [] [ text todo.title ]
            , button [ class "destroy" ] []
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
