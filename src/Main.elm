module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onInput)
import Json.Decode as Json


type alias Todo =
    { id : Int
    , title : String
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
    , nextId : Int
    }


type Msg
    = Add Todo
    | Complete Todo
    | Delete Todo
    | Filter FilterState
    | InputTitle String


blankTodo : Todo
blankTodo =
    { id = 0
    , title = ""
    , completed = False
    , editing = False
    }


initialModel : Model
initialModel =
    { todos =
        [ { id = 1
          , title = "The first todo"
          , completed = False
          , editing = False
          }
        ]
    , todo = blankTodo
    , filter = All
    , nextId = 2
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
            { model
                | todos = { todo | id = model.nextId } :: model.todos
                , todo = blankTodo
                , nextId = model.nextId + 1
            }

        Complete todo ->
            model

        Delete todo ->
            model

        Filter filterState ->
            model

        InputTitle title ->
            let
                oldTodo =
                    model.todo

                newTodo =
                    { oldTodo | title = title }
            in
                { model | todo = newTodo }


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
                , onInput InputTitle
                , onEnter (Add model.todo)
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
