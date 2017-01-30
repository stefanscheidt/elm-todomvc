module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput)
import Html.Keyed as Keyed
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
    | ClearCompleted
    | Delete Todo
    | Filter FilterState
    | InputTitle String
    | Toggle Todo


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

        ClearCompleted ->
            let
                newTodos =
                    model.todos |> List.filter (not << .completed)
            in
                { model | todos = newTodos }

        Delete todo ->
            let
                newTodos =
                    model.todos
                        |> List.filter (\t -> t.id /= todo.id)
            in
                { model | todos = newTodos }

        Filter filterState ->
            { model | filter = filterState }

        InputTitle newTitle ->
            let
                oldTodo =
                    model.todo

                newTodo =
                    { oldTodo | title = newTitle }
            in
                { model | todo = newTodo }

        Toggle todo ->
            let
                newTodos =
                    model.todos
                        |> List.map
                            (\it ->
                                if it.id == todo.id then
                                    toggle it
                                else
                                    it
                            )
            in
                { model | todos = newTodos }


toggle : Todo -> Todo
toggle todo =
    { todo | completed = not todo.completed }


filterTodos : FilterState -> List Todo -> List Todo
filterTodos filterState todos =
    let
        filter =
            case filterState of
                All ->
                    always True

                Active ->
                    not << .completed

                Completed ->
                    .completed
    in
        todos
            |> List.filter filter


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
            [ Keyed.ul [ class "todo-list" ]
                (model.todos
                    |> filterTodos model.filter
                    |> List.map todoView
                )
            ]
        , footer [ class "footer" ]
            [ span [ class "todo-count" ]
                [ strong []
                    [ text (toString (model.todos |> filterTodos Active |> List.length))
                    ]
                , text " items left"
                ]
            , ul [ class "filters" ]
                [ filterItemView model All
                , filterItemView model Active
                , filterItemView model Completed
                ]
            , button
                [ class "clear-completed"
                , onClick ClearCompleted
                ]
                [ text "Clear completed" ]
            ]
        ]


todoView : Todo -> ( String, Html Msg )
todoView todo =
    let
        key =
            "todo-" ++ (toString todo.id)

        item =
            li [ classList [ ( "completed", todo.completed ) ] ]
                [ div [ class "view" ]
                    [ input
                        [ class "toggle"
                        , type_ "checkbox"
                        , checked todo.completed
                        , onCheck (\_ -> Toggle todo)
                        ]
                        []
                    , label [] [ text todo.title ]
                    , button
                        [ class "destroy"
                        , onClick (Delete todo)
                        ]
                        []
                    ]
                ]
    in
        ( key, item )


filterItemView : Model -> FilterState -> Html Msg
filterItemView model filterState =
    li []
        [ a
            [ classList [ ( "selected", model.filter == filterState ) ]
            , onClick (Filter filterState)
            , href "#"
            ]
            [ text (toString filterState) ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
