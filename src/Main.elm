port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput)
import Html.Keyed as Keyed
import Json.Decode as JD
import Json.Encode as JE


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
    | NoOp
    | SetModel Model
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
                JD.succeed msg
            else
                JD.fail "not the right keycode"
    in
        on "keydown" (keyCode |> JD.andThen isEnter)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add todo ->
            let
                newModel =
                    { model
                        | todos = { todo | id = model.nextId } :: model.todos
                        , todo = blankTodo
                        , nextId = model.nextId + 1
                    }
            in
                ( newModel, saveModel newModel )

        ClearCompleted ->
            let
                newTodos =
                    model.todos |> List.filter (not << .completed)

                newModel =
                    { model | todos = newTodos }
            in
                ( newModel, saveModel newModel )

        Delete todo ->
            let
                newTodos =
                    model.todos
                        |> List.filter (\t -> t.id /= todo.id)

                newModel =
                    { model | todos = newTodos }
            in
                ( newModel, saveModel newModel )

        Filter filterState ->
            let
                newModel =
                    { model | filter = filterState }
            in
                ( newModel, saveModel newModel )

        InputTitle newTitle ->
            let
                oldTodo =
                    model.todo

                newTodo =
                    { oldTodo | title = newTitle }

                newModel =
                    { model | todo = newTodo }
            in
                ( newModel, saveModel newModel )

        NoOp ->
            ( model, Cmd.none )

        SetModel newModel ->
            ( newModel, Cmd.none )

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

                newModel =
                    { model | todos = newTodos }
            in
                ( newModel, saveModel model )


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


port loadModelJson : (JD.Value -> msg) -> Sub msg


port saveModelJson : JE.Value -> Cmd msg


saveModel : Model -> Cmd Msg
saveModel model =
    model |> encodeModel |> saveModelJson


encodeModel : Model -> JE.Value
encodeModel model =
    JE.object
        [ ( "todos", model.todos |> List.map encodeTodo |> JE.list )
        , ( "todo", model.todo |> encodeTodo )
        , ( "filter", model.filter |> encodeFilterState )
        , ( "nextId", model.nextId |> JE.int )
        ]


encodeTodo : Todo -> JE.Value
encodeTodo todo =
    JE.object
        [ ( "id", todo.id |> JE.int )
        , ( "title", todo.title |> JE.string )
        , ( "completed", todo.completed |> JE.bool )
        , ( "editing", todo.editing |> JE.bool )
        ]


encodeFilterState : FilterState -> JE.Value
encodeFilterState filterState =
    filterState |> toString |> JE.string


decodeModelJson : JD.Value -> Result String Model
decodeModelJson value =
    JD.decodeValue modelDecoder value


modelDecoder : JD.Decoder Model
modelDecoder =
    JD.map4 Model
        (JD.field "todos" (JD.list todoDecoder))
        (JD.field "todo" todoDecoder)
        (JD.field "filter" filterStateDecoder)
        (JD.field "nextId" JD.int)


todoDecoder : JD.Decoder Todo
todoDecoder =
    JD.map4 Todo
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)
        (JD.field "completed" JD.bool)
        (JD.field "editing" JD.bool)


filterStateDecoder : JD.Decoder FilterState
filterStateDecoder =
    let
        stringToFilterState string =
            case string of
                "Active" ->
                    Active

                "Completed" ->
                    Completed

                _ ->
                    All
    in
        JD.map stringToFilterState JD.string


decodeModelJsonToMsg : JD.Value -> Msg
decodeModelJsonToMsg value =
    case (decodeModelJson value) of
        Ok model ->
            SetModel model

        Err errorMessage ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    loadModelJson decodeModelJsonToMsg


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
