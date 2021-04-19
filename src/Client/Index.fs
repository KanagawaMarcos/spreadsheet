module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = { Todos: Todo list; Input: string }

type Position = char * int

type State =
    { Cols: char list
      Rows: int list
      Cells: Map<Position, string>
      Active: Position option }

let state =
    { Rows = [ 1 .. 15 ]
      Cols = [ 'A' .. 'K' ]
      Active = None
      Cells = Map.empty }

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model = { Todos = []; Input = "" }

    let cmd =
        Cmd.OfAsync.perform todosApi.getTodos () GotTodos

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Input

        let cmd =
            Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        { model with Input = "" }, cmd
    | AddedTodo todo ->
        { model with
              Todos = model.Todos @ [ todo ] },
        Cmd.none

open Feliz

let renderCell dispatch pos state = Html.td [ Html.p "|...|" ]

let view (model: Model) (dispatch: Msg -> unit) =
    Html.table [
        yield
            Html.tr [
                yield Feliz.Html.th []
                for col in state.Cols -> Feliz.Html.th [ Feliz.Html.h1 $"{col}" ]
            ]
        for row in state.Rows ->
            Html.tr [
                yield Html.th [ Html.h1 $"{row}" ]
                for col in state.Cols -> renderCell dispatch (col, row) state
            ]
    ]
