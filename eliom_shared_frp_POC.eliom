[%%shared

open Eliom_lib
open Eliom_content
open Html.D

open FRP.Types

module S = Eliom_shared.React.S
module R = Html.R
]

[%%shared

type row_model  = { v : int; row_valid : bool }

type row_action =
  | Set of int
  | Try_set of string
  | Invalid
  (* [@@deriving show] *)

module ROW =
struct
  let create v = { v; row_valid = true }

  let update m action =
    match action with
    | Set v -> { v; row_valid = true; }
    | Invalid -> { m with row_valid = false; }
    | Try_set s -> begin
        match int_of_string s with
          | exception _ -> { m with row_valid = false }
          | v -> { v; row_valid = true; }
      end

  let view (port : row_action FRP.Types.port) m =
    let inp =
      Raw.input
        ~a:[ R.a_value @@
               S.map [%shared fun m -> string_of_int m.v] m;
             R.a_style @@
               S.map
                 [%shared
                    function
                      | { row_valid = true; _ } -> ""
                      | { row_valid = false; _ } -> "border: 2px solid red"]
                 m
        ]
        () in

    let () =
      ignore [%client ( FRP.on_keyups (fun s -> (Try_set s, None)) ~%inp ~%port : unit)]
    in
      div [ inp ]
end

module IM = ReactMap.Make(struct type t = int let compare = compare end)

type test_model =
    { a      : int;
      valid  : bool;
      row_id : int;
      rows   : row_model IM.t;
    }

type test_action =
  | Set of int
  | Try_set of string
  | Invalid
  | Increment
  | Decrement
  | Add_row
  | Row_action of int * row_action
  (* [@@deriving show] *)
]

[%%shared

let mk_row_view port =
  [%shared fun id row ->
    ROW.view (FRP.tag_port (fun a -> Row_action (id, a)) ~%port) row]

module TEST =
struct

  let make a =
    { a; valid = true; row_id = 1;
      rows = IM.add 0 (ROW.create 111) IM.empty;
    }

  let update m = function
    | Set a -> { m with a; valid = true; }
    | Increment -> { m with a = m.a + 1 }
    | Decrement -> { m with a = m.a - 1 }
    | Invalid -> { m with valid = false }
    | Try_set s -> begin
        match int_of_string s with
          | exception _ -> { m with valid = false }
          | a -> { m with a; valid = true; }
      end
    | Add_row ->
        let id = m.row_id in
          { m with row_id = m.row_id + 1;
                   rows   = IM.add id (ROW.create 1) m.rows;
          }
    | Row_action (id, action) -> begin
        match IM.find id m.rows with
          | exception Not_found -> m
          | row -> { m with rows = IM.add id (ROW.update row action) m.rows }
      end

  let view (port : test_action FRP.Types.port) m =
    let input_a =
      Raw.input
        ~a:[ R.a_value @@ S.map [%shared (fun m -> string_of_int m.a)] m;
             R.a_style @@
               S.map
                 [%shared
                   function
                     | { valid = false } -> "border: 2px solid red"
                     | _ -> "" ]
                 m;
        ]
        () in

    let () =
      ignore [%client (FRP.on_keyups (fun s -> (Try_set s, None)) ~%input_a ~%port : unit)] in

    let input_b =
      Raw.input
        ~a:[ R.a_value @@ S.map [%shared (fun m -> string_of_int @@ 2 * m.a)] m ]
        () in

    let rows =
      R.div @@
      Eliom_shared.ReactiveData.RList.from_signal @@
      S.map [%shared fun rows -> List.rev @@ IM.fold (fun _ v l -> v :: l) rows []] @@
      S.switch @@
      S.map [%shared fun m -> IM.React.map ~eq:(FRP.shared_eq ()) (mk_row_view ~%port) m] @@
      S.const @@
      S.map ~eq:[%shared (==)] [%shared fun m -> m.rows] m

    in

      div
        [
          input_a; br ();
          input_b; br ();

          R.pcdata @@
            S.map [%shared (fun m -> Printf.sprintf "value: %d" m.a)] m;

          br ();

          button ~a:[FRP.onclick port Decrement] [ pcdata "- " ];
          pcdata " ";
          button ~a:[FRP.onclick port Increment] [ pcdata "+ " ];
          br ();

          button
            ~a:[a_onclick
                  [%client FRP.effect ~%port @@ FRP.focus ~%input_a]]
            [ pcdata "Focus A" ];
          pcdata " ";
          button
            ~a:[a_onclick
                  [%client FRP.effect ~%port @@ FRP.focus ~%input_b]]
            [ pcdata "Focus B" ];

          br();

          button ~a:[FRP.onclick port Add_row] [ pcdata "ADD ROW" ];
          pcdata " ";
          pcdata "SUM: ";
          R.pcdata @@
            S.map [%shared string_of_int] @@
            S.map
              [%shared fun m -> IM.fold (fun _ { v; _ } s -> v + s) m.rows 0] m;

          hr ();

          rows;
        ]
end
]

module Testreactive_app =
  Eliom_registration.App (
    struct
      let application_name = "eliom_shared_frp_POC"
      let global_data_path = None end)

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let () =
  Testreactive_app.register ~service:main_service @@ fun () () ->
  Lwt.return @@
  Eliom_tools.F.html
    ~title:"eliom_shared_frp_POC"
    ~css:[["css";"eliom_shared_frp_POC.css"]]
    Html.F.(body [
      h1 [pcdata "Welcome from Eliom's distillery!"];
      div [
        let m, pm = (S.create @@ TEST.make 42) in
        let ev    = [%client (React.E.create () : test_action FRP.Types.action_event)] in
          ignore [%client ( FRP.run (*~pp_action:show_test_action *)
                              TEST.update (~%m, ~%pm) ~%ev : unit) ];
          TEST.view (FRP.port ev) m
      ]
    ])
