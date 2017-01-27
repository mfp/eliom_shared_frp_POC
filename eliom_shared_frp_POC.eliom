[%%shared
open Printf
open Eliom_lib

module S = Eliom_shared.React.S
module R = Eliom_content.Html.R
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

  let update (port : row_action FRP.port) m action =
    match action with
    | Set v -> { v; row_valid = true; }
    | Invalid -> { m with row_valid = false; }
    | Try_set s -> begin
        match int_of_string s with
          | exception _ -> { m with row_valid = false }
          | v -> { v; row_valid = true; }
      end

  let view (port : row_action FRP.Types.port) m =
    let open Eliom_content.Html.D in
    let inp =
      Raw.input
        ~a:[ R.a_value @@ S.map [%shared fun m -> string_of_int m.v] m;
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
    { a          : int;
      valid      : bool;
      row_id     : int;
      rows       : row_model IM.t;
      submitting : bool;
      result     : string option;
    }

type test_action =
  | Set of int
  | Try_set of string
  | Invalid
  | Increment
  | Decrement
  | Add_row
  | Row_action of int * row_action
  | Submit
  | Submit_result of string
  (* [@@deriving show] *)
]

let do_stuff x = Lwt_unix.sleep 2. >> Lwt.return (x ^ "++" ^ x)

[%%shared

let dummy_serv = ~%(Eliom_client.server_function [%derive.json: string] do_stuff)

let mk_row_view port =
  [%shared fun id row ->
    ROW.view (FRP.tag_port (fun a -> Row_action (id, a)) ~%port) row]

module TEST =
struct

  let make a =
    { a; valid = true; row_id = 1;
      rows = IM.add 0 (ROW.create 111) IM.empty;
      submitting = false; result = None;
    }

  let update (port : test_action FRP.port) m = function
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
          | row ->
            let row =
              ROW.update
                (FRP.tag_port (fun a -> Row_action (id, a)) port)
                row action
            in
                { m with rows = IM.add id row m.rows }
      end
    | Submit ->
        let sum = sprintf "%d" @@ IM.fold (fun _ { v; _ } s -> v + s) m.rows 0 in
          ignore [%client (
            let%lwt x = ~%dummy_serv ~%sum in
              FRP.push ~%port (Submit_result x) ();
              Lwt.return_unit
            : unit Lwt.t)
          ];
          { m with submitting = true }
    | Submit_result result -> { m with submitting = false; result = Some result }

  let view (port : test_action FRP.Types.port) m =
    let open Eliom_content.Html.D in
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
        ~a:[ R.a_value @@ S.map [%shared (fun m -> sprintf "(%d)" m.a)] m ] () in

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

          R.pcdata @@ S.map [%shared (fun m -> sprintf "value: %d" m.a)] m;

          br ();

          button ~a:[FRP.onclick port Decrement] [ pcdata "- " ];
          pcdata " ";
          button ~a:[FRP.onclick port Increment] [ pcdata "+ " ];
          br ();

          button
            ~a:[a_onclick [%client FRP.effect ~%port @@ FRP.focus ~%input_a]]
            [ pcdata "Focus A" ];
          pcdata " ";
          button
            ~a:[a_onclick [%client FRP.effect ~%port @@ FRP.focus ~%input_b]]
            [ pcdata "Focus B" ];

          br();

          button ~a:[FRP.onclick port Add_row] [ pcdata "ADD ROW" ];
          pcdata " ";
          pcdata "SUM: ";
          R.pcdata @@
            S.map [%shared string_of_int] @@
            S.map [%shared fun m -> IM.fold (fun _ { v; _ } s -> v + s) m.rows 0] m;

          br ();
          button
            ~a:[FRP.onclick port Submit;
                R.filter_attrib (a_disabled ()) @@
                S.map [%shared fun m -> m.submitting] m
               ]
            [ pcdata "SUBMIT" ];
          R.pcdata @@
            S.map
              [%shared function | { submitting = true; _ } -> " SUBMITTING" | _ -> ""]
              m;
          R.pcdata @@
            S.map
              [%shared
                function
                  | { result = None; _ } -> ""
                  | { result = Some s; _ } -> sprintf " result: %s" s]
              m;

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
    Eliom_content.Html.F.(body [
      h1 [pcdata "Welcome from Eliom's distillery!"];
      div [ FRP.make [%client TEST.update] TEST.view (TEST.make 42) ]
    ])
