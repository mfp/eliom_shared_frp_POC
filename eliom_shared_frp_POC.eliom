[%%shared

open Eliom_lib
open Eliom_content
open Html.D

module FRP_types =
struct
  type effect = (unit -> unit) option

  type 'a action = 'a option

  type 'a port =
    P : (_ * (?step:React.step -> ('a action * effect) -> unit)) Eliom_client_value.t -> 'a port

  type 'a action_event =
      ('a action * effect) React.E.t * (?step:React.step -> ('a action * effect) -> unit)
end

open FRP_types
]

[%%client
module Shared_react =
struct
  type 'a signal = 'a Lwt_react.signal
  type 'a event  = 'a Lwt_react.event

  module E = Lwt_react.E

  module S =
  struct
    include Lwt_react.S

    let keep_value s =
      let push v = function
        | Some _ as x -> x
        | None -> Some v in

      let define d =
        let d' = l2 push s d in
          (d', d')
      in
        map ~eq:(==)
          (function
             | Some x -> x
             | None ->
                 !Lwt.async_exception_hook
                   (Failure "React_map.keep_value: None unexpected");
                 assert false) @@
        fix ~eq:(==) None define

    (* Issue: internal signals/events that depend on the supplied signal
     * are not stopped, causing a leak in absence of weak refs / GC finalizers
     * (i.e., under js_of_ocaml) *)
    let to_rlist s =
      let init, push = E.create () in
      let needs_init = ref true in
        ignore begin
          Lwt.pause () >>
          Lwt.catch
            (fun () ->
               if not !needs_init then
                 Lwt.return ()
               else begin
                 needs_init := false;
                 Lwt.return @@ push @@ value s
               end)
            (fun _ -> Lwt.return ())
        end;
        ReactiveData.RList.from_event (try value s with _ -> []) @@
        E.map (fun l -> needs_init := false; ReactiveData.RList.Set l) @@
        E.select [ changes s; init ]

    let space_safe_switch ?eq ss =
      let s = switch ?eq ss in
      let e = diff (fun _ old -> stop ~strong:true old) ss in
        l2 ~eq:(==) (fun s () -> s) s (hold () e)
  end
end
]

[%%server
module ReactMap(O : Map.OrderedType) :
sig
  include module type of Map.Make(O)
  module React :
  sig
    val map :
      ?eq:('a -> 'a -> bool) Eliom_shared.Value.t ->
      (O.t -> 'a Eliom_shared.React.S.t -> 'b) Eliom_shared.Value.t ->
      'a t Eliom_shared.React.S.t -> 'b t Eliom_shared.React.S.t
  end
end =
struct
  module M = Map.Make(O)
  include M

  module ESV = Eliom_shared.Value
  module ERS = Eliom_shared.React.S

  module React =
  struct
    let map ?eq f s =
      ERS.const @@
      M.fold
        (fun k v m -> M.add k (ESV.local f k (ERS.const v)) m)
        (ESV.local @@ ERS.value s) M.empty
  end
end
]

[%%client
module ReactMap(O : Map.OrderedType) :
sig
  include module type of Map.Make(O)
  module React :
  sig
    val merge : 'a React.signal t -> 'a t React.signal

    val map_s :
      ?eq:('a -> 'a -> bool) ->
      (O.t -> 'a React.signal -> 'b React.signal) ->
      'a t React.signal -> 'b t React.signal

    val map :
      ?eq:('a -> 'a -> bool) ->
      (O.t -> 'a React.signal -> 'b) ->
      'a t React.signal -> 'b t React.signal
  end
end =
struct
  include Map.Make(O)

  module SSET = Set.Make(O)

  module React =
  struct
    open React
    open Shared_react

    let merge m =
      let changes =
        fold
          (fun k v l ->
             let e = S.changes v |> E.map (fun x -> (k, x)) in
               e :: l)
          m [] |>
        E.merge (fun l e -> e :: l) [] in

      let init =
        S.map ~eq:(==)
          (fun l -> List.fold_left (fun m (k, v) -> add k v m) empty l) @@
        S.merge ~eq:(==) (fun l x -> x :: l) [] @@
        fold (fun k v l -> S.Pair.pair (S.const k) (S.keep_value v) :: l)
          m []
      in
        S.space_safe_switch ~eq:(==) @@
        S.map ~eq:(==)
          (fun init ->
             S.fold ~eq:(==)
               (fun m l -> List.fold_left (fun m (k, v) -> add k v m) m l)
               init
               changes) @@
          init

    let keys m = fold (fun k _ l -> k :: l) m []

    let map_s ?eq f m =
      let key_changes =
        S.diff
          (fun m2 m1 ->
             let b1      = SSET.of_list @@ keys m1 in
             let b2      = SSET.of_list @@ keys m2 in
             let deleted = SSET.elements @@ SSET.diff b1 b2 in
             let added   = SSET.elements @@ SSET.diff b2 b1 in
               (added, deleted)) @@
          m in

      let mk k =
        f k @@
        S.space_safe_switch ?eq @@
        S.map ~eq:(==)
          (fun init ->
             S.fmap ?eq
               (fun m -> try Some (find k m) with Not_found -> None)
               (find k init)
               m) @@
        S.keep_value m in

      let init =
        S.map ~eq:(==)
          (fun m -> fold (fun k v m -> add k (mk k) m) m empty) @@
        S.keep_value m
      in

        S.space_safe_switch ~eq:(==) @@
        S.map ~eq:(==) merge @@
        S.space_safe_switch ~eq:(==) @@
        S.map
          (fun init ->
             S.fold ~eq:(==)
               (fun m (added, deleted) ->
                  List.iter (fun k -> S.stop ~strong:true @@ find k m) deleted;
                  let m = List.fold_left (fun m k -> remove k m) m deleted in
                  let m = List.fold_left (fun m k -> add k (mk k) m) m added in
                    m)
               init
               key_changes)
          init

    let map ?eq f m =
      let key_changes =
        S.diff
          (fun m2 m1 ->
             let b1      = SSET.of_list @@ keys m1 in
             let b2      = SSET.of_list @@ keys m2 in
             let deleted = SSET.elements @@ SSET.diff b1 b2 in
             let added   = SSET.elements @@ SSET.diff b2 b1 in
               (added, deleted)) @@
          m in

      let mk k =
        let m =
          S.space_safe_switch ?eq @@
          S.map ~eq:(==)
            (fun init ->
               S.fmap ?eq
                 (fun m -> try Some (find k m) with Not_found -> None)
                 (find k init)
                 m) @@
          S.keep_value m
        in
          (f k m, m) in

      let signal init =
        S.fold ~eq:(==)
          (fun (m, m_) (added, deleted) ->
             List.iter
               (fun k ->
                  try S.stop ~strong:true @@ snd @@ find k m; with Not_found -> ())
               deleted;
             let m     = List.fold_left (fun m k -> remove k m) m deleted in
             let m_    = List.fold_left (fun m k -> remove k m) m_ deleted in
             let added = List.map (fun k -> (k, mk k)) added in
             let m     = List.fold_left (fun m (k, v) -> add k v m) m added in
             let m_    = List.fold_left (fun m (k, (v, _)) -> add k v m) m_ added in
               (m, m_))
          (init, map fst init)
          key_changes
      in
        S.map ~eq:(==) snd @@
        S.space_safe_switch ~eq:(==) @@
        S.map ~eq:(==) signal @@
        S.map ~eq:(==) (fun m -> fold (fun k v m -> add k (mk k) m) m empty) @@
        S.keep_value m
  end
end
]

[%%client
module FRP =
struct
  let run ?pp_action update (m, (pm : ?step:React.step -> 'a -> unit)) (ev, _) : unit =
    ignore @@
      Eliom_shared.React.E.map
        (fun (action, effect) ->
           begin match action, pp_action with
             | None, _ | _, None -> ()
             | Some a, Some pp -> Firebug.console##log(Js.string @@ pp a)
           end;
           begin match action with
             | None -> ()
             | Some action -> pm @@ update (Eliom_shared.React.S.value m) action
           end;
           begin match effect with
             | None -> ()
             | Some f -> f ()
           end)
        ev;
    ()

  let push ?effect (P (_, (p : ?step:React.step -> _ -> unit))) x _ = p (Some x, effect)

  let effect (P (_, (p : ?step:React.step -> _ -> unit))) effect _ = p (None, Some effect)

  let push_delayed (P (_, (p : ?step:React.step -> _ -> unit))) f _ = p @@ f ()

  let focus inp () = (Html.To_dom.of_input inp)##focus

  let on ev f inp port =
    let open Lwt_js_events in
    let inp = Html.To_dom.of_input inp in
      ignore @@
      async
        (fun () ->
           ev inp
             (fun _ _ ->
                Lwt.return @@
                try
                  let v, effect = f @@ Js.to_string inp##.value in
                    push ?effect port v ()
                with _ -> () (* TODO: logging *)))

  let on_keyups f inp port = on Lwt_js_events.keyups f inp port

  let fpush_keyups f inp port =
    let open Lwt_js_events in
    let inp = Html.To_dom.of_input inp in
      async
        (fun () ->
           keyups inp
             (fun _ _ ->
                Lwt.return @@
                match f @@ Js.to_string inp##.value with
                  | None -> ()
                  | exception exn -> () (* TODO: logging *)
                  | Some (v, effect) -> push ?effect port v ()))
end
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

  let view (port : row_action FRP_types.port) m =
    let inp =
      Raw.input
        ~a:[ Html.R.a_value @@
               Eliom_shared.React.S.map [%shared fun m -> string_of_int m.v] m;
             Html.R.a_style @@
               Eliom_shared.React.S.map
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

module IM = ReactMap(struct type t = int let compare = compare end)

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

[%%client
let tag_row_port id (push : test_action FRP_types.port) : row_action FRP_types.port =
  let P (ev, push) = push in

  let push ?step (a, e) = match a with
    | None -> push (None, e)
    | Some a -> push (Some (Row_action (id, a)), e)
  in
    P (ev, push)
]

[%%server
let tag_row_port id (p : test_action FRP_types.port) : row_action FRP_types.port =
  P [%client (React.E.create ())]
]

[%%shared

let shared_eq = [%shared (==)]

let mk_row_view port =
  [%shared fun id row -> ROW.view (tag_row_port id ~%port) row]

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

  let view (port : test_action FRP_types.port) m =
    let input_a =
      Raw.input
        ~a:[ Html.R.a_value @@
               Eliom_shared.React.S.map [%shared (fun m -> string_of_int m.a)] m;
             Html.R.a_style @@
               Eliom_shared.React.S.map
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
        ~a:[ Html.R.a_value @@
             Eliom_shared.React.S.map
               [%shared (fun m -> string_of_int @@ 2 * m.a)] m
        ]
        () in

    let rows =
      Html.R.div @@
      Eliom_shared.ReactiveData.RList.from_signal @@
      Eliom_shared.React.S.map
        [%shared fun rows -> List.rev @@ IM.fold (fun _ v l -> v :: l) rows []] @@
      Eliom_shared.React.S.switch @@
      Eliom_shared.React.S.map
        [%shared fun m -> IM.React.map ~eq:shared_eq (mk_row_view ~%port) m] @@
      Eliom_shared.React.S.const @@
      Eliom_shared.React.S.map ~eq:[%shared (==)] [%shared fun m -> m.rows] m

    in

      div
        [
          input_a; br ();
          input_b; br ();

          Html.R.pcdata @@
            Eliom_shared.React.S.map [%shared (fun m -> Printf.sprintf "value: %d" m.a)] m;

          br ();

          button
            ~a:[a_onclick [%client FRP.push ~%port Decrement]]
            [ pcdata "- " ];
          pcdata " ";
          button
            ~a:[a_onclick [%client FRP.push ~%port Increment]]
            [ pcdata "+ " ];
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

          button ~a:[a_onclick [%client FRP.push ~%port Add_row]]
            [ pcdata "ADD ROW" ];
          pcdata " ";
          pcdata "SUM: ";
          Html.R.pcdata @@
            Eliom_shared.React.S.map [%shared string_of_int] @@
            Eliom_shared.React.S.map
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
        let m, pm = (Eliom_shared.React.S.create @@ TEST.make 42) in
        let ev    = [%client (React.E.create () : test_action FRP_types.action_event)] in
          ignore [%client ( FRP.run (*~pp_action:show_test_action *)
                              TEST.update (~%m, ~%pm) ~%ev : unit) ];
          TEST.view (P ev) m
      ]
    ])
