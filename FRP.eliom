[%%shared

open Eliom_content

module Types =
struct
  type effect = (unit -> unit) option

  type 'a action = 'a option

  type 'a port =
    P : (_ * (?step:React.step -> ('a action * effect) -> unit)) Eliom_client_value.t -> 'a port

  type 'a action_event =
      ('a action * effect) React.E.t * (?step:React.step -> ('a action * effect) -> unit)
end

include Types

let shared_eq () = [%shared (==)]
]

[%%client

let tag_port (f : 'a -> 'b) (push : 'b port) : 'a port =
  let P (ev, push) = push in

  let push ?step (a, e) = match a with
    | None -> push (None, e)
    | Some a -> push (Some (f a), e)
  in
    P (ev, push)

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
]


[%%server
let port x : _ port = P x

let tag_port (f : 'a -> 'b) (_ : 'b port) : 'a port =
  P [%client (React.E.create ())]
]

let make
    (update : ('a -> 'b -> 'a) Eliom_client_value.t)
    (view : 'b port -> 'a Eliom_shared.React.S.t -> 'c) (m0 : 'a) =
  let m, pm = Eliom_shared.React.S.create m0 in
  let ev    = [%client (React.E.create () : _ action_event)] in
    ignore [%client ( run ~%update (~%m, ~%pm) ~%ev : unit) ];
    view (port ev) m
[@@ warning "-22"]

[%%shared
let onclick (port : 'a port) (action : 'a) =
  Html.D.a_onclick [%client (push ~%port ~%action : _ -> unit)]
  (* TODO: how to disable warning? [@@ warning "-22"] doesn't work *)
]
