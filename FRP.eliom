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