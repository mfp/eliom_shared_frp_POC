[%%client
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
]
