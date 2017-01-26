
[%%server
module Make(O : Map.OrderedType) :
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
module Make(O : Map.OrderedType) :
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
