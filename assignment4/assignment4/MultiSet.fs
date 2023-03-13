module MultiSet
    type MultiSet<'a when 'a: comparison> = MS of Map<'a, uint32>
    let empty = MS Map.empty
    let isEmpty (MS set) = Map.isEmpty set
    let size (MS set) = Map.fold (fun acc _ value -> acc + value ) 0u set
    let contains item (MS set) = Map.containsKey item set
//    let numItems item (MS set) = Map.fold(fun acc key _ -> if key = item then acc+1u else acc  ) 0u set

    let numItems item (MS set) =
        match set.TryFind item with
        | res when res.IsSome -> res.Value
        | _ -> 0u

    let add key value (MS set) =
        match set.TryFind key with
        | x when x.IsSome -> MS(set.Add(key,x.Value + value))
        | _ -> MS(set.Add(key,value))

    let addSingle key (MS set) = add key 1u (MS set)

    let remove key value (MS set) =
        match set.TryFind key with
        | x when x.IsSome -> if x.Value < value then MS(set.Remove(key)) else MS(set.Add(key,x.Value-value))
        | _ -> MS(set)

    let removeSingle key (MS set) = remove key 1u (MS set)

    let fold f acc (MS set) = Map.fold f acc set

    let foldBack f (MS set) acc = Map.foldBack f set acc
