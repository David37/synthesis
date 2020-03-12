module Synthesis

let abelar input =
    match input>12 && input<3097 && input%12=0 with
    | true -> true
    | _ -> false
    

let area b h =
    match h<0.0 with
    | true -> failwith "height cannot be a negative number"
    | _ -> match b<0.0 with
           | true -> failwith "base cannot be a negative number"
           | _ -> 0.5*b*h

let zollo _ =
    failwith "Not implemented"

let min _ _ =
    failwith "Not implemented"

let max _ _ =
    failwith "Not implemented"

let ofTime _ _ _ =
    failwith "Not implemented"

let toTime _ =
    failwith "Not implemented"

let digits _ =
    failwith "Not implemented"

let minmax _ =
    failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"