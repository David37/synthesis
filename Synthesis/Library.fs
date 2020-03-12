module Synthesis

let abelar input =
    input>12 && input<3097 && input%12=0 

    

let area b h =
    match h<0.0 && b<0.0 with
    | true -> failwith "height cannot be a negative number"
    | _ -> 0.5*b*h

let zollo input =
    match input<0 with
    | true -> -1*input
    | _ -> input*2

let min a b =
    match a>b with
    | true -> b
    | _ -> a

let max a b =
    match a>b with
    | true -> a
    | _ -> b


let ofTime h m s =
    (h*3600)+(m*60)+s

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