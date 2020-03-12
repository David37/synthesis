module Synthesis

let abelar input =
    input>12 && input<3097 && input%12=0 

    

let area b h =
    match (h<0.0 || b<0.0) with
    | true -> failwith "height cannot be a negative number"
    | false -> 0.5*b*h

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

let toTime s =
    match s<0 with
    | true -> (0,0,0)
    | _ -> let h= s/3600
           let m= s%3600/60
           let s= s%3600%60
           (h,m,s)

let digits n =
    let rec getDigits count numDigits=
        match (count < 10 && count > -10) with
        | true -> numDigits
        | false -> getDigits (count/10) (numDigits+1)
    getDigits  n 1


let minmax _ =
   failwith "not inmplemented"

let isLeap y =
    match y<1582 with 
    | true -> failwith "Year cannot be less than 1852"
    | false -> match y%4=0 with
            | true -> match y%100=0 with 
                    | true -> match y%400=0 with 
                            | true -> true
                            | false -> false
                    | false -> true
            | false -> false

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