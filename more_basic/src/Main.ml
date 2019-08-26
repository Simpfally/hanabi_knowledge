open Printf

let _ =
let record =
  let filename =
    match Array.length Sys.argv with
      | 2 -> Sys.argv.(1)
      | _ -> "-" in
  Parse.from_file filename in
printf "%s" (Hanabi_types.print record);
printf "\n\nPairs :\n%s" (Kbd.analyse record)
  
  (*let e = try Parse.from_file filename with
    | _ -> (((1,1), []), [], [])*)
