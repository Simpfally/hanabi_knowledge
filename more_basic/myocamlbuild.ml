open Ocamlbuild_plugin

let compile_errors grammar messages (witness : string list) target =
  rule
    "menhir/compile_errors"
    ~prod:target
    ~deps:([ grammar; messages ] @ witness)
    (fun env _ ->
      let grammar = env grammar in
      let tags = tags_of_pathname grammar ++ "ocaml" ++ "menhir" in
      Cmd (S [
(*       !Options.ocamlyacc; (* menhir *)*)
        A "menhir";
        T tags;
        P grammar;
        A "--compile-errors"; P (env messages);
        Sh ">"; Px (env target);
      ]))

let generic_compile_errors (check_completeness : bool) =
  compile_errors
(* sources: *)
    "%.mly" "%Messages.messages"
(* if present, this dependency forces a completeness check: *)
    (if check_completeness then [ "%Messages.witness" ] else [])
(* target: *)
        "%Messages.ml"


let () =
        dispatch begin function
                | After_rules ->
                generic_compile_errors false;
                | _ -> ()
        end
