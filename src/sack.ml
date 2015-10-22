open Core.Std
open Core_extended.Std

module Sack = struct
  let shortcuts_file = ".sack_shortcuts"

  module Samples = struct
    let tuple_line =
      ("/Users/zph/src/ocaml/sack/pt.log", "59",
       "sack/sack.go:7:var debug = Debug(\"sack\")")

    let raw_line =
      "sack/pt.log:1:src/install:4:readonly BASE_URL="

    let sack_shortcuts_line =
      "[0]:/Users/zph/src/ocaml/sack/pt.log:59:sack/sack.go:7:var debug = Debug(\"sack\")"
  end

  module Shortcut = struct
    let home =
      match Sys.getenv "HOME" with
      | None -> "~"
      | Some x -> x
    let shortcuts = String.concat ~sep:"/" [home; shortcuts_file]
    let shortcuts_debug =
      String.concat ~sep:"/" [home; "src/ocaml/sack"; shortcuts_file]

    let to_tuple line =
      let f = String.split ~on:':' in
      let ix :: filename :: line_no :: content = f line in
      let cont = String.concat ~sep:":" content in
      (ix, filename, line_no, cont)

    let lines path =
      In_channel.read_lines path
               |> List.map ~f:to_tuple

    let to_vim_fragment index =
      let line =
        match List.nth (lines shortcuts) index with
        | None -> ("","","","")
        | Some x -> x
      in
      let (_, f, i, _) = line in
      sprintf "-c 'tabe +%s %s'" i f

    let full_vim_cmd ix =
      let subs = List.map ~f:to_vim_fragment ix
                 |> String.concat ~sep:" "
      in
      (* tabclose closes the first tab *)
      sprintf "vim %s -c 'tabclose 1'" subs
  end

  module Color = struct
    (* https://github.com/UnixJunkie/dolog/blob/master/lib/log.ml *)
    (* Copyright (c) 2014, INRIA.
     * Copyright (c) 2013, Zhang Initiative Research Unit,
     * Advance Science Institute, RIKEN
     * 2-1 Hirosawa, Wako, Saitama 351-0198, Japan
     * All rights reserved.
     *
     * Redistribution and use in source and binary forms, with or without
     * modification, are permitted provided that the following conditions
     * are met:
     *
     * Redistributions of source code must retain the above copyright notice,
     * this list of conditions and the following disclaimer.
     * Redistributions in binary form must reproduce the above copyright notice,
     * this list of conditions and the following disclaimer in the documentation
     * and/or other materials provided with the distribution.
     *
     * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
     * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
     * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
     * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
     * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
     * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
     * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
     * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
     * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
     * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
     * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. *)

    type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
               | Default

    (* ANSI terminal colors for UNIX *)
    let color_to_string = function
      | Black   -> "\027[30m"
      | Red     -> "\027[31m"
      | Green   -> "\027[32m"
      | Yellow  -> "\027[33m"
      | Blue    -> "\027[34m"
      | Magenta -> "\027[35m"
      | Cyan    -> "\027[36m"
      | White   -> "\027[37m"
      | Default -> "\027[39m"

    let reset = "\027[0m"

    let colorize color str =
      (color_to_string color) ^ str ^ (reset)
  end

  module Line = struct
    open Color

    let line_to_tuple line =
      let f = String.lsplit2_exn ~on:':' in
      let (filename, rest) = f line in
      let (line_no, txt)   = f rest in
      (filename, line_no, txt)

    let formatted_line index (file, lineno, txt) =
      let i = colorize Red (sprintf "%i" index) in
      let f = colorize Green file in
      let l = colorize Yellow lineno in
      sprintf "[%s] %s %s %s" i f l txt

    let colon_sep_line index (file, lineno, txt) =
      sprintf "[%i]:%s:%s:%s" index file lineno txt

    let output_lines f lines =
      List.mapi ~f:f lines

    let space_separated_output_lines lines =
      output_lines formatted_line lines

    let colon_separated_output_lines lines =
      output_lines colon_sep_line lines
  end

  module Search = struct
    let search_tool = "pt"

    let execute (term : string) =
        let cwd = Sys.getcwd() in
        Shell.run_lines search_tool ["--ignore-case"; "--"; term; cwd]
        |> List.map ~f:Line.line_to_tuple

    let format_and_join fn_format lines =
      lines
      |> fn_format
      |> String.concat ~sep:"\n"

    let write_to_console results =
      let console_output = format_and_join Line.space_separated_output_lines results in
      console_output
      |> print_string
      |> print_newline

    let write_to_file results =
      let sack_shortcuts = results
                           |> Line.colon_separated_output_lines
                           |> String.concat ~sep:"\n"
      in
      Out_channel.write_all shortcuts_file sack_shortcuts

    let to_output term =
      let results = execute term in
      write_to_console results;
      write_to_file results;;
  end

  module CLI = struct
    let search = ref false
    let edit = ref false
    let shell_init = ref false
    let args = ref ""
    let shell =
"
S(){
  # S search_term
  sack -s -- \"$*\"
}
F(){
  # F 1 2 3
  local result=$(sack -e -- \"$*\")
  eval \"${result}\"
}"

    let args_to_int a =
      String.split ~on:' ' a
      |> List.map ~f:int_of_string

    let main =
      let speclist = [("-s", Arg.Set search, "Puts Sack in Search Mode");
                      ("-e", Arg.Set edit, "Edit mode");
                      ("--init", Arg.Set shell_init, "Shell init");
                      ("--", Arg.Rest (fun x -> args := x), "Catch the rest of args");
                     ] in
      let usage_msg = "Usage: " in
      Arg.parse speclist print_endline usage_msg;
      match (!search, !edit, !shell_init, !args) with
      | (_,_,true,_) -> print_endline shell;
      | (_,_, _, "") -> print_endline usage_msg;
      | (true, _,_, _) -> (Search.to_output !args);
      | (_, true,_, _) -> print_endline (Shortcut.full_vim_cmd (args_to_int !args));
      | _ -> print_endline usage_msg;;
  end
end

let main () =
  print_string (Sys.getcwd ());
  print_string (String.concat ~sep:" " (Shell.run_lines "pwd" []));
  Sack.CLI.main
