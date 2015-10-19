open Core.Std
open Core_extended.Std

module Sack = struct
  module Samples = struct
    let tuple_line =
      ("/Users/zph/src/ocaml/sack/pt.log", "59",
       "sack/sack.go:7:var debug = Debug(\"sack\")")

    let raw_line =
      "sack/pt.log:1:src/install:4:readonly BASE_URL=\"https://github.com/zph/go-sack/releases/download/$VERSION"

    let sack_shortcuts_line =
      "[0]:/Users/zph/src/ocaml/sack/pt.log:59:sack/sack.go:7:var debug = Debug(\"sack\")"

  end

  module Shortcut = struct
    let lines () =
      let home =
        match Sys.getenv "HOME" with
        | None -> "~"
        | Some x -> x
      in
      let path = String.concat ~sep:"/" [home;".sack_shortcuts"] in
      In_channel.read_all path

    let to_tuple line =
      let f = String.split ~on:':' in
      let ix :: filename :: line_no :: content = f line in
      let cont = String.concat ~sep:":" content in
      (ix, filename, line_no, cont)
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

    let print_indexed_lines lx =
      space_separated_output_lines lx
      |> String.concat ~sep:"\n"
      |> print_string
  end

  module Search = struct
    let execute (term : string) =
        let cwd = Sys.getcwd() in
        Shell.run_lines "pt" ["--ignore-case"; "--"; term; cwd]
        |> List.map ~f:Line.line_to_tuple

    let append_char s c = s ^ String.make 1 c

    let format_and_join fn_format lines =
      lines
      |> fn_format
      |> String.concat ~sep:"\n"

    let write_to_console results =
      let console_output = format_and_join Line.space_separated_output_lines results in
      console_output |> print_string

    let write_to_file results =
      let sack_shortcuts = results
                           |> Line.colon_separated_output_lines
                           |> String.concat ~sep:"\n"
      in
      Out_channel.write_all ".sack_shortcuts" sack_shortcuts

    let to_output term () =
      let results = execute term in
      write_to_console results;;

      (*
      Line.print_indexed_lines results *)

  end

  module CLI = struct
    let command =
      Command.basic
        ~summary:"Search for something"
        ~readme: (fun () -> "Detailed information")
        Command.Spec.(empty +> anon ("search_term" %: string))
        Search.to_output
  end
end

(*
let () =
  Command.run ~version:"0.1" ~build_info:"RWO" Sack.CLI.command
 *)
