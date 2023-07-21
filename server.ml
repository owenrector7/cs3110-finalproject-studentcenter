open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson

(** INSTALLATION:

    opam install cohttp-lwt-unix cohttp-async brew install openssl opam install
    ssl opam install lwt_ssl *)

let json_body uri =
  Client.get (Uri.of_string uri) >>= fun (_resp, body) ->
  (* here you could check the headers or the response status
   * and deal with errors, see the example on cohttp repo README *)
  body |> Cohttp_lwt.Body.to_string >|= Yojson.Basic.from_string

(* In utop the promise is realised immediately, so if you run the
 * function above, you'll get the result immediately *)

let json_from_uri uri = json_body uri |> Lwt_main.run
