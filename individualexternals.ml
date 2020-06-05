
open Yojson.Basic.Util
open Http

(* Implement this method to combine your state and id *)

(* Implement this method to cleanup after each test *)
let afterTestcleanup =
    ignore(Http.rawpost ("http://localhost:3000/reset") "{\"todos\": [],\"comments\": [],\"users\": []}")