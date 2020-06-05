open QCheck
open Yojson


type comment = {
  id: string;
  title: string;
  author: string;
} [@@deriving show, yojson];;

type thread = {
  id: string;
  title: string;
  comments: comment list;
} [@@deriving show, yojson];;

(*  let gens1 = Gen.generate1 Gen.string;;

let test_comment_generator = Gen.map ( fun () -> {id = Gen.generate1 Gen.string; title = Gen.generate1 Gen.string; author = Gen.generate1 Gen.string}) Gen.unit;;

let test_thread_generator = Gen.map( fun ()  -> {id = Gen.generate1 Gen.string; title = Gen.generate1 Gen.string; comments = Gen.generate1 (Gen.list_size (Gen.return 2) test_comment_generator)}) Gen.unit;;

*)
(*Gen.map3 (fun x y z -> {id ; title = y; author = z}) 
                                    (Gen.string_readable) 
                                    (Gen.string_readable) 
                                    (Gen.string_readable);;

*)
(*  Gen.generate ~n:1 test_thread_generator*)

let c = {id = "id1"; title= "title"; author="author"};;
let c1 = {id = "id"; title= "title"; author="author"};;

print_string "Comparison is";
print_int (compare c c1);;
print_newline;;

print_endline (Yojson.Safe.to_string (comment_to_yojson c));;
(*  QCheck_runner.run_tests ~verbose:true[] *)
