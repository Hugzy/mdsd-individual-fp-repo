open QCheck
open Yojson.Basic.Util
open Curl
open Format
open Http


let combine_state_id stateItem id =
	let sutJson = Yojson.Basic.from_string ("{\"id\": " ^ id ^ "}") in
    Yojson.Basic.Util.combine stateItem sutJson
	

(* Implement this method to extract your id from a json body *)
let extractIdFromContent (content:Yojson.Basic.t) : string =
	 string_of_int (content |> member "id" |> to_int)


module APIConf =
struct

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

  type sut = (comment list) ref
  type state = comment list
  
  type cmd =
  	 | Get of int
  	 | Post of comment
  	 | Del of int
  	 [@@deriving show { with_path = false }]
       
  let getURL="http://localhost:3000/todos/"
  let postURL="http://localhost:3000/todos/"
  let delURL="http://localhost:3000/todos/"
 
 

  let comment_generator = Gen.map ( fun () -> {id = Gen.generate1 Gen.string; 
                                                    title = Gen.generate1 Gen.string; 
                                                    author = Gen.generate1 Gen.string}) 
                                                Gen.unit;;

let thread_generator = Gen.map (fun () -> {id = Gen.generate1 Gen.string;
                                          title = Gen.generate1 Gen.string;
                                          comments = Gen.generate1 (Gen.list_size (Gen.return 3) comment_generator)}) Gen.unit;;

(*
  let thread_generator = Gen.map( fun ()  -> {id = Gen.generate1 Gen.string; 
                                                   title = Gen.generate1 Gen.string; 
                                                   comments = Gen.generate1 (Gen.list_size 2 comment_generator)}) 
                                                Gen.unit;;
*)

  let init_state = []
  let init_sut() = ref []
  let cleanup _  =  ignore(Http.rawpost ("http://localhost:3000/reset") "{\"todos\": [],\"comments\": [],\"users\": []}")
  
  (* Functions *)
  (* Recursively drops n heads of a list and returns the rest of the list *)
  let rec drop n h = if n == 0 then h else (drop (n-1) (match h with
      | a::b -> b
      | [] -> []))
  
  (* basically this https://en.wikipedia.org/wiki/De_Bruijn_index *)
  let lookupItem ix state = List.hd (drop (ix mod List.length state) (List.rev state))
  
  let lookupSutItem ix sut = List.hd (drop (ix mod List.length sut) (List.rev sut))
  
  let checkInvariant state sut = List.length state = List.length !sut
  
  let rec remove_item pos list = match (list, pos) with
    | ([], _) -> []
    | (head::tail, 0) -> tail
    | (head::tail,_) -> [head]@(remove_item (pos-1) tail)
  
  (* Wanting to get the index of the id back, length of list will always start at 1 for a given element but the first element is at index 0*)
  let getPos ix list = ((List.length list - 1) - (ix mod List.length list))
  
  let replaceElem pos list newelem = List.mapi (fun i x -> if i = pos then newelem else x) list
  
  let inSpace value state = List.mem (value) state
  
  let isEmpty state = (List.length state = 0)

  let arb_cmd state = 
    if state = [] then
      QCheck.make ~print:show_cmd
      (Gen.oneof [(Gen.map (fun c -> Post c) comment_generator)])
    else
      QCheck.make ~print:show_cmd
        (Gen.oneof [ Gen.map (fun i -> Get i) Gen.small_int;
  	      		  (Gen.map (fun c -> Post c) comment_generator);
  	      		  Gen.map (fun i -> Del i) Gen.small_int ])
 
  let next_state cmd state = match cmd with
  	| Get ix -> state
  	| Post c -> state@[c]
  	| Del ix -> state
 
  let run_cmd cmd state sut = match cmd with
  	| Get ix ->
    if (checkInvariant state sut) then 
    (* Lookup the id of the item that has been chosen to test *)
  	  let id = lookupSutItem ix !sut in
      (* Fetch that item from the api *)
  		let code,content = Http.get (getURL^"/"^id) in
          (* Fetch our control from the state *)
  	  let control = lookupItem ix state in
      (** Right now we've relied on comparing models using json within strings but how do you compare e.g records?*)
      (* Derive our model to json then cast to a string and compare to content *)
          String.compare (Yojson.Safe.to_string (comment_to_yojson control)) (Yojson.Basic.to_string content) == 1
          (*
          (let combined = combine_state_id stateJson id in
              (String.compare ("") (Yojson.Basic.to_string content) == 0)
            ) 
            *)
            && (code == 200)
            else false
  	| Post c ->
    if (checkInvariant state sut) then 
  	let code,content = Http.post postURL (Yojson.Safe.to_string (comment_to_yojson c)) in
  		  sut := !sut@[c.id]; 
        (code == 201) 
        else false
  	| Del ix ->
    if (checkInvariant state sut) then 
  	let id = lookupSutItem ix !sut in
  		let code,content = Http.delete (delURL^"/"^id) in
      (code == 200)
      else false
 
  let precond cmd state = match cmd with
      | Get ix -> (List.length state > 0)  
      	 && (isEmpty state = false)
      | Post c -> true 
      | Del ix -> (List.length state > 0)  
      	 && (isEmpty state = false)
 

 end
 
 
 module APItest = QCSTM.Make(APIConf)
 ;; 
 
 QCheck_runner.run_tests ~verbose:true
   [APItest.agree_test ~count:10 ~name:"Individual"]
