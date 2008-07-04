(* 
   OCaml script that queries the JSON interface of Yahoo! Image Search,
   and displays the results (not very nicely, but you can improve this part).

   What you need to compile this program:
   - json-wheel
   - json-static
   - netclient

   1) Compile

     ocamlfind ocamlopt -o yahoo yahoo.ml -linkpkg \
        -package json-static,netclient -syntax camlp4o

   2) Run

     ./yahoo "Nelson Mandela"


   For more info on JSON and Yahoo! web services, go to
     http://developer.yahoo.com/common/json.html
*)

open Printf

type json search_results =
    < result_set "ResultSet": 
      < total_results_available "totalResultsAvailable": string;
        total_results_returned "totalResultsReturned": int;
        first_result_position "firstResultPosition": int;
	result "Result": item list > >

and item =
    < title "Title": string;
      summary "Summary": string;
      url "Url": string;
      click_url "ClickUrl": string;
      referer_url "RefererUrl": string;
      file_size "FileSize": int;
      file_format "FileFormat": string option;
      height "Height": string;
      width "Width": string;
      thumbnail "Thumbnail": thumbnail >

and thumbnail =
    < url "Url": string;
      height "Height": string;
      width "Width": string >


let query_url query =
  "http://api.search.yahoo.com/ImageSearchService/V1/imageSearch?\
   appid=YahooDemo&query=" ^ 
  Netencoding.Url.encode query  ^ "&output=json"

let search query =
  let url = query_url query in
  printf "From %s\n%!" url;
  let j=
    (Json_io.json_of_string
       (Http_client.Convenience.http_get url))
  in
  printf "Got the following JSON data:\n%s\n%!"
    (Json_io.string_of_json ~compact:false j);
    
  search_results_of_json j


let display obj =
  let x = obj#result_set in
  let start = x#first_result_position in
  printf "Showing results %i-%i of %s\n" 
    start (start + x#total_results_returned - 1) x#total_results_available;
  List.iter 
    (fun x -> printf "  %s\n" x#url)
    x#result

let _ =
  match Sys.argv with
      [| _; q |] -> display (search q)
    | _ -> failwith "Usage: yahoo \"your search query\""
