open Riot

exception Failed
type Message.t += Start | Ping of Pid.t | Pong | Die

let rec gen_server_helper () =
  (match receive_any () with
    | Ping from -> Printf.printf "Helper got ping: sending pong\n";
        send from Pong
    | Die -> Printf.printf "Child shutdown";
        shutdown ()
    | _ -> Printf.printf "Unknown message"
    );
    gen_server_helper ();;

let rec gen_server_main_loop helper =
  (match receive_any () with
    | Start -> Printf.printf "Main initating protocol\n";
      send helper (Ping (self ()))
    | Pong -> Printf.printf "Main got pong from child, sending shutdown\n";
      send helper Die;
    | _ -> Printf.printf "Unknown message"
  );
  gen_server_main_loop helper;;

let gen_server_main () =
  let helper = spawn gen_server_helper in
  monitor helper;

  gen_server_main_loop helper;;

let%test_unit "genserver" =
  Riot.run @@ fun () ->
  let main = spawn gen_server_main in

  monitor main;
  send main Start;

  match receive_any () with
  | _ -> Printf.printf "finished\n"
  ;
