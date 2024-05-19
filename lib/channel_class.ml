open Eio

exception Closed

(** Channel error type. It is set to the Exception type [exn] to allow `raising` the error that was
  returnd by a function. *)
type error = exn;;

(** Can read data. *)
class virtual ['a] reader = object(self)
  (** Try to read one value. *)
  method virtual read : ('a, error) result

  (** Read or raise an exception. *)
  method read_exn : 'a =
    match self#read with
      | Ok v -> v
      | Error e -> raise e
end

(** Can write data. *)
class virtual ['a] writer = object(self)
  (** Try to write one value. *)
  method virtual write : 'a -> (unit, error) result

  (** Read or raise an exception. *)
  method write_exn v=
    match self#write v with
      | Ok () -> ()
      | Error e -> raise e
end

(** Can close something. *)
class virtual ['a] closer = object(self)
  (** Try to close a channel. *)
  method virtual close : (unit, error) result

  (** Close or raise an exception. *)
  method close_exn =
    match self#close with
      | Ok () -> ()
      | Error e -> raise e
end

(** Inner channel data. *)
type 'a inner = {
  mutable stream : 'a Stream.t;
  mutable closed : unit Promise.t;
  mutable close : unit Promise.u
}

(** A full channel. It implements the [reader], [writer] and [closer] interfaces. *)
class ['a] channel inner = object
  inherit ['a] reader
  inherit ['a] writer
  inherit ['a] closer

  (** Note that this class *does not* have any inner state! It just uses its constructur argument
      [inner] throughout the implementation. It is possible to add some inner state like this:

      val i : 'a inner = inner

      But somehow we dont *need* to do that. Fine by me ¯\_(ツ)_/¯ *)

  method read =
    Fiber.first
      (* The order matters! Check if its closed LAST. *)
      (fun () ->
        Ok (Stream.take inner.stream))
      (fun () ->
        Promise.await inner.closed;
        Error Closed)
  
  method write v =
    Fiber.first
      (* The order matters! Check if its closed FIRST. *)
      (fun () ->
        Promise.await inner.closed;
        Error Closed)
      (fun () ->
        Stream.add inner.stream v;
        Ok ())

  method close =
    match Promise.try_resolve inner.close () with
    | true -> Ok ()
    | false -> Error Closed
end

(** Partial handle that can only be used to read something.

    This is good for cases where we want a read-only channel. *)
class ['a] read_handle (inner: 'a reader) = object
  inherit ['a] reader

  method read = inner#read
end

(** Partial handle that can only be used to write something.

    This is good for cases where we want a write-only channel. *)
class ['a] write_handle (inner: 'a writer) = object
  inherit ['a] writer

  method write = inner#write
end

(** Partial handle that can only be used to close something.*)
class ['a] close_handle (inner: 'a closer) = object
  inherit ['a] closer

  method close = inner#close
end

(** Construct a [read_handle] from a [reader] (eg a channel).*)
let of_reader r = new read_handle (r :> 'a reader)

(** Construct a [write_handle] from a [writer] (eg a channel).*)
let of_writer w = new write_handle (w :> 'a writer)

(** Construct a [close_handle] from a [closer] (eg a channel).*)
let of_closer c = new close_handle (c :> 'a closer)

(** Create a channel with a specific capacity. *)
let of_capacity cap =
  let stream = Stream.create cap in
  let closed, close = Promise.create () in
  new channel { stream; closed; close }

(** Create a channel with an unspecific capacity. *)
let create () = of_capacity 16

let%test_unit "read_after_close_works" =
  Eio_main.run @@ fun _ ->
    let chan = create () in
    
    (* Write directly into the channel: *)
    chan#write_exn 1;
    (* Or into a less-permissioned write handle: *)
    (of_writer chan)#write_exn 2;

    chan#close_exn;

    assert(chan#read = Ok 1);
    assert(chan#read = Ok 2);
    assert(chan#read = Error Closed);
    ;
