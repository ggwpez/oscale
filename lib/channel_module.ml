open Base
open Stdlib

(** Unidirectional channel for thread-safe communication. Can be closed by either side. *)
module Channel = struct
	open Eio

	exception Closed

	(** Read handle of a channel. *)
	type 'a reader = {
		mutable stream: 'a Stream.t;
		mutable close: unit Promise.u;
		mutable closed: unit Promise.t
	}

	(** Write handle of a channel. *)
	type 'a writer = {
		mutable stream: 'a Stream.t;
		mutable close: unit Promise.u;
		mutable closed: unit Promise.t
	}

	(** Close handler of a channel. Can only be constructed from a Writer. *)
	type 'a closer = C of 'a writer

	let of_stream stream : 'a reader * 'a writer =
		let closed, close = Promise.create () in
		({ stream; closed; close }, { stream; closed; close })
	
	let create cap =
		of_stream (Stream.create cap)
	
	let try_close (self: 'a closer) =
		let self = match self with
			| C writer -> writer
		in
		match Promise.try_resolve self.close () with
			| true -> Ok ()
			| false -> Error ()

	let closer_of_writer w : 'a closer = C w

	let close_exn (self: 'a closer) =
		match try_close self with
			| Ok () -> ()
			| Error () -> raise Closed
	
	let closew_exn (self: 'a writer) =
		close_exn (closer_of_writer self)

	(** Try to add a value and error if not possible. *)
	let try_add (self: 'a writer) v =
		Fiber.first
			(fun () ->
				Promise.await self.closed;
				Error ())
			(fun () ->
				Stream.add self.stream v;
				Ok ())
	
	(** Add a value and raise [Closed] if closed.*)
	let add_exn (self: 'a writer) v =
		match try_add self v with
			| Error () -> raise Closed
			| Ok () -> ()
	
	let try_read (self: 'a reader) =
		Fiber.first
			(fun () ->
				Ok (Stream.take self.stream))
			(fun () ->
				Promise.await self.closed;
				Error ())
	
	let read_exn (self : 'a reader) =
		match try_read self with
			| Error () -> raise Closed
			| Ok v -> v
end;;

(* Ensure that all remaining data can be read from a closed Channel. *)
let%test_unit "read_after_close_works" =
	Eio_main.run @@ fun _env ->
		let rc, wc = Channel.create 5 in
		
		Channel.add_exn wc 1;
		Channel.add_exn wc 2;
		Channel.add_exn wc 3;

		(* Note that OCaml does not allow us to do function overloading. We can therefore not just allow
			 to close a [writer], when we at the same time also want to have a dedicated [closer], since
			 that would necessitate a way to call the [close_exn] function on both types.

			 Instead, we have to use some enum together with an explicit ctor... If we use classes
			 instead, then we could inherit the virtual closer class for both: the closer and the writer
			 types. *)
		let cc = (Channel.closer_of_writer wc) in
		Channel.close_exn cc;

		assert(Channel.try_read rc = Ok 1);
		assert(Channel.try_read rc = Ok 2);
		assert(Channel.try_read rc = Ok 3);
		assert(Channel.try_read rc = Error ());
		;
