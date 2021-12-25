(* Copyright 2010, Martin Elsman. MIT License *)

signature CONTEXT = sig
  type t

  datatype color = RGB of int * int * int
  val black : color
  val white : color

  datatype valign = Top | Bot | Middle
  datatype halign = Left | Center | Right

  type entry
  val stroke_width : int -> entry        (* not yet used by any backend *)
  val stroke_color : color -> entry
  val fill_color   : color -> entry      (* not yet used by any backend *)
  val text_valign  : valign -> entry
  val text_halign  : halign -> entry
  val empty        : t
  val add_entry    : entry -> t -> t

  val get_stroke_width : t -> int
  val get_stroke_color : t -> color
  val get_fill_color   : t -> color
  val get_text_valign  : t -> valign
  val get_text_halign  : t -> halign
end

signature OBJECT = sig
  structure C : CONTEXT
  type t
  type point = int * int

  val line      : point -> point -> t
  val rect      : point -> point -> t
  val polyline  : point list -> t
  val polygon   : point list -> t
  val all       : t list -> t
  val circle    : int -> t
  val arc       : int -> real -> t
  val text      : point -> string -> t    (* rotation and scale not supported for text *)

  (* Simple transformations *)
  val translate : point -> t -> t
  val rotate    : real -> t -> t
  val scale     : real * real -> t -> t
  val mirror    : point -> point -> t -> t     (* not supported yet *)

  val mirror_yaxis : t -> t

  (* Infix operators *)
  val >> : t * int -> t
  val << : t * int -> t
  val ^^ : t * int -> t
  val vv : t * int -> t
  val %> : t * real -> t
  val %^ : t * real -> t
  val @@ : t * real -> t
  val &  : t * t -> t
  val // : C.entry * t -> t

  (* logo style drawing *)
  datatype dir = N | S | E | W | AX | AY    (* relative and absolute directions *)
  type dist = int
  val logo : point -> (dir * dist) list -> t
  val logogon : point -> (dir * dist) list -> t
end

(*

[line p1 p2] returns an object representing a line from p1 to p2.
[rect p1 p2] returns an object representing a rectangle with p1 and p2 as opposite corners.
[polyline ps] returns an object representing multiple lines  : point list -> t
[polygon   : point list -> t
[all       : t list -> t
[circle    : int -> t
[arc       : int -> real -> t
[text      : point -> string -> t    (* rotation and scale not supported for text *)

[translate : point -> t -> t
[rotate    : real -> t -> t
[scale     : real * real -> t -> t
[mirror    : point -> point -> t -> t     (* not supported yet *)

[obj >> n] translates obj n units to the right.
[obj << n] translates obj n units to the left.
[obj ^^ n] translates obj n units up.
[obj vv n] translates obj n units down.
[obj %^ s] scales obj on the vertical axis by s.
[obj %> s] scales obj on the horizontal axis by s.
[obj @@ r] rotates obj by r with respect to origon.
[e // obj] use entry e when laying out obj.


*)

signature WIDGET = sig
  structure O : OBJECT
  val ruler : (string*string)option -> O.t
  val ruler2 : real -> (string*string)option -> O.t
  val measure : real -> int -> O.point -> O.point -> O.t
  val dotline : real -> O.point -> O.point -> O.t
end

signature PICTURE = sig
  type t
  structure O : OBJECT
  structure W : WIDGET
  sharing type O.t = W.O.t
  sharing O.C = W.O.C
  datatype length = MM of real | CM of real | PT of int
  val picture : {unitlength:length,
                 dim:int*int} -> O.t -> t
  val seq : t list -> t
  val export : t -> string  (* LaTeX picture environment code *)

  datatype size = A4 | A3
  val toFile : size -> string -> t -> unit
end
