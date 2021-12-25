(* Copyright 2010, Martin Elsman. MIT License *)

structure Context :> CONTEXT = struct
  datatype color = RGB of int * int * int
  datatype valign = Top | Bot | Middle
  datatype halign = Left | Center | Right
  datatype entry =
           stroke_width of int
         | stroke_color of color
         | fill_color of color
         | text_valign of valign
         | text_halign of halign
  type t = entry list
  val black = RGB(0,0,0)
  val white = RGB(255,255,255)
  fun add_entry x xs = x::xs
  val empty : t = nil

  val rec get_stroke_width =
   fn stroke_width n :: rest => n
    | _ :: rest => get_stroke_width rest
    | nil => 1

  val rec get_stroke_color =
   fn stroke_color c :: rest => c
    | _ :: rest => get_stroke_color rest
    | nil => black

  val rec get_fill_color =
   fn fill_color c :: rest => c
    | _ :: rest => get_fill_color rest
    | nil => white

  val rec get_text_valign =
   fn text_valign a :: rest => a
    | _ :: rest => get_text_valign rest
    | nil => Middle

  val rec get_text_halign =
   fn text_halign a :: rest => a
    | _ :: rest => get_text_halign rest
    | nil => Center
end

structure Object = struct
  structure C = Context
  type point = int * int
  type rpoint = real * real
  val scaling = 10.0
  fun toPoint(x,y) = (scaling * x, scaling * y)
  fun toRpoint (x,y) = toPoint(real x, real y)
  datatype t0 = Line of rpoint * rpoint
              | Polyline of rpoint list
              | Polygon of rpoint list
              | All of obj list
              | Text of rpoint * string
              | Qbezier of rpoint * rpoint * rpoint
  withtype obj = C.t * t0
  type t = C.t -> obj

  fun line p1 p2 c = (c, Line (toRpoint p1,toRpoint p2))
  fun rect p1 p2 c =
      let val p1 as (x1,y1) = toRpoint p1
          val p2 as (x2,y2) = toRpoint p2
      in (c, Polygon [p1,(x2,y1),p2,(x1,y2)])
      end
  fun polyline ps c = (c, Polyline (map toRpoint ps))
  fun polygon ps c = (c, Polygon (map toRpoint ps))
  fun all (ts: t list) c = (c, All (map (fn t => t c) ts))
  fun text p s c = (c, Text(toRpoint p,s))
  fun qbezier p1 p2 p3 c = (c,Qbezier(toRpoint p1,toRpoint p2,toRpoint p3))

  local
    fun transl f (c,t0) =
        case t0 of
          Line(p1,p2) => (c, Line(f p1, f p2))
        | Polyline ps => (c, Polyline (map f ps))
        | Polygon ps => (c, Polygon (map f ps))
        | All ts => (c, All (map (transl f) ts))
        | Text(p,s) => (c, Text (f p,s))
        | Qbezier(p1,p2,p3) => (c, Qbezier(f p1,f p2,f p3))
  in
    fun translate p t c =
        let val (t_x, t_y) = toRpoint p
            fun t_p (x,y) = (x + t_x, y + t_y)
        in transl t_p (t c)
        end
    fun rotate theta t c =
        let fun r (x,y) = (x * Math.cos theta - y * Math.sin theta,
                           y * Math.cos theta + x * Math.sin theta)
        in transl r (t c)
        end
    fun scale (xs,ys) t c =
        let fun s (x,y) = (xs * x, ys * y)
        in transl s (t c)
        end
    fun mirror p1 p2 k c =
        let val (t_x,t_y) = toRpoint p1
            val (l_x,l_y) = toRpoint p2
            fun sq x = x * x
            val (l_x,l_y) = (l_x - t_x, l_y - t_y)
            val d = sq l_x + sq l_y
            fun f (x,y) =
                ((x * (2.0 * l_x * l_y) + y * (sq l_x - sq l_y)) / d,
                 (x * (sq l_y - sq l_x) + y * (2.0 * l_x * l_y)) / d)
            fun t (x,y) = (x - t_x, y - t_y)
            fun t' (x,y) = (x + t_x, y + t_y)
        in transl (t' o f o t) (k c)
        end

    fun mirror_yaxis k c = transl (fn (x,y) => (~x,y)) (k c)
    fun mirror_xaxis k c = transl (fn (x,y) => (x,~y)) (k c)

    datatype dir = N | S | E | W | AX | AY
    type dist = int

    fun interp (x,y) (d,dist) =
        case d of
          N => (x,y+dist)
        | W => (x-dist,y)
        | S => (x,y-dist)
        | E => (x+dist,y)
        | AX => (dist,y)
        | AY => (x,dist)
    fun logo0 f p ds c =
        let fun loop p ds =
              case ds of
                nil => nil
              | d :: ds =>
                let val p' = interp p d
                in p' :: loop p' ds
                end
              val points = loop p ds
        in f (p::points) c
      end

    fun logo p = logo0 polyline p
    fun logogon p = logo0 polygon p

    val >> : t * int -> t = fn (t,x) => translate (x,0) t
    val << : t * int -> t = fn (t,x) => translate (~x,0) t
    val ^^ : t * int -> t = fn (t,y) => translate (0,y) t
    val vv : t * int -> t = fn (t,y) => translate (0,~y) t
    val %> : t * real -> t = fn (t,s) => scale (s,1.0) t
    val %^ : t * real -> t = fn (t,s) => scale (1.0,s) t
    val @@ : t * real -> t = fn (t,theta) => rotate theta t

    val &  : t * t -> t = fn (t1,t2) => all[t1,t2]
    val // : C.entry * t -> t = fn (e,t) => fn c => t(C.add_entry e c)
  end

  fun arc8 (r:int) : t =
      let val p1 = toRpoint (r,0)
          val p2 = toPoint(real r, real r * (Math.sqrt 2.0 - 1.0))
          val p3 = let val k = real r * 0.5 * Math.sqrt 2.0
                   in toPoint(k,k)
                   end
      in fn c => (c, Qbezier(p1, p2, p3))
      end

  val pi = Math.pi

  fun circle r : t =
      let val arc = arc8 r
          val arc2 = all [arc,rotate (pi/4.0) arc]
          val arc4 = all [arc2,rotate (pi/2.0) arc2]
      in all [arc4,rotate pi arc4]
      end

  infix == @@ %^
  fun (r1:real) == r2 = not (r1 < r2 orelse r2 < r1)
  fun pointsEq((x1,y1),(x2,y2)) =
      x1 == x2 andalso y1 == y2

  fun arc0 n (r:int) (theta:real) : t =
      if theta >= pi/4.0 then
        all[arc8 r @@ (n * pi/4.0),
            arc0 (n+1.0) r (theta - pi/4.0)]
      else
        let val p1 = toRpoint (r,0)
            val p2 = toPoint(real r, real r * Math.tan (theta / 2.0))
            val p3 = toPoint(real r * Math.cos theta, real r * Math.sin theta)
            val lastarc = fn c => (c, Qbezier(p1, p2, p3))
        in lastarc @@ (n * pi/4.0)
        end

  fun arc (r:int) (theta: real) : t =
      if r <= 0 orelse theta == 0.0 then all[]
      else if theta > 2.0 * pi then
        arc r (theta - 2.0 * pi)
      else if theta < 0.0 then arc r (~theta) %^ ~1.0
      else arc0 0.0 r theta

end

structure Widget : WIDGET = struct
  type t = Object.t
  structure O = Object
  fun ruler opt =
      let val ts =
              [O.line (0,0) (100,0),
               O.line (0,0) (0,20),
               O.line (10,0) (10,10),
               O.line (20,0) (20,10),
               O.line (30,0) (30,10),
               O.line (40,0) (40,10),
               O.line (50,0) (50,20),
               O.line (60,0) (60,10),
               O.line (70,0) (70,10),
               O.line (80,0) (80,10),
               O.line (90,0) (90,10),
               O.line (100,0) (100,20)]
        val ts = case opt of
                   NONE => ts
                 | SOME (min,max) =>
                   O.text (0,~50) min ::
                   O.text (100,~50) max :: ts
      in O.all ts
      end

  fun ruler2 r opt =
      let val r = 10.0 * r
          fun norm z = round (real z / r)
          fun line (x1,y1) (x2,y2) =
              O.line (norm x1,norm y1) (norm x2,norm y2)
          val ts =
              [line (0,0) (100,0),
               line (0,0) (0,20),
               line (10,0) (10,10),
               line (20,0) (20,10),
               line (30,0) (30,10),
               line (40,0) (40,10),
               line (50,0) (50,20),
               line (60,0) (60,10),
               line (70,0) (70,10),
               line (80,0) (80,10),
               line (90,0) (90,10),
               line (100,0) (100,20)]
        val ts = case opt of
                   NONE => ts
                 | SOME (min,max) =>
                   O.text (0,norm ~50) min ::
                   O.text (norm 100,norm ~50) max :: ts
      in O.all ts
      end

  type point = O.point
  fun line_angle (p1:point) (p2:point) =
     let val x = #1 p2 - #1 p1
         val y = #2 p2 - #2 p1
         val a = if x = 0 then
                   if y > 0 then Math.pi/2.0
                   else if y < 0 then 1.5 * Math.pi else 0.0
                 else
                   Math.atan (real y / real x)
     in if x < 0 then a + Math.pi else a
     end

  fun line_len (p1:point) (p2:point) =
     let val x = #1 p2 - #1 p1
         val y = #2 p2 - #2 p1
     in floor (Math.sqrt (real (x*x + y*y)))
     end

  local
   fun conv ulen a = floor (real a * 0.05/ulen)

   fun hdotline ulen len =
     let val dot_sz = conv ulen 20
         fun lines n a =
             let val x1 = n * dot_sz
             in if x1 >= len then a
                else
                  let val x2 = (n+1) * dot_sz
                    val x2 = if x2 >= len then len else x2
                  in O.line (x1,0) (x2,0) :: lines (n+2) a
                  end
             end
     in O.all (lines 0 [])
     end

   fun vdotline ulen len =
     O.rotate (Math.pi/2.0) (hdotline ulen len)

   fun sizer ulen offset rotation point len =
     let
       val offset = ~offset
       val stopline_sz = conv ulen 80
       val stopline_sz2 = conv ulen 50
       val textsz = conv ulen 100
       val v = vdotline ulen stopline_sz
       val d = O.translate (~(stopline_sz2 div 2),
                            ~(stopline_sz2 div 2))
                           (O.rotate (Math.pi/4.0) (hdotline ulen stopline_sz2))
       val x = O.all ([O.translate (0,offset) (hdotline ulen (len div 2 - textsz)),
                       O.translate (len div 2 + textsz,offset) (hdotline ulen (len div 2 - textsz)),
                       O.translate (0,offset) d,
                       O.translate (len,offset) d,
                       O.text (len div 2,offset) (Int.toString len)] @
                      (if offset <> 0 then
                         [O.translate (0,offset) v,
                          O.translate (len,offset) v]
                       else []))

       val x = O.rotate rotation x
     in O.translate point x
     end
  in
   fun dotline ulen p1 p2 =
       O.translate p1 (O.rotate (line_angle p1 p2) (hdotline ulen (line_len p1 p2)))
   fun measure ulen offset p1 p2 =
     sizer ulen offset (line_angle p1 p2) p1 (line_len p1 p2)
  end
end


structure LatexPicture :> PICTURE = struct
  structure O = Object
  structure W = Widget
  type t = string list

  fun real_to_string r =
      let fun posreal_to_string r = Real.fmt (StringCvt.FIX NONE) r
      in if r < 0.0 then "-" ^ posreal_to_string (~r)
         else posreal_to_string r
      end

  fun pr_color (O.C.RGB(r,g,b)) =
      let fun pr_comp i =
              if i > 255 orelse i < 0 then
                raise Fail "pr_color"
              else real_to_string (real i / 255.0)
      in
        "[rgb]{" ^ pr_comp r ^ "," ^ pr_comp g ^ "," ^ pr_comp b ^ "}"
      end

  type prop = {color:O.C.color}
  fun prop_color {color} = color
  val prop_default = {color=O.C.black}

  fun int_to_string i =
      if i < 0 then "-" ^ Int.toString (~i)
      else Int.toString i

  fun pr_p (x, y) =
      "(" ^ int_to_string (floor x) ^ "," ^ int_to_string (floor y) ^ ")"

  infix ==
  val op == = O.==

  datatype length = MM of real | CM of real | PT of int
  datatype size = A4 | A3

  fun pp_length (MM r) = real_to_string (r / O.scaling) ^ "mm"
    | pp_length (CM r) = real_to_string (r / O.scaling) ^ "cm"
    | pp_length (PT i) = int_to_string (i div (round O.scaling)) ^ "pt"

  (* Notice: prop in continuation specifies the assumptions of the continuation *)
  fun pp_obj (c,t0) (A:string list,prop:prop) : string list * prop =
      let val (A, prop) =
          if O.C.get_stroke_color c = prop_color prop then (A, prop)
          else ("\\color" ^ pr_color (prop_color prop) :: A, {color=O.C.get_stroke_color c})
      in
        case t0 of
          O.Line(p1 as (x1,y1), p2 as (x2,y2)) =>
          (if O.pointsEq(p1,p2) then (A,prop)
           else
             let
               fun putline p sl l (A:string list) : string list =
                  ("\\put" ^ pr_p p ^ "{\\line" ^ pr_p sl ^ "{" ^ int_to_string (floor l) ^ "}}") :: A
               val A2 =
                   if floor x1 = floor x2 then
                     if floor y1 = floor y2 then A
                     else if y2 > y1 then putline p1 (0.0,1.0) (y2 - y1) A
                     else putline p1 (0.0,~1.0) (y1 - y2) A
                   else if floor y1 = floor y2 then
                     if x2 > x1 then putline p1 (1.0,0.0) (x2 - x1) A
                     else (* x1 > x2 *) putline p1 (~1.0,0.0) (x1 - x2) A
                   else
                     "\\qbezier" ^ pr_p p1 ^ pr_p p1 ^ pr_p p2 :: A
             in (A2,prop)
             end)
        | O.Qbezier(p1,p2,p3) =>
          (if O.pointsEq(p1,p2) andalso O.pointsEq(p2,p3) then (A,prop)
           else
             let val A2 =
                     "\\qbezier" ^ pr_p p1 ^ pr_p p2 ^ pr_p p3 :: A
             in (A2,prop)
             end)
        | O.Text(p,s) =>
          let val x = ""
              val x = if O.C.get_text_halign c = O.C.Left then x^"l" else x
              val x = if O.C.get_text_halign c = O.C.Right then x^"r" else x
              val x = if O.C.get_text_valign c = O.C.Top then x^"t" else x
              val x = if O.C.get_text_valign c = O.C.Bot then x^"b" else x
              val x = if x <> "" then "[" ^ x ^ "]" else x
          in
            ("\\put" ^ pr_p p ^ "{\\makebox(0,0)" ^ x ^ "{" ^ s ^ "}}" :: A, prop)
          end
        | O.Polyline (p1::p2::ps) =>
          pp_obj (c,O.Line(p1,p2)) (pp_obj(c,O.Polyline(p2::ps)) (A,prop))
        | O.Polyline _ => (A,prop)
        | O.Polygon (p::ps) =>
          pp_obj(c,O.Polyline(p::ps))
                (case rev ps of
                   nil => (A,prop)
                 | p2::_ => pp_obj(c,O.Line(p,p2)) (A,prop))
        | O.Polygon _ => (A,prop)
        | O.All (t::ts) =>
          let val (A,prop) = pp_obj (c,O.All ts) (A,prop)
          in pp_obj t (A,prop)
          end
        | O.All nil => (A,prop)
      end

  fun picture {unitlength,dim=p} t =
      let val obj = t O.C.empty
          val (A,prop) = pp_obj obj (["\\end{picture}"],prop_default)
      in
        "\\setlength{\\unitlength}{" ^ pp_length unitlength ^ "}" ::
        "\\begin{picture}" ^ pr_p (O.toRpoint p) ::
(*        "\\color" ^ pr_color (O.C.RGB(240,240,240)) ::
        "\\graphpaper[50](0,0)" ^ pr_p p ::
*)
        "\\color" ^ pr_color (prop_color prop) ::
        A
      end
  fun export lines = String.concatWith "\n" lines

  fun concatWith (sep:'a) (ss:'a list list) : 'a list =
      case ss of
        [] => []
      | [x] => x
      | x::xs => List.concat(x::List.map (fn y => sep::y) xs)

  fun seq ss = concatWith "\n\n" ss

  fun pp_size size =
      case size of
        A4 => "a4paper"
      | A3 => "a3paper"

  fun header size =
      ["\\documentclass{article}",
       "\\usepackage[" ^ pp_size size ^ "]{geometry}",
       "\\usepackage{color}",
       "\\usepackage{graphpap}",
       "\\RequirePackage[latin1]{inputenc}",
       "\\begin{document}",
       "\\thispagestyle{empty}",
       "\\setlength{\\textwidth}{14cm}",
       "\\noindent"
      ]

  val footer =
      ["\\end{document}"]

  fun toFile size file lines =
      let val lines = header size @ lines @ footer
          val text = export lines
          val os = TextIO.openOut file
      in (TextIO.output(os, text) handle ? => (TextIO.closeOut os; raise ?))
       ; TextIO.closeOut os
      end
end


structure SvgPicture :> PICTURE = struct
  structure O = Object
  structure W = Widget
  fun qq s = "'" ^ s ^ "'"
  fun pr_color (O.C.RGB(r,g,b)) =
      let fun zero_prefix s =
              if size s = 1 then "0" ^ s
              else s
          fun pr_comp i =
              if i > 255 orelse i < 0 then
                raise Fail "pr_color"
              else zero_prefix (Word8.toString (Word8.fromInt i))
      in
        "#" ^ pr_comp r ^ pr_comp g ^ pr_comp b
      end

  fun int_to_string i =
      if i < 0 then "-" ^ Int.toString (~i)
      else Int.toString i
  fun real_to_string r =
      let fun posreal_to_string r = Real.fmt (StringCvt.FIX NONE) r
      in if r < 0.0 then "-" ^ posreal_to_string (~r)
         else posreal_to_string r
      end

  fun pr_p (x, y) =
      "(" ^ int_to_string (floor x) ^ "," ^ int_to_string (floor y) ^ ")"

  fun pp_attrs nil = ""
    | pp_attrs ((x,y)::xs) =
      " " ^ x ^ "=" ^ qq y ^ pp_attrs xs

  fun taga t a e = "<" ^ t ^ pp_attrs a ^ ">\n" ^ e ^ "</" ^ t ^ ">\n"
  fun taga0 t a = "<" ^ t ^ pp_attrs a ^ " />\n"
  fun tag t e = taga t nil e

  datatype length = MM of real | CM of real | PT of int
  datatype size = A4 | A3

  fun pp_mm d = real_to_string d ^ "mm"
  fun pp_length (MM r) n = pp_mm (n / O.scaling * r)
    | pp_length (CM r) n = pp_mm (n / O.scaling * 10.0 * r)
    | pp_length (PT i) n = int_to_string (floor (n / O.scaling) * i)

  val pp_y_r = ref (pp_length (PT 1))
  val pp_x_r = ref (pp_length (PT 1))

  fun pp_y n = !pp_y_r n
  fun pp_x n = !pp_x_r n
  fun pp_p (x,y) = pp_x x ^ "," ^ pp_y y

  fun pp_obj (c,t0) (A:string list) : string list =
      case t0 of
        O.Line(p1 as (x1,y1), p2 as (x2,y2)) =>
        if O.pointsEq(p1,p2) then A
        else
          taga0 "line" [("x1",pp_x x1),
                        ("y1",pp_y y1),
                        ("x2",pp_x x2),
                        ("y2",pp_y y2),
                        ("style","stroke:" ^ pr_color (O.C.get_stroke_color c))] :: A
      | O.Qbezier(p1,p2,p3) =>
        if O.pointsEq(p1,p2) andalso O.pointsEq(p2,p3) then A
        else
          taga0 "path" [("d", "M" ^ pp_p p1 ^ " Q" ^ pp_p p2 ^ " T" ^ pp_p p3),
                        ("stroke", pr_color (O.C.get_stroke_color c)),
                        ("stroke-width", Int.toString (O.C.get_stroke_width c)),
                        ("fill","none")] :: A
      | O.Text((x,y),s) =>
        let val t = if O.C.get_text_halign c = O.C.Right then "end" else
                    if O.C.get_text_halign c = O.C.Center then "middle"
                    else "start"
            val b = if O.C.get_text_valign c = O.C.Top then "text-after-edge"
                    else if O.C.get_text_valign c = O.C.Middle then "middle"
                    else "text-before-edge"
        in
          taga "text"
               [("x",pp_x x),
                ("y",pp_y y),
                ("text-anchor",t),
                ("alignment-baseline",b)] s :: A
        end
      | O.Polyline (p1::p2::ps) =>
        pp_obj (c,O.Line(p1,p2)) (pp_obj(c,O.Polyline(p2::ps)) A)
      | O.Polyline _ => A
      | O.Polygon (p::ps) =>
        pp_obj(c,O.Polyline(p::ps))
              (case rev ps of
                 nil => A
               | p2::_ => pp_obj(c,O.Line(p,p2)) A)
      | O.Polygon _ => A
      | O.All (t::ts) =>
        let val A = pp_obj (c,O.All ts) A
        in pp_obj t A
        end
      | O.All nil => A

  type t = string

  fun picture {unitlength,dim=p} (f:O.t) : t =
      let
        val obj = f O.C.empty
        val (x0,y0) = O.toRpoint p  (* x0 not used *)
        val () = pp_x_r := (pp_length unitlength)
        val () = pp_y_r := (fn y => pp_length unitlength (y0 - y))
        val A = pp_obj obj nil
        val svg = taga "svg" [("xmlns", "http://www.w3.org/2000/svg"),
                              ("version", "1.1")] (String.concat A)
      in svg
      end
  fun export t = t

  fun seq ss = String.concatWith "\n" ss

  fun toFile size file lines =
      let
          val text = export lines
          val os = TextIO.openOut file
      in (TextIO.output(os, text) handle ? => (TextIO.closeOut os; raise ?))
       ; TextIO.closeOut os
      end
end
