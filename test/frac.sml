(* Test of PICTURE interface *)
functor Frac(structure P : PICTURE
             val file : string) = struct 
 open P

 (* Different color entries *)
 val red = O.C.stroke_color (O.C.RGB(255,0,0))
 val green = O.C.stroke_color (O.C.RGB(0,255,0))

 infix // & >> @@ %^ vv ^^ <<
 val op // = O.//
 val op & = O.&
 val op >> = O.>>
 val op << = O.<<
 val op @@ = O.@@
 val op %^ = O.%^
 val op vv = O.vv
 val op ^^ = O.^^

 fun frac (n, len) = 
     if n = 0 then O.line (0,0) (len,0)
     else
       let val a = len div 3
         val r = Math.pi / 3.0
       in O.all [(*O.line (0,0) (a,0), *)
                 frac(n-1,a),
                 frac(n-1,a) >> (2 * a),
                 frac(n-1,a) @@ r >> a,
                 frac(n-1,a) %^ ~1.0 @@ (2.0 * r) >> (2 * a) (*,
                 O.line (2*a,0) (len,0) *) ]
       end

 fun star (n, len) =
     let val r = Math.pi / 3.0
       val f = frac(n,len)
     in
       O.all[f %^ ~1.0,
             f %^ ~1.0 @@ (2.0 * r) >> len,
             f @@ r]
     end

 (* pythagoraian tree *)
 val theta = Math.pi / 1.8
 fun pyth n len =
     if n = 0 then O.all []
     else
     let val len_i = round len
         val x = len / 2.0 * ( 1.0 + Math.cos theta)
         val y = len / 2.0 * Math.sin theta
         val a1 = Math.atan (y/x)
         val h1 = Math.sqrt (x*x + y*y)
         val a2 = Math.atan (y/(len-x))
         val h2 = Math.sqrt ((len-x)*(len-x) + y*y)
     in O.all[O.rect (0,0) (len_i,len_i),
              pyth (n-1) h1 @@ a1 ^^ len_i,
              (pyth (n-1) h2 << (floor h2)) @@ (~a2) >> len_i ^^ len_i]
     end

 val pyt = pyth 6 200.0 >> 600

 val stars = O.all[star (1,400), star(2,400) >> 500,
                  O.all [star (3,400), star(4,400) >> 500] ^^ 500,
                  star (5,1000) ^^ 1300]
 val p1 = picture {unitlength=MM 0.1,dim=(1000,2000)} stars
 val p2 = picture {unitlength=MM 0.1,dim=(1000,600)} pyt
 val () = toFile A4 file (seq [p1,p2])
end

(* Paint with LaTeX backend - use pdflatex *)
structure A = Frac(structure P = LatexPicture
                   val file = "frac.tex")

structure B = Frac(structure P = SvgPicture
                   val file = "frac.xml")
