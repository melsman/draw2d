(* Test of PICTURE interface *)
functor Test(structure P : PICTURE
             val file : string) = struct 
 open P

 (* Different color entries *)
 val red = O.C.stroke_color (O.C.RGB(255,0,0))
 val green = O.C.stroke_color (O.C.RGB(0,255,0))

 (* Sine example *)
 local
   fun gen i a = 
       if i < 0 then a
       else (i,50 + round (50.0 * Math.sin (real i / 10.0))) :: gen (i-5) a
   val ps = gen 100 []
 in
   val sine = O.polyline ps
   val sine2 = O.rotate 0.1 (O.scale (1.0,0.8) sine)
 end

 (* Two rectangles *)
 val r = O.rect (30,55) (45,75)
 val r2 = O.rotate 0.2 r

 (* A red line and a rotated red line *)
 infix //
 val op // = O.//

 val l = red // O.line (20,40) (36,80)
 val l1 = O.rotate 0.3 l

 (* A centimeter ruler *)
 val ruler = 
     O.all [O.line (0,0) (10,0),
            O.line (0,0) (0,2),
            O.line (1,0) (1,1),
            O.line (2,0) (2,1),
            O.line (3,0) (3,1),
            O.line (4,0) (4,1),
            O.line (5,0) (5,2),
            O.line (6,0) (6,1),
            O.line (7,0) (7,1),
            O.line (8,0) (8,1),
            O.line (9,0) (9,1),
            O.line (10,0) (10,2)]

 val l_m = red // O.line (0,0) (20,30)
 val l_mm = green // O.line (40,10) (50,80)
 val l_m2 = O.mirror (40,10) (50,80) l_m

 val circle = green // O.circle 20
 val ellipse = O.scale (2.0,1.0) circle
 val ellipse2 = O.rotate (Math.pi / 4.0) ellipse
 val obj = O.scale (2.0,1.0) ellipse2
 val circles = red // O.all [O.translate (140,20) circle,
                             O.translate (20,20) ellipse,
                             O.translate (60,20) ellipse2,
                             O.translate (100,20) obj]

 infix >> @@ & << %> ^^ vv
 val op @@ = O.@@
 val op & = O.&
 val op >> = O.>>
 val op vv = O.vv
 fun mycirc n r =
     let val theta = 2.0 * Math.pi / real n
         val a = O.arc r theta
         fun loop i = if i >= n then []
                      else (a @@ (real i * 2.0 * theta)) :: loop (i+1)
     in O.all (loop 0)
     end

 val circles2 = red // O.translate (30,30) (mycirc 32 40) &
                (green // O.translate (10,10) (mycirc 64 20))

 val lyssvaerd =
     let open O
       val len = 80
       val side = logo (0,0) [(E,5),(N,4),(W,2),(N,18),(E,2),(N,2),
                              (W,3),(N,len)]
     in
       all[side,
           side %> ~1.0,
           arc 2 Math.pi ^^ (len + 24)]
     end

 val glight = 
     let fun loop n = 
             if n = 0 then []
             else 
               (lyssvaerd @@ ~(real n * Math.pi/32.0)) :: loop (n-1)
     in O.all (loop 8)
     end

 (* The entire picture to paint *)
 val pict =
     O.all [O.line (20,40) (20,80),
            l,l1,
            r,r2,
            l_m,l_mm,l_m2,
            green // O.rect (0,0) (100,100),
            sine, sine2,
            ruler,
            O.text (10,10) "hello world",
            circles,
            circles2,
            (red // (lyssvaerd @@ Math.pi/4.0) >> 60 &
                 (green // (lyssvaerd @@ ~(Math.pi/4.0)))) vv 100,
           red // glight >> 90 vv 100]

 val p = picture {unitlength=MM 1.0,dim=(100,100)} pict
 val () = toFile A4 file p
end

(* Paint with LaTeX backend - use pdflatex *)
structure A = Test(structure P = LatexPicture
                   val file = "simple.tex")

(* Paint with SVG backend - just load the file into a browser (e.g., Firefox) *)
structure B = Test (structure P = SvgPicture
                    val file = "simple.xml")
