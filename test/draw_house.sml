structure M = struct
  val g_sw_p = (0,0)            (* ground *)
  val g_nw_p = (0,3380)
  val g_ne_p = (2483,2627)
  val g_se_p = (1320,~458)
  val h_length = 873            (* house *)
  val h_width = 690
  val kanap_dim = (302,125)
  val kanap_h_offset = 84      
  val h_sw_p = (500,764)
  val h_nw_p = (500,1455)
  val h_ne_p = (500+h_length,764+h_width)
  val h_se_p = (500+h_length,764)
  val h_mw_p = (500,764+h_width div 2)
  val h_me_p = (#1 h_ne_p,#2 h_mw_p)
  val u_w_off = 8               (* udbygning *)
  val u_length = 631
  val u_width_w = 441
  val u_width_e = 243
  val u_length_n = 299
  val u_sw_p = (#1 h_nw_p + u_w_off,#2 h_nw_p)
  val u_nw_p = (#1 u_sw_p,#2 u_sw_p + u_length)
  val u_nm_p = (#1 u_nw_p + u_width_w,#2 u_nw_p)
  val u_mm_p = (#1 u_nm_p,#2 u_nm_p - u_length_n)
  val u_em_p = (#1 u_mm_p + u_width_e, #2 u_mm_p)
  val u_eh_p = (#1 u_em_p, #2 h_nw_p)        (* east house point *)
  val o_off_w = 30
  val o_off_n = 10
  val o_off_e = 66
  val o_sw_p = (#1 u_sw_p - o_off_w,#2 u_sw_p)
  val o_nw_p = (#1 u_nw_p - o_off_w,#2 u_nw_p + o_off_n)
  val o_ne_p = (#1 o_nw_p + u_width_w + u_width_e + o_off_e,#2 o_nw_p)
  val o_se_p = (#1 o_ne_p,#2 o_sw_p)
  val o_ss_p = (#1 o_sw_p + (#1 o_se_p - #1 o_sw_p) div 2, #2 h_mw_p)
  val ga_dim = (360,716)        (* garage *)
  val ga_sw_p = (0,#2 h_sw_p + h_width-110)
  val ga_nw_p = (0,#2 ga_sw_p + #2 ga_dim)
  val ga_ne_p = (#1 ga_dim,#2 ga_nw_p)
  val ga_se_p = (#1 ga_dim,#2 ga_sw_p)
  val kvist1 = 760
  val kvist2 = 1090
end

val date = Date.fmt "%d/%m-%Y %H:%M" (Date.fromTimeLocal(Time.now()))

functor Ground(structure P : PICTURE
               val file : string) = 
struct
 open P (* units: 1 unit is 1cm *) 
 val // = O.// infix //          
 val unitlength = 0.05
 val unitlength_s = "Målestoksforhold: 1:" ^ Int.toString (floor (10.0 / unitlength)) ^ "."
 val red = O.C.stroke_color (O.C.RGB(255,0,0))
 val green = O.C.stroke_color (O.C.RGB(0,255,0))
 val left = O.C.text_halign O.C.Left

 type point = O.point

 val measure = W.measure unitlength
 val dotline = W.dotline unitlength

 val ground = O.polygon [M.g_sw_p, M.g_nw_p, M.g_ne_p, M.g_se_p]
 val house = 
     let open O 
     in logogon M.h_sw_p [(N,M.h_width),
                          (E,M.u_w_off),
                          (N,M.u_length),
                          (E,M.u_width_w),
                          (S,M.u_length_n),
                          (E,M.u_width_e),
                          (S,M.u_length - M.u_length_n),                       
                          (E,M.h_length - M.u_width_w - M.u_width_e),
                          (S,M.h_width),(W,488),(S,125),(W,302),(N,125)]
     end
 val ga = 
     let open O
     in translate M.ga_sw_p (rect (0,0) M.ga_dim)
     end
 val overbyg =
     let open O
     in red // O.all [dotline M.o_sw_p M.o_nw_p, 
                      dotline M.o_nw_p M.o_ne_p, 
                      dotline M.o_ne_p M.o_se_p, 
                      dotline M.o_se_p M.o_ss_p,
                      dotline M.o_ss_p M.o_sw_p]
     end
 val all = O.all [ground,house,ga,
                  O.line M.h_mw_p M.h_me_p,
                  overbyg,
                  measure 300 M.h_sw_p M.h_se_p,
                  measure 200 M.g_nw_p (0,0),
                  measure 100 M.h_se_p M.h_ne_p,
                  measure 100 M.g_se_p M.g_ne_p,
                  measure 100 M.g_ne_p M.g_nw_p,
                  measure 100 M.g_sw_p M.g_se_p,
                  measure 60 M.ga_ne_p M.ga_se_p,
                  measure 100 M.ga_nw_p M.ga_ne_p,
                  measure 100 M.ga_sw_p M.g_sw_p,
                  measure 100 M.h_sw_p (0,#2 M.h_sw_p),
                  measure 300 M.h_sw_p (#1 M.h_sw_p,0),
                  measure 70 M.u_nw_p M.u_nm_p,
                  measure 70 M.u_sw_p M.u_nw_p,
                  measure 70 M.u_mm_p M.u_nm_p,
                  measure 100 M.u_mm_p M.u_em_p,
                  measure 200 M.u_eh_p M.u_em_p,
                  red // measure 150 M.o_ne_p M.o_nw_p,
                  red // measure 350 M.o_se_p M.o_ne_p,
                  O.translate (1800,~750) (O.scale (10.0,1.0) (W.ruler (SOME("0m","10m")))),
                  left // O.all
                       [O.text (0,3700) "{\\large\\textbf{Grund og bebyggelsesangivelser for Matr.\\ nr.\\ 2na, Pcl.\\ 1.}}",
                        O.text (0,3600) "Peter Rørdams Vej 24, 2800 Lyngby.",
                        O.text (0,~700) "Målangivelser er i centimeter. ",
                        O.text (0,~800) unitlength_s,
                        O.text (1800,100) "Grund areal: 644m$^2$.",
                        O.text (1800,0) "Nyt bebyggelsesareal: 167m$^2$",
                        O.text (1900,~100) "(stue 99m$^2$ + 1.sal 68m$^2$).",
                        O.text (1800,~200) "Kælderareal (dyb): 45m$^2$",
                        O.text (1800,~300) "Bebyggelsesprocent: 25,9",
                        O.text (1800,~400) "Garageareal: 22m$^2$",
                        O.text (1800,~500) "Højdekoter: Max 1m forskel",
                        O.text (1900,~600) "i terrænhøjde på grund.",                   
                        O.text (30,1500) "Garage",
                        O.text (750,1550) "Udbygning",
                        O.text (700,900) "Oprindeligt hus"
                       ]
                   ]
 val p = picture {unitlength=MM unitlength,dim=(1600,3500)} all
 val () = toFile A4 file p
end

functor House(structure P : PICTURE
              val file : string) = 
struct 
 open P
 val // = O.// infix //          
 val unitlength = 0.1
 val unitlength_s = "Målestoksforhold: 1:" ^ Int.toString (floor (10.0 / unitlength)) ^ "."
 val red = O.C.stroke_color (O.C.RGB(255,0,0))
 val green = O.C.stroke_color (O.C.RGB(0,255,0))
 val left = O.C.text_halign O.C.Left

 val measure = W.measure unitlength
 val dotline = W.dotline unitlength

 val window1 =
     O.all [O.rect (0,0) (60,130),
            O.rect (10,10) (50,120)]

 val window3 =
     let val g = O.rect (5,5) (50,100)
         val f = O.rect (0,0) (155,110)
     in O.all [f, g, 
               O.translate (50,0) g, 
               O.translate (100,0) g,
               measure 40 (0,0) (155,0),
               measure 40 (155,0)(155,110)]
     end

 val window2 =
     let val g = O.rect (0,0) (50,100)
         fun tr p = O.translate p g
         val f = O.rect (0,0) (115,110)
     in O.all [f, tr (5,5), tr (60,5),
               measure 40 (0,0) (115,0)]
     end

 val window_kitchen =
    O.all [O.rect (0,0) (90,90),
           O.rect (10,10) (80,80)]

 val door =
     let val f = O.rect (0,0) (90,210)
         val g = O.rect (0,0) (30,30)
         fun tr p = O.translate p g
     in
       O.all [f, tr (10,160), tr (10,120), tr (10,80), tr (10,40),
              tr (50,160), tr (50,120), tr (50,80), tr (50,40)]
     end

 val gavl =
     let val gavl_left = 
             O.all [O.line (0,0) (40,0),
                    O.line (0,0) (0,20),
                    O.line (40,0) (40,20),
                    O.line (0,20) (390,436),
                    O.line (40,20) (390,396)]
         val gavl_right =
             O.translate (2*390,0) (O.scale (~1.0,1.0) gavl_left)
     in O.all [gavl_left, gavl_right]
     end

 val kvist =
     let val g = O.rect (0,0) (50,120)
         fun tr p = O.translate p g
     in
       O.all [O.polygon [(0,0),(190,0),(190,150),(95,190),(0,150)],
              tr (10,10), tr (70,10), tr (130,10),
              measure 30 (0,0)(190,0),
              measure 30 (190,0)(190,150)
             ]
     end

 val trappe =
     O.all [O.line (0,0) (0,100),
            O.line (90,0) (90,100),
            O.line (0,20) (90,20),
            O.line (0,40) (90,40),
            O.line (0,60) (90,60),
            O.line (0,80) (90,80)]

 (* Øst facade *)

 val found =
     O.all [dotline (0,25) (130,25),
            O.line (130,25) (850,10),
            O.line (130,25) (130,100),
            O.line (850,10) (850,100),
            O.rect (130,100) (850,360),
            O.line (850,10) (1300,0),
            O.rect (850,100) (1450,360),
            O.line (1490,100) (1490,360),
            O.line (1490,360) (1510,360),
            O.line (1510,360) (1510,800),
            O.line (1510,800) (490,800),
            O.line (1180,100) (1180,360),
            O.line (1510,390) (880,390),
            O.translate (1210,0) trappe,
            O.line (1490,100) (1530,100),
            dotline (1530,100) (1630,0),
            dotline (1630,0) (1750,0),
            measure 80 (1490,360) (1490,800),
            measure 200 (1490,0) (1490,360)
           ]

 val kv =
     O.all [O.line (0,0) (0,190),
            O.line (0,190) (170,190),
            O.line (0,150) (132,150),
            measure 50 (170,190) (0,190),
            measure 50 (0,190) (0,0)]

 val oest_facade =
     O.all [found,
            O.translate (100,360) gavl,
            O.translate (410,450) window3,
            O.translate (750,150) window1,
            O.translate (910,190) window_kitchen,
            O.translate (1060,190) window_kitchen,
            O.translate (1300,100) door,
            O.translate (M.kvist1+130,450) kvist,
            O.translate (M.kvist2+110,450) kvist,  (*new*)
            O.translate (168,450) kv,
            left // 
                 O.all [O.text (0,1000) "{\\large\\textbf{Østfacade for Matr.\\ nr.\\ 2na, Pcl.\\ 1}}",
                        O.text (0,950) "Peter Rørdams Vej 24, 2800 Lyngby.",
                        O.text (0,900) ("\\textbf{Version " ^ date ^ "}")
                       ]
           ]

 (* Nordfacade *)

 val kw = 
     O.all [O.line (188,440) (188,440+190),
            O.line (188,440+190) (343,440+190),
            O.line (188,440+150) (308,440+150),
            measure 50 (343,440+190) (188,440+190),
            measure 50 (188,440+190) (188,440)]

 val nord_found =
     O.all [dotline (850,0) (850+500,0),
            O.line (850-250,0) (850,0),
            dotline (~100,0) (~50,0),
            dotline (~50,0) (100,100),
            dotline (100,100) (130,100),
            O.line (850,0) (850,100),
            O.line (850-441,100) (850-441,360),
            O.line (160,100) (160,360),
            O.rect (130,100) (850,360),
            O.translate (168,450) kv,
            O.translate (810,450) (O.scale(~1.0,1.0) kv),
            left // O.text (0,900) "{\\large\\textbf{Nordfacade}}"
           ]

 val nord_facade =
     O.all [nord_found,
            O.translate (100,360) gavl,
            O.translate (350,450) window2,
            O.translate (520,450) window2,
            measure 30 (520,560) (465,560),
            O.translate (520,450) (measure 30 (115,0) (115,110)),
            O.translate (270,100) door,
            measure 100 (100,800) (100,360),
            measure 200 (550,0) (550,360),
            red // 
                O.all [dotline (850+500,0) (850,700),
                       left // O.text (850,750) "skrå højdegrænseplan (1,4 $\\times$ \\emph{afstand til skel})",
                       dotline (850+500,0) (850+500,100),
                       O.text (850+500,150) "skel"
                      ],
            measure 100 (850,0) (850+500,0),
            measure 100 (850,360) (850,450)
           ]

 (* Grundplan over første sal *)

 fun glass_h w = O.all [O.line (0,0) (w,0),
                        O.line (0,10) (w,10)]

 fun glass_v w = O.all [O.line (0,0) (0,w),
                        O.line (10,0) (10,w)]

 val gavl_glas_h = glass_h 190
 val gavl_glas_v = glass_v 190

 val grundplan_foerste =
     let open O
         fun wall_h p w = translate p (rect (0,0) (w,10))
         fun wall_v p w = translate p (rect (0,0) (10,w))
         val inner_walls = 
             all [wall_h (210,330) 210,
                  wall_h (210,590) 210,
                  wall_h (500,330) 210,
                  wall_h (500,590) 210,
                  wall_v (400,340) 120,
                  wall_v (400,540) 50,
                  wall_v (700,600) 280,
                  translate (540,430) (all [rect (0,0) (40,40),
                                            rect (10,10) (30,30)])
                 ]
     in
       all [green // rect (0,0) (1600,980),
            logogon (180,00) [(E,30),(N,20),(E,190),(N,30),(W,190),(N,360),(W,30),(S,410)],
            logogon (590,20) [(E,190),(S,20),(E,30),(N,230),(AX,910),(N,30),(AX,700),(S,210),(W,110)],
            logogon (180,580) [(N,350),(E,30),(S,20),(E,190),(S,30),(W,190),(S,300),(W,30)],
            logogon (1150,230) [(E,380),(N,30),(W,20),(N,190),(W,30),(S,190),(W,330)],
            logogon (1530,830) [(S,30),(W,20),(S,190),(W,30),(N,190),(W,330),(N,30)],
            logogon (810,930) [(S,100),(E,100),(S,30),(W,130),(N,80),(W,190),(N,30),(E,190),(N,20)],
            O.translate (400,30) gavl_glas_h,
            O.translate (400,890) gavl_glas_h,
            O.translate (910,240) (glass_h 240),
            O.translate (910,810) (glass_h 240),
            O.translate (1490,450) (glass_v 160),
            O.translate (190,410) (glass_v 170),
            inner_walls
           ]
     end

 val pict =
     O.all [O.translate (~200,1000) oest_facade,
            O.translate (~200,0) nord_facade,
            left // O.all [O.text (~200,~100) "Målangivelser er i centimeter. ",
                           O.text (~200,~150) unitlength_s],
            O.translate (1300,~100) (W.ruler (SOME("0","1m")))
           ]
 val p = picture {unitlength=MM unitlength,dim=(1600,2000)} pict
 val () = toFile A4 file p
end

functor Bath(structure P : PICTURE
             val file : string) = 
struct 
 open P
 val // = O.// infix //
 val >> = O.>> infix >>
 val ^^ = O.^^ infix ^^
 val vv = O.vv infix vv
 val & = O.& infix &
 val @@ = O.@@ infix @@
 val unitlength = 0.5
 val oneto = floor (10.0 / unitlength)
 val unitlength_s = "Målestoksforhold: 1:" ^ Int.toString oneto ^ "."
 val red = O.C.stroke_color (O.C.RGB(255,0,0))
 val green = O.C.stroke_color (O.C.RGB(0,255,0))
 val gray = O.C.stroke_color (O.C.RGB(200,200,200))
 val left = O.C.text_halign O.C.Left

 val measure = W.measure unitlength
 val dotline = W.dotline unitlength

 val vaeg = 10
 fun door_north_right x = 
     let open O
         val w = x - 10 
         val m = vaeg div 2 
         val d = line (0,0)(w,0) @@ (Math.pi/8.0) 
         val a = gray // arc w (Math.pi/5.0)
         val blade = d & a
     in rect (0,0) (5,vaeg) & rect (w+5,0)(x,vaeg) & (blade >> 5 ^^ m) & measure 50 (x,0)(0,0)
     end

 fun measureline off p1 p2 =
     O.all [O.line p1 p2,
            measure off p1 p2]  

 fun measurebox (x,y) t =
     O.all [O.rect (0,0) (x,y),
            O.text (x div 2, y div 2) t,
            measure 20 (x,0) (x,y)]

 val skab = O.rect (~7,0) (40,180)

 val batteri =
     O.all [O.rect (0,0) (5,5),
            O.translate (20,0) (O.rect (0,0) (5,5)),
            O.rect (5,2) (20,4),
            O.rect (11,0) (14,7)]

 val vask = O.all [O.rect (5,5) (30,55),
                   O.translate (10,30) (O.circle 3),
                   O.translate (38,18) (O.rotate (Math.pi/2.0) batteri)]

 val vask_skab_bord =
     O.all [skab,
            O.translate (0,60) vask,
            measure ~10 (40,170) (~7,170)]

 val sten1 = O.all [O.rect (0,0) (20,10),
                    O.text (10,5) "A"]
 val sten2 = O.all [O.rect (0,0) (10,20),
                    O.text (5,10) "A"]

 val bad_y = 100
 val bad_x = 80
 val bad =
     O.all [
     O.translate (0,bad_y) sten1,
     O.translate (20,bad_y) sten1,
     O.translate (bad_x,0) sten2,
     O.text (37,50) "Bad",
     O.translate (37,20) (O.circle 5),
     measure 10 (0,bad_y)(0,0),
     measure 10 (0,0)(bad_x,0),
     O.translate(bad_x + 2,20)(O.rect (0,0) (6,55)),
     O.translate(40,bad_y + 2)(O.rect (0,0) (15,6)),
     O.translate(55,75)(O.arc 27 (Math.pi/2.0)),
     O.translate(55,75)(O.arc 33 (Math.pi/2.0)),
(*
     O.line (55,bad_y + 2) (bad_x + 2,75),
     O.line (55,bad_y + 8) (bad_x + 8,75),
*)
     measure 20 (80,20) (80,75),
     measure 10 (55,bad_y + 5) (40,bad_y + 5),
     O.translate (25,0) batteri,
     door_north_right 80 >> 110 vv 10
     ]

 val toilet =
     O.all [O.translate (100,135) (O.rect (0,0)(35,45)),
            O.text (120,150) "Toilet"
           ]

 val room_y = 180
 val walls = O.all [measureline 20 (0,room_y) (0,0),
                    measureline 20 (240,room_y) (0,room_y),
                    O.line (240,room_y) (240,0),
                    measureline 20 (0,0) (110,0),
                    measureline 20 (190,0) (240,0)]
 val window =
     O.all [O.rect (0,0) (5,10),
            O.rect (57,0) (62,10),
            O.rect (5,4)(57,6)]

 val windows = O.all [window,
                      O.translate (57,0) window,
                      O.translate (114,0) window,
                      O.translate (171,0) window]
 val noter =
     String.concat [
     "\\begin{minipage}{13cm}\\textbf{Noter:}\\begin{enumerate}",
     "\\item Der lægges sorte gulvfliser (60x30) udenfor bad. Den lange fliseside lægges parallelt med vinduespartiet og sideliggende rækker lægges halvt forskudt.",
     "\\item I bad lægges sorte mosaiksten",
     "\\item Til badafskærmning mures der en sokkel rundt som anført (2 mosaiksten høj). Soklen beklæbes med mosaiksten på siderne fra væg til væg både indenfor og udenfor badet. I badåbningen lægges der tillige 2 rækker mosaiksten ovenpå soklen. Udenfor badåbningen placeres glasstenene ovenpå soklen.",
     "\\item Vægge beklæbes med hvide vægfliser til loft og helt til gulv (ingen sokkel). Den lange fliseside sættes parallelt med loft og sideliggende rækker sættes halvt forskudt.",
     "\\item Der indklæbes et spejl over vasken på 120x90 (BxH) i højden 105cm over gulv (spejlet erstatter fliserne)",
     "\\item Der etableres en termostat til gulvvarmen.",
     "\\item Der etableres en luftudsugning over bad.",
     "\\end{enumerate}\\end{minipage}"]

 val pict =
     O.all [left // O.all [O.text (0,~40) "Målangivelser er i centimeter.",
                           O.text (0,~60) unitlength_s,
                           O.text (25,~75) " glassten 20x20cm",
                           O.text (0,230) "{\\large\\textbf{Badeværelse, 1. Sal}}",
                           O.text (0,~180) noter],
            O.translate (0,~80) sten1,
            O.translate (220,~40) (W.ruler2 unitlength (SOME("0", Int.toString oneto ^ "cm"))),
            walls,
            O.translate (200,0) vask_skab_bord,
            bad,
            toilet,
            O.translate (3,room_y) windows
           ]
 val p = picture {unitlength=MM unitlength,dim=(300,200)} pict
 val () = toFile A4 file p
end

functor FirstFloor(structure P : PICTURE
                   val file : string
                   val paper : P.size
                   val unitlength : real) = 
struct 
 val area = ((246 * 2 + 10 - (2*30)) * (300+324+10+104)) + 
            150*30*4 +
            (465-60) * (303+10+239+10+257) +
            (32*239)
 val area = area div 10000
 open P
 open O
 infix // >> << ^^ vv %> %^ @@ &
 val oneto = floor (10.0 / unitlength)
 val unitlength_s = "Målestoksforhold: 1:" ^ Int.toString oneto ^ "."
 val measure = W.measure unitlength
 val red = C.stroke_color (C.RGB(255,0,0))
 val green = C.stroke_color (C.RGB(0,255,0))
 val blue = C.stroke_color (C.RGB(0,0,255))
 val gray = C.stroke_color (C.RGB(200,200,200))
 val orange = O.C.stroke_color (O.C.RGB(15*16+8,8*16,1*16+7)) (*#F88017*)
 val left = C.text_halign C.Left

 val house = 
     logogon M.h_sw_p [(N,M.h_width),
                       (E,M.u_w_off),
                       (N,M.u_length),
                       (E,M.u_width_w),
                       (S,M.u_length_n),
                       (E,M.u_width_e),
                       (S,M.u_length - M.u_length_n),                       
                       (E,M.h_length - M.u_width_w - M.u_width_e),
                       (S,M.h_width),(W,488),(S,125),(W,302),(N,125)]

 fun radiator sz = 
     blue // (rect (0,0) (10,sz) & line (0,0) (10,sz) & line (10,0) (0,sz) &
                   measure 40 (0,0) (0,sz))

 val vaeg = 10
 fun door_north_right x = 
     let val w = x - 10 
         val m = vaeg div 2 
         val d = line (0,0)(w,0) @@ (Math.pi/8.0) 
         val a = gray // arc w (Math.pi/5.0)
         val blade = d & a
     in rect (0,0) (5,vaeg) & rect (w+5,0)(x,vaeg) & (blade >> 5 ^^ m) & measure 50 (x,0)(0,0)
     end

 fun door_north_left x = door_north_right x %> ~1.0 >> x
 fun door_west_right x = door_north_right x @@ (Math.pi/2.0) >> vaeg
 fun door_east_left x = door_west_right x %> ~1.0 >> vaeg
 fun door_south_right x = door_north_left x %^ ~1.0 ^^ vaeg

 val stik = red // (rect (0,0)(10,10) & line (0,5)(10,5))
 val kontakt = red // ((circle 5 >> 5 ^^ 5) & line (5,0)(5,10))
 val stikkontakt = stik & kontakt (*red // ((circle 5 >> 5 ^^ 5) & rect (0,0) (10,10))*)
 val loftudtag = red // ((circle 5 >> 5 ^^ 5) & line (0,0) (10,10) & line (0,10) (10,0))
 val indbspots = scale (1.5,1.5) (red // polygon [(0,5),(5,10),(10,5),(5,0)])
 val udluftning = red // (circle 10 >> 10 ^^ 5)

 val wallwidth = 30
 val skorsten_width = 48
 val skorsten = rect (0,0) (skorsten_width,skorsten_width) & rect (9,9) (39,39)
 val skorsten_x = M.h_length - wallwidth - 386 - skorsten_width
 val skorsten_y = M.h_width - wallwidth - 241 - skorsten_width
 val window = rect (0,0) (10,150) & measure 50 (0,150) (0,0)
 val window_y = M.h_width div 2 - 150 div 2
 val sovevaer_width = 257
 val sovevaer_start_y = skorsten_y - 10 + 90
 val sovevaer_start_x = skorsten_x-112-vaeg
 val y1 = 189
 val sovevaer_y_low = sovevaer_start_y + 92 - y1 - 123 -150
 val sovevaer1 = logo (sovevaer_start_x,sovevaer_start_y)
                      [(N,92),(W,sovevaer_width),(S,123),(W,wallwidth)] &
                      (window >> 10 ^^ window_y) &
                      (door_west_right 90 >> sovevaer_start_x ^^ (sovevaer_start_y - 90)) &
                      (stikkontakt >> sovevaer_start_x << vaeg ^^ (sovevaer_start_y + 10)) &
                      (kontakt >> sovevaer_start_x >> vaeg ^^ (sovevaer_start_y + 10)) &
                 measure 0 (sovevaer_start_x,sovevaer_start_y + 30) (sovevaer_start_x - sovevaer_width,sovevaer_start_y + 30) &
                 let val x = sovevaer_start_x - sovevaer_width + 30
                   val y = 57
                 in measure 0 (x, sovevaer_start_y + 92) (x,sovevaer_y_low) &
                    (orange // (line (x-30,sovevaer_y_low+y) (x+sovevaer_width-30,sovevaer_y_low+y) &
                                measure 0 (x+30,sovevaer_y_low)(x+30,sovevaer_y_low+y))) &
                    measure 0 (x+710, sovevaer_start_y + 92) (x+710,sovevaer_y_low-3)
                 end
                 
 val y2 = 279
 val sovevaer2 = logo (0,window_y) [(E,wallwidth),(S,y1),(E,sovevaer_width),(N,y2)]
 val trappe_start = (sovevaer_start_x + vaeg,sovevaer_start_y + 6)
 val trin1 = line (0,0) (112,0)
 val trappe = logo (sovevaer_start_x,sovevaer_start_y) [(E,vaeg),(N,102),(W,106),(N,104),(E,218),(S,200),(W,112)] &
              (translate trappe_start
                ((trin1 ^^ 25) & (trin1 ^^ 50) & (trin1 ^^ 75) &
                 (line (0,96) (112, 110)) &
                 (line (0,96) (112, 155)) &
                 (line (0,96) (112, 200)) &
                 (line (0,96) (112-45, 200)) &
                 (line (0,96) (112-90, 200)) &
                 (line (0,96) (112-135, 200)) &
                 (line (0,96) (112-180, 200)) &
                 (line (0,96) (112-218, 200)) &
                 (line (0,96) (112-218, 155)) &
                 (line (0,96) (112-218, 110))
              ))

 val wall_k_sz_x = 50   (* width of wall between toilet and hall *)
 val vaer2_width = 304
 val bath_wall = 124 + (45 - wall_k_sz_x)
 val bath_x = 239
 val bath_y = 180
 val bath_x_start = #1 trappe_start
 val bath_y_measure = #2 trappe_start - 270
 val measure_bath_x = measure 50 (bath_x_start, bath_y_measure) (bath_x_start + bath_x, bath_y_measure)
 val bath_y_start = #2 trappe_start - 96 - 76 - vaeg
 val bath_x_measure = bath_x_start + 200
 val measure_bath_y = measure 0 (bath_x_measure, bath_y_start) (bath_x_measure, bath_y_start - bath_y)

 val gang = logo (#1 trappe_start - vaeg, #2 trappe_start - 96) 
                 [(E,vaeg),(S,76),(E,wall_k_sz_x),(S,vaeg),(W,wall_k_sz_x),(S,bath_y),(E,bath_x),(N,bath_y),(W,239-80-wall_k_sz_x),
                  (N,vaeg),(E,bath_wall),(S,207),(E,vaer2_width),(N,194),(E,wallwidth)]
            & measure_bath_x & measure_bath_y

 val gang2_start = (M.h_length + 7,
                    skorsten_y - 86 - 207 + 194 + 150)

 val vaer2_x_start = #1 gang2_start - wallwidth
 val gang2 = logo gang2_start 
                  [(W,wallwidth),(N,129),(W,200)]
                  & (window >> (#1 gang2_start - 20) ^^ (#2 gang2_start - 150))
                  & (measure 50 (vaer2_x_start, #2 gang2_start) (vaer2_x_start - 279, #2 gang2_start) vv 30 )
                  & let val p1 = (vaer2_x_start, #2 gang2_start - 343 + 60)
                        val p2 = (vaer2_x_start - (279+24), #2 gang2_start - 343 + 60)
                    in 
                      measure 50 p1 p2 ^^ 10 &
                      (orange // (line p1 p2 &
                                  measure 0 (vaer2_x_start - 30, #2 p1) (vaer2_x_start - 30, #2 p1 - 60))) 
                    end
             
 val skunk_offset = skorsten_y - 86 - vaeg - (*170*) 184

 val skunk_offset1 = skunk_offset + M.u_w_off
 val stik_left = stik >> skunk_offset1

 val wall1 = M.kvist1 - M.h_width - 15 + 50 (* 760 - 690 - 15 = 55 *)
 val wall2 = 135 (* afstand mellem kviste *)
 val wall3 = (*100*) 40  (* north first wall *)
 val wall4 = (*97*) 111
 val wall4a = 25
 val wall5 = (*320*) 300
 val wall6 = 25 (* middle wall between rooms *)
 val window2_width = 110
 val kvistwidth = 160
 val wall2a = wall2 + wall3 + kvistwidth - wall5 - vaeg
 val wall2b = wall5 - kvistwidth - wall3
 val wall2x = wall4 + window2_width + wall4a - 90 - wall6
 val splitwall = wall1 + wall2a + kvistwidth
 val kvistwindow = rect (0,0) (10,kvistwidth) & measure 80 (0,kvistwidth+15) (0,~15) & measure 40 (0,kvistwidth) (0,0)
 val radiator_v = (radiator 80) @@ (~ Math.pi / 2.0)
 val wall = 
     logo (0,0) [(N,wall1),(W,wallwidth)] &
          (logo (~wallwidth,wall1+kvistwidth) [(E,wallwidth),(N,wall2a),(E,wall2x),(N,vaeg),(W,wall2x),(N,wall2b),(W,wallwidth)]) &
          (logo (~wallwidth,wall1+wall2+2*kvistwidth) [(E,wallwidth),(N,wall3),(E,wall4),(N,wallwidth)]) &
          (logo (wall4+window2_width,wall1+wall2+wall3+2*kvistwidth+wallwidth) 
                [(S,wallwidth),(E,wall4a),(S,wall5),(W,wall6),(S,vaeg),(E,wall6+vaeg div 2)]) &
          (kvistwindow << 20 ^^ wall1) &
          (kvistwindow << 20 ^^ (wall1 + wall2 + kvistwidth)) &
          ((rect (0,0) (window2_width,10) & (radiator_v >> 15 vv 5)) >> wall4 ^^ (wall1 + wall2 + wall3 + 2 * kvistwidth + 10)) &
          (stik ^^ (splitwall - 20)) &
          (stik ^^ 570) &
          (stik ^^ 590 >> 229) &
          (stikkontakt ^^ (splitwall + vaeg) >> 110) & (* stikkontakt ved dør *)
          (translate (wall4+(*window2_width+wall4a -*) 40,splitwall + 160) loftudtag) &
          (door_north_left 90 ^^ splitwall >> wall2x) &
          (measure 50 (0,580) (wall4+window2_width+wall4a,580)) &
          (measure 70 (0,0) (0,wall2b + kvistwidth + wall3) ^^ (wall1 + wall2a + kvistwidth + vaeg)) &
          (orange // (line (30,~137) (30,602) & measure 0 (0,60) (30,60)))

 val leftwall = 
     (translate (skunk_offset + M.u_w_off,M.h_width) 
                (wall &
                 (measure ~100 (0,splitwall) (0,~34)) &
                 (stik ^^ 20) & 
                 logo (0,0) [(S,137),(E,92)]
                )) & 
       (measure 50 (0,960)(vaeg + 2*wall6,960) >> 320)
     
 val rightwall = 
     translate (M.u_width_w + M.u_width_e + M.u_w_off - skunk_offset,M.h_width)
               (wall %> ~1.0 & logo (0,0) [(S,37),(W,40),(S,70)])

 val bath_corner = (skorsten_x - 112 + wall_k_sz_x + 80 + bath_wall,skorsten_y - 86)
             
 val corner_offs_x = 15
 val corner_offs_y = 15

 val newwall1 = logo bath_corner 
                     [(E,corner_offs_x),(N,corner_offs_y),(E,vaeg),(S,corner_offs_y+vaeg),(W,corner_offs_x+vaeg)] &
                (translate bath_corner (door_south_right 80) vv vaeg << (110 + 80 + 15 - (wall_k_sz_x - 45))) &
                (translate bath_corner (kontakt & (loftudtag << 23 vv 60)) vv vaeg vv 10 << (110 + 80 + 15 + 20)) &
                (measure 0 (skorsten_x,skorsten_y) (skorsten_x, #2 bath_corner) >> 35) &
                (measure 30 (skorsten_x -112 + wall_k_sz_x, skorsten_y - 86) (skorsten_x - 112, skorsten_y - 86))


 val corner = (#1 bath_corner + corner_offs_x,#2 bath_corner + corner_offs_y + 90)
 val newwall2 = logo corner [(N,193)]
                & logo corner [(E,vaeg),(N,161),(E,80)]
                & (translate corner stikkontakt >> vaeg ^^ 10)
                & (translate corner stikkontakt << 10 ^^ 160)
                & (translate corner (door_east_left 90) vv 90)
                & (measure 0 (skorsten_x + skorsten_width,skorsten_y) (#1 corner, skorsten_y) ^^ 35)

 fun pp_list pp title elems =
     left // (O.text(0,0) ("{\\textbf{" ^ title ^ ":}}") &
                    #2 (foldl (fn (e,(n,a)) => (n+1,a & pp(n,e))) (1,all[]) elems))

 val noter = pp_list (fn (n,e) => O.text(0,n * ~50) (Int.toString n ^ ". " ^ e)) "Noter"
             ["Højde på radiatorer: 50cm",      
              "Højde på radiator i kvist: 40cm",      
              "Stuekvistes ydre bredde: 190cm", 
              "Skunkhøjde i ovenbygning: 80cm"]

 val symboler = 
     pp_list (fn (n,(e,s)) => (e & O.text(30,0) s) vv (n * 50)) "Symboler"
             [(stik, "Stikdåse, 20cm over gulv"),
              (kontakt, "Kontakt loftlampe, 110cm over gulv"),
              (stikkontakt, "Stikdåse og kontakt, 110cm over gulv"),
              (loftudtag, "Loftlampeudtag"),
              (indbspots, "Indbygningsspot"),
              (udluftning, "Ventilation")]

 val pict = O.all [green // house << (#1 M.h_sw_p) vv (#2 M.h_sw_p),
                   skorsten ^^ skorsten_y >> skorsten_x,
                   sovevaer1, sovevaer2, trappe, gang, gang2, leftwall, rightwall, newwall1, newwall2,
                   text (400,200) "Bad",
                   translate (340,160) indbspots,
                   translate (450,160) indbspots,
                   translate (470,230) indbspots,
                   translate (510,240) udluftning,
                   translate (250,800) loftudtag,
                   text (350,800) "Stue",
                   text (240,1100) "Værelse",
                   text (480,1100) "Værelse",
                   text (150,300) "Værelse",
                   text (700,300) "Værelse",
                   W.ruler (SOME("0","1m")) vv 150,
                   translate corner (radiator 80) >> 15 ^^ 70,
                   translate corner (radiator 120 %> ~1.0) >> 44 ^^ 425,
                   left // all [O.text (500,~50) "Målangivelser er i centimeter.",
                                O.text (500,~100) "Orange linier angiver 150cm afstand til ydre tag.",
                                O.text (500,~150) ("Areal 1. sal (indenfor orange linier, inkl. kvistarealer): " ^ Int.toString area ^ "m$^2$"),
                                O.text (500,~200) unitlength_s,
                                O.text (0,1400) "{\\large\\textbf{Plantegning, 1. Sal}}",
                                O.text (0,1360) ("\\textbf{Version " ^ date ^ "}")],
                   noter >> 750 ^^ 1350,
                   symboler >> 750 ^^ 1020
                  ]

 val p = picture {unitlength=MM unitlength,dim=(300,1300)} pict
 val () = toFile paper file p
end

structure A = House(structure P = LatexPicture
                    val file = "house.tex")

structure B = House(structure P = SvgPicture
                    val file = "house.xml")

structure C = Ground(structure P = LatexPicture
                     val file = "ground.tex")

structure D = Ground(structure P = SvgPicture
                     val file = "ground.xml")

structure E = Bath(structure P = LatexPicture
                   val file = "bath.tex")

structure F1 = FirstFloor(structure P = LatexPicture
                         val file = "firstfloor.tex"
                         val paper = P.A4
                         val unitlength = 0.15)

structure F2 = FirstFloor(structure P = LatexPicture
                         val file = "firstfloor_a3.tex"
                         val paper = P.A3
                         val unitlength = 0.2)


