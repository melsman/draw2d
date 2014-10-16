open LatexPicture

open O
infix // & >> @@ %^ vv ^^ <<

val delta = 2
val len = 40
fun t caption src tgt base =
    let val x1 = (len-delta) div 2
        val x2 = len-delta + len div 2
        val x3 = len-delta + len + (len-delta) div 2
        val y1 = (len-delta) div 2
        val y2 = ~len div 2 
    in
      logogon (0,0) [(E,len-delta),(S,len),(E,len),(N,len),(E,len-delta),(N,len-delta),(W,3*len-2*delta)] &
      text (x1, y1) src &
      text (x3, y1) tgt & 
      text (x2, y1) caption &
      text (x2, y2) base
    end

val mlkit_runtime = "$\\stackrel{\\mbox{x86}}{\\mbox{MLKit}}$"
val mlton_runtime = "$\\stackrel{\\mbox{x86}}{\\mbox{MLton}}$"
val mlkit_source = "$\\stackrel{\\mbox{\\textbf{MLKit}}}{\\mbox{\\textbf{Source}}}$"
val mlkit_src = t mlkit_source "ML" mlkit_runtime "ML"
val mlton_bin = t "\\textbf{MLton}" "ML" mlton_runtime mlton_runtime
val mlkit_bin1 = t "\\textbf{MLKit1}" "ML" mlkit_runtime mlton_runtime
val mlkit_bin2 = t "\\textbf{MLKit2}" "ML" mlkit_runtime mlkit_runtime
val mlkit_bin3 = t "\\textbf{MLKit3}" "ML" mlkit_runtime mlkit_runtime

fun addSource t0 s = t0 & (s << 2*len ^^ len)
fun addTarget t0 t = t0 & (t >> 2*len ^^ len)

val all = addSource (addTarget mlton_bin (addTarget mlkit_bin1 (addSource (addTarget mlkit_bin2 mlkit_bin3) mlkit_src))) (addTarget mlkit_src mlkit_src)

val p = picture {unitlength=MM 0.4,dim=(400,300)} (all>>len*2)
val () = toFile A4 "tdiagram.tex" p

