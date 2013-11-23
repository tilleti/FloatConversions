(* @todo 'using real number arithmetic eats up a few bits'
         'try avoiding it - makefloat, prevfloat, nextfloat' *) 

structure AlgoR= 
struct
  
  val delta = 10; (* radix of output    *)
  val beta = 2;   (* radix of input     *)
  val n = 24;     (* precision required *)
  
  (* @param x  string 'input decimal number' *)
  (* @ret   <> string 'float significand'    *)
  (* @ret   <> int    'float exponent'       *)
  (*@notes  'alternative - Real.toManExp'    *)
  fun toSigExp (x: string) = 
      let
	  val (whole, frac) = Helper.split (Helper.dec2bin x);	  
      in
	  if frac = "0"
	  then (Helper.bin2dec whole, 0)
	  else (Helper.bin2dec (whole ^ frac), ~(size frac))
      end;

  (* @param x  string 'significand'                             *)
  (* @param y  int    'exponent'                                *)
  (* @ret   <> string 'resulting number as string'              *)
  (* @notes    'simply put => sig * pow(2, exp)' 
               'direct multiplication requires real arithmetic' *)
  fun fromSigExp (x: string, y: int) = 
      let
	  val bin = Helper.dec2bin x;
	  val bin = Helper.format (bin, y);
      in
	  Helper.bin2dec (bin)
      end;

  (* @param q  InfInt.int 'significand'                *)
  (* @param k  int        'exponent'                   *)
  (* @ret   <> string     'resulting number as string' *)
  fun make_float (q: IntInf.int, k: int) = 
      Real.toString (
	  Real.fromManExp {exp = k, man = Real.fromLargeInt q});

  (* @param q  InfInt.int 'significand'                *)
  (* @param k  int        'exponent'                   *)
  (* @ret   <> string     'resulting number as string' *)
  fun next_float (q: IntInf.int, k: int) = 
      Real.toString (
	  Real.fromManExp {exp = k, man = Real.fromLargeInt (q+1)});

  (* @param q  InfInt.int 'significand'                *)
  (* @param k  int        'exponent'                   *)
  (* @ret   <> string     'resulting number as string' *)
  fun prev_float (q: IntInf.int, k: int) = 
      Real.toString (
	  Real.fromManExp {exp = k, man = Real.fromLargeInt (q-1)});

  (* @param f  IntInf.int 'input positive decimal' *)
  (* @param e  int        'exponent'               *)
  (* @param z  string     'closest float'          *)
  (* @ret   <> string     'output closest float'   *)
  fun algor (f: IntInf.int, e: int, z: string) : string = 
      let
	  val b = IntInf.fromInt beta;
	  val B = IntInf.fromInt delta;
	  
	  (* --loop--                                    *)
	  (* loop until a proper convergence is acheived *)
	  fun loop (z: string) = 
	      let		      
		  val (m, k) = toSigExp z;
		  val m = valOf (IntInf.fromString m);
		  
		  (* --compare--                             *)
		  (* decide when to stop the loop and output *)
		  fun compare (x: IntInf.int, y: IntInf.int) = 
		      let 
			val D = x - y;
			val D2 = 2 * m * abs (D);
		      in
			  if (D2 < y)
			  then (if (m = IntInf.pow (b, n-1)) 
				   andalso (D < 0) 
				   andalso ((b * D2) > y) 
				then loop (prev_float (m, k)) 
				else z) 
			  else if (D2 = y)
			  then (if (IntInf.rem (m, IntInf.fromInt 2) = 0) 
				then (if (m = IntInf.pow (b, n-1)) 
					 andalso (D < 0) 
				      then loop (prev_float (m, k)) 
				      else z) 
				else if (D < 0) 
				then prev_float (m, k) 
				else next_float (m, k))
			  else if (D < 0)
			  then loop (prev_float (m, k))
			  else loop (next_float (m, k))
		      end;
	      in
		 case Int.compare (e, 0) of
		     LESS => if (k < 0)
			     then compare (f * (IntInf.pow (b, ~k)), m * (IntInf.pow (B, ~e)))
			     else compare (f, m * (IntInf.pow (b, k)) * (IntInf.pow (B, ~e)))
		   | _ => if (k < 0)
			  then compare (f * (IntInf.pow (B, e)) * (IntInf.pow (b, ~k)), m)
			  else compare (f * (IntInf.pow (B, e)), m * (IntInf.pow (b, k)))
	      end
      in
	  loop (z)
      end

end
