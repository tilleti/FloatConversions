(* @todo 'try to avoid real number arithmetic wherever possible' *)

structure Bellerophon = 
struct
  
  val n = 24 (* precision - shortfloat *);
  val p = 32 (* precision - float      *);
  val h = 9  (* minimum representable exponent without an approximation *);
  
  (* @param x string 'string representation of the decimal integer input'                *)
  (* @ret   f string 'string representation of the decimal output with p bits precision' *)
  fun i2f (x: string) = 
      Helper.setPrecision (x, p);

  (* @param x  string 'string representation of the decimal integer input'                *)
  (* @ret   sf string 'string representation of the decimal output with n bits precision' *)
  fun i2sf (x: string) = 
      Helper.setPrecision (x, n);

  (* @param f  string 'string representation of the decimal input with p bits precision'  *)
  (* @ret   sf string 'string representation of the decimal output with n bits precision' *)
  fun f2sf (f: string) = 
      Helper.setPrecision (f, n);

  (* @param x  string 'string representation of the input'       *)
  (* @ret   <> string 'string representation of the significand' *)
  fun f_significand (x: string) = 
      let
	  val x = Helper.setPrecision (x, p);
	  val (whole, frac) = Helper.split (Helper.dec2bin x);
      in
	  if frac = "0"
	  then Helper.bin2dec (whole)
	  else Helper.bin2dec (whole ^ frac)
      end;

  (* @param x  IntInf.int 'number'   *)
  (* @param y  int        'exponent' *)
  (* @ret   <> string     'product'  *)
  fun f_mul (x: IntInf.int, y: int) = 
      Helper.format (IntInf.toString x, y);
  
  (* @param x  IntInf.int 'number'   *)
  (* @param y  int        'exponent' *)
  (* @ret   <> string     'product'  *)
  fun sf_mul (x: IntInf.int, y: int) = 
      Helper.format (IntInf.toString x, y);
  
  (* @param x  IntInf.int 'number'               *)
  (* @param y  int        'exponent'             *)
  (* @ret   <> string     'quotient + remainder' *)
  fun sf_div (x: IntInf.int, y: int) = 
      Helper.format (IntInf.toString x, y);

  (* @param f  IntInf.int 'input number'         *)
  (* @param e  int        'exponent'             *)
  (* @param z  string     'closest float'        *)
  (* @ret   <> string     'output closest float' *)
  fun fail (f: IntInf.int, e: int, z: string) = 
      AlgoR.algor (f, e, f2sf (z));

  (* @param f    IntInf.int 'input number' *)
  (* @param e    int        'exponent'     *)
  (* @param slop int        'error bound'  *)
  (* @ret   F    string     'output float' *)
  fun multiply_and_test (f: IntInf.int, e: int, slop: int) = 
      let
	  val x = valOf (IntInf.fromString (i2f (IntInf.toString f))); (* IntInf.int *)
	  val y = IntInf.pow (IntInf.fromInt 10, e); (* IntInf.int *)
	  val z = f_mul (x, e); (* string *)
	  
	  val pow1 = IntInf.pow (IntInf.fromInt 2, p-n);
	  val pow2 = IntInf.pow (IntInf.fromInt 2, p-n-1);
	  val lowbits = IntInf.rem (valOf (IntInf.fromString (f_significand z)), pow1);

	  val F = 
	      if (IntInf.abs (lowbits - pow2) <= IntInf.fromInt (slop))
	      then fail (f, e, z)
	      else f2sf (z)
      in
	  F
      end
  
  (* @param f IntInf.int 'decimal input > 0'                  *)
  (* @param e int        'exponent'                           *)
  (* @ret   F string     'float approximation of given input' *)
  fun bellerophon (f: IntInf.int, e: int)  = 
      let
	  val pow1 = IntInf.pow (IntInf.fromInt 2, n);
	  val pow2 = IntInf.pow (IntInf.fromInt 2, p);
	  val log  = 
	      Real.ceil (
		  Real.fromInt (n) 
		  * Math.ln (Real.fromInt 2) 
		  / Math.ln (Real.fromInt 5));

	  val cond1 = (f < pow1) andalso (e >= 0) andalso (e < h) andalso (e < log);
	  val cond2 = (f < pow1) andalso (e < 0) andalso (~e < h) andalso (~e < log);
	  val cond3 = (f < pow2) andalso (e >= 0) andalso (e < h);
	  val cond4 = (f < pow2) andalso ((e < 0) orelse (e >= h));
	  val cond5 = (f >= pow2) andalso (e >= 0) andalso (e < h);
	  val cond6 = (f >= pow2) andalso ((e < 0) orelse (e >= h));

	  val F = 
	      if cond1
	      then sf_mul (valOf (IntInf.fromString (i2sf (IntInf.toString f))), e)
	      else if cond2 
	      then sf_div (valOf (IntInf.fromString (i2sf (IntInf.toString f))), e)
	      else if cond3 
	      then multiply_and_test (f, e, 0) 
	      else if cond4 
	      then multiply_and_test (f, e, 3) 
	      else if cond5 
	      then multiply_and_test (f, e, 1) 
	      else if cond6 
	      then multiply_and_test (f, e, 4)
	      else "Something's wrong!!! Shouldn't have reached here...";
      in
	  F
      end

end
