(* @todo 'handle exceptions'                                    *)
(* @todo 'try not to use real arithmetic in bin2dec'            *)
(* @todo 'fractional part in bin2dec doesn't work properly yet' *)

structure Helper = 
struct

  (* @param x     string 'string representation of the real number'           *)
  (* @ret   whole string 'integral part of the input represented as string'   *)
  (* @ret   frac  string 'fractional part of the input represented as string' *)
  fun split (x: string) = 
      let
	  val list = String.tokens
			 (fn a => (Char.ord(a) = 46))
			 x;
	  val whole = hd list;
	  val frac = 
	      if (tl list) = nil
	      then "0"
	      else hd (tl list);
      in
	  (whole, frac)
      end;

  (* @param x  string 'string representation of an integer'           *)
  (* @param y  int    'exponent'                                      *)
  (* @ret   <> string 'formatted string representation of the number' *)
  fun format (x: string, y: int) = 
      let
	  val len = size (x);
	  val pos = abs (y);
	  
	  val y = 
	      case Int.compare (y, 0) of
		  LESS => String.substring (x, 0, len-pos)
			  ^ "." ^
			  String.extract (x, len-pos, NONE)
		| EQUAL => x
		| GREATER => x ^ 
			     String.extract (
				 IntInf.toString (
				     IntInf.pow (IntInf.fromInt 10, y)), 
				 1, 
				 NONE);	  
      in
	  y
      end

  (* @param l  'IntInf.int list' 'list of integers'                          *)
  (* @ret   <> string           'string representation of the list elements' *)
  fun list2str (l: IntInf.int list) = 
      String.concat (List.map IntInf.toString l);

  (* @param x  string            'string representing an integer'           *)
  (* @ret   <> 'IntInf.int list' 'list generated from digits in the string' *)
  fun str2list (x: string) = 
      let
	  val list = String.explode x;
	  val list = List.map Char.toString list;
      in
	  List.map (valOf o IntInf.fromString) list
      end

  (* @param dec string 'string representation of the decimal float' *)
  (* @ret   <>  string 'string representation of the binary float'  *)
  fun dec2bin (dec: string) = 
      let
	  val (whole, frac) = split (dec);
	  val len1 = size (whole);
	  val len2 = size (frac);
	  val factor = IntInf.pow (10, len2);
	  
	  (* --loop for integer part conversion-- *)
	  fun loop1 (x, list) = 
	      let
		 val q = IntInf.quot (x, 2);
		 val r = IntInf.rem (x, 2);
	     in
		 case IntInf.compare (q, 0) of
		     EQUAL => list2str (r::list)
		   | _ => loop1 (q, r::list)
	     end;
	  
	  (* --loop for fractional part conversion--
	     stops after generating 64 bits for recurring numbers  *)
	  fun loop2 (x, list, k) = 
	    let
		 val p = x * 2;		 
		 val r = IntInf.quot (p, factor);
		 val q = IntInf.rem (p, factor);
	     in
		 if (k = 64) orelse (q = 0)
		 then list2str (List.rev (r::list))
		 else loop2 (q, r::list, k+1)
	     end;
	  
	  val whole = valOf (IntInf.fromString whole);
	  val frac = valOf (IntInf.fromString frac);

	  val ipart = 
	      if whole = 0
	      then "0"
	      else loop1 (whole, []);
	  val fpart = 
	      if frac = 0
	      then "0"
	      else loop2 (frac, [], 0);
      in
	  if fpart = "0"
	  then ipart
	  else ipart ^ "." ^ fpart
      end

  (* @param bin  string 'string representation of the binary float'  *)
  (* @ret   dec  string 'string representation of the decimal float' *)
  fun bin2dec (bin: string) = 
      let
	  val (whole, frac) = split (bin);
	  val list1 = str2list whole;
	  val list2 = str2list frac;

	  fun fun1 (x, (y, z)) = (y + x * z, z * (IntInf.fromInt 2));
	  
	  (* --integer part conversion-- *)	  
	  val (ipart, _) = 
	      if whole = "0"
	      then (0, 0)
	      else List.foldr fun1 (0, 1) list1;
	  val ipart = IntInf.toString ipart;
	  
	  (* --fractional part conversion-- *)
	  val (num, _) = 
	      if frac = "0"
	      then (0, 0)
	      else List.foldr fun1 (0, 1) list2;
	  val den = IntInf.pow (IntInf.fromInt 2, size frac);

	  val fpart = 
	      if num = 0
	      then "0"
	      else String.extract (
		      Real.toString (
			  (Real.fromLargeInt num) 
			  / (Real.fromLargeInt den)),
		      2,
		      NONE);
      in
	  if fpart = "0"
	  then ipart
	  else ipart ^ "." ^ fpart
      end
	 
  (* @param x  string 'string representation of the decimal number' *)
  (* @param n  int    'required digits of precision'                *)
  (* @ret   <> string 'string representation of the decimal number' *)
  fun setPrecision (x: string, n: int) = 
      let
	  (* --function to set zeroes-- *)
	  fun setZeroes (i: string, k: int) =
	      let
		  val first = String.substring (i, 0, k);
		  val second = String.extract (i, k, NONE);
		  val list = str2list second;
		  val list = List.map (fn a => 0) list;
		  val second = list2str list;
	      in
		  first ^ second
	      end;
	  
	  val bin = dec2bin (x);
	  val len = size (bin);

	  val (whole, frac) = split (bin);
	  val len1 = size (whole);
	  val radixPos = 
	      if frac = "0"
	      then len
	      else len1;

	  val num = 
	      case String.compare (frac, "0") of
		  EQUAL => if n < len1 
			   then setZeroes (whole, n) 
			   else whole
		| _ => case Int.compare (n, radixPos) of
			   EQUAL => whole
			 | LESS => setZeroes (whole, n)
			 | GREATER => whole ^ "." ^ String.substring (frac, 0, n-len1);
      in
	  bin2dec (num)
      end
 
end
