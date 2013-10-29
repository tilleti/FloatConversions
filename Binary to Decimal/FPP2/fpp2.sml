
structure FPP2 = 
struct
  
  fun fpp2 {b: int, e: int, p: int, fs: int list, B: int}
	 : {N: int, H:int, Fs: int list} = 

    let
	val b = InfRat.fromInt b;
	val B = InfRat.fromInt B;
	val e = InfRat.fromInt e;
	val p = InfRat.fromInt p;
	val fs = List.map InfRat.fromInt fs;

	val (fs, _) = 
	    List.foldr 
		(fn (f, (fs, y)) => 
		    (InfRat.add(fs, InfRat.mul(f, y)), InfRat.mul(y, b))) 
		(InfRat.zero, InfRat.one) 
		fs;	

	val d = InfRat.sub(e, p);

	(* ASSERT fs != 0 *)
	
	val R = InfRat.shift(fs, InfRat.max(d, InfRat.zero), b);
	val S = InfRat.shift(InfRat.one, InfRat.max(InfRat.zero, InfRat.neg d), b);
	
	(* ASSERT R/S = fs * b^(e-p) = v *)

	val Mm = InfRat.shift(InfRat.one, InfRat.max(d, InfRat.zero), b);
	val Mp = Mm;

	val v = InfRat.mul(fs, InfRat.pow(b, InfRat.toInt d));
	val vp = InfRat.add(v, InfRat.pow(b, InfRat.toInt d));
	val vm = 
	    if fs = InfRat.pow(b, InfRat.toInt(InfRat.sub(p, InfRat.one))) 
	    then InfRat.sub(v, InfRat.pow(b, InfRat.toInt(InfRat.sub(d, InfRat.one)))) 
	    else InfRat.sub(v, InfRat.pow(b, InfRat.toInt d))

	(* -------- SIMPLE FIXUP -------- *)
	
	(* ASSERT R/S = v *)
	(* ASSERT Mm/S = Mp/S = b^(e-p)  *)
	
	val (Mp, R, S) = 
	    if (fs = InfRat.shift(InfRat.one, InfRat.sub(p, InfRat.one), b))
	    then (InfRat.shift(Mp, InfRat.one, b),
		  InfRat.shift(R, InfRat.one, b),
		  InfRat.shift(S, InfRat.one, b))
	    else (Mp, R, S)

	fun loop1 (k, R, S, B, Mm, Mp, v, vm, vp) = 
	    let
		
		(* ASSERT (R/S) * B^k = v *)
		(* ASSERT (Mm/S) * B^k = v - vm *)
		(* ASSERT (Mp/S) * B^k = vp - vm *)

	    in
		if InfRat.lt(R, InfRat.ceil(InfRat.quot(S, B)))
		then loop1(InfRat.sub(k, InfRat.one), 
			   InfRat.mul(R, B), S, B, 
			   InfRat.mul(Mm, B), 
			   InfRat.mul(Mp, B), 
			   v, vm, vp)
		else (k ,R, Mm, Mp)
	    end

	val (k, R, Mm, Mp) = loop1(InfRat.zero, R, S, B, Mm, Mp, v, vm, vp);

	(* ASSERT k = min(0, 1 + floor(log-B(v))) *)

	fun loop2 (k, R, S, B, Mm, Mp, v, vm, vp) = 
	    let
		
		(* ASSERT (R/S) * B^k = v *)
		(* ASSERT (Mm/S) * B^k = v - vm *)
		(* ASSERT (Mp/S) * B^k = vp - vm *)
		
	    in
		if InfRat.gte(InfRat.add(InfRat.mul(InfRat.two, R), Mp), InfRat.mul(InfRat.two, S))
		then loop2(InfRat.add(k, InfRat.one),
			  R, InfRat.mul(S, B),
			  B, Mm, Mp, v, vm, vp)
		else (k, S)
	    end

	val (k, S) = loop2(k, R, S, B, Mm, Mp, v, vm, vp);

	(* ASSERT k = 1 + floor(log-B((v + vp)/2))  *)
	
	(* -------- ------ ----- -------- *)
	
	val H = InfRat.sub(k, InfRat.one);
	
	fun loop3 (k, R, B, S, Mm, Mp, Fs) = 
	    let
		
		(* ASSERT ((R/S) * B^k) + summation(i=k:H)(Di * B^i) = v   *)

		val k = InfRat.sub(k , InfRat.one);
		val U = InfRat.floor(InfRat.quot(InfRat.mul(R, B), S));
		val R = InfRat.rem(InfRat.mul(R, B), S);
		val Mm = InfRat.mul(Mm, B);
		val Mp = InfRat.mul(Mp, B);
		val low = InfRat.lt(InfRat.mul(InfRat.two, R), Mm);
		val high = InfRat.gt(InfRat.mul(InfRat.two, R), InfRat.sub(InfRat.mul(InfRat.two, S), Mp));
	    in
		if (not low) andalso (not high)
		then loop3(k, R, B, S, Mm, Mp, U::Fs)
		else (low, high, k, R, U::Fs)
	    end
		
	val (low, high, k, R, Fs) = loop3(k, R, B, S, Mm, Mp, []);
	
	(* COMMENT Let Vk = summation(i=k+1:H)(Di * B^i)  *)
	(* ASSERT low  :  ((vm + v)/2) < (U * B^k + Vk) <= (v)   *)
	(* ASSERT high : (v) <= ((U+1) * B^k + Vk) < ((vp + v)/2)  *)
	
	val Fs = 
	    case (low, high) of
		(true, false) => Fs
	      | (false, true) => (InfRat.add(hd Fs, InfRat.one))::(tl Fs)
	      | (true, true) => case InfRat.compare(InfRat.mul(InfRat.two, R), S) of
				    LESS => Fs
				  | EQUAL => Fs
				  | GREATER => (InfRat.add(hd Fs, InfRat.one)) :: (tl Fs)
	val N = k;
	val Fs = List.map InfRat.toInt(List.rev Fs);
    in
	{N = InfRat.toInt N, H = InfRat.toInt H, Fs = Fs}
    end


  fun main (name: string, args: string list) : OS.Process.status =
      let
         fun usage () =
            (print (concat ["usage: ", 
			    name, 
			    " <B> <b> <e> <p> <f1> <f2> ... <fn>\n"]);
             OS.Process.failure)
      in
         case args of
            B::b::e::p::fs => (let
				  val B = valOf (Int.fromString B)
				  val b = valOf (Int.fromString b)
				  val e = valOf (Int.fromString e)
				  val p = valOf (Int.fromString p);
				  val fs = List.map (valOf o Int.fromString) fs;
				  val {N, H, Fs} = fpp2 {b = b, e = e, p = p, fs = fs, B = B}
				  val () = print(concat["N = ", Int.toString N, "\n"])
				  val () = print "Fs = "
				  val () = print (String.concatWith " " (List.map Int.toString Fs))
				  val () = print "\n"
                              in
				  OS.Process.success
                              end handle Option => usage ())
          | _ => usage ()
      end
end
