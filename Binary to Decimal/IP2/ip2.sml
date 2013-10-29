structure IP2 = 
struct

  fun ip2 {b: int, n: int, h: int, d: int list, B: int}
	  : {N: int, H: int, D: int list} = 
 
      let
	  val b = InfRat.fromInt b
	  val d = List.map InfRat.fromInt d
	  val B = InfRat.fromInt B
	  val M = InfRat.pow(b, n);
	  val (R,_) = 
	      List.foldr
		  (fn (f, (R,p)) =>
		      (InfRat.add (R, InfRat.mul (f, p)),
		       InfRat.mul (p, b)))
		  (InfRat.zero, InfRat.one)
		  d;
	  

	  fun loop1 (k, R, M, S) = 
	      let
		  val S = InfRat.mul (S, B);
		  val k = k + 1;
	      in
		  if InfRat.gte ( InfRat.add(InfRat.mul(InfRat.two, R), M), InfRat.mul(InfRat.two, S) )
		     then loop1 (k ,R, M, S)
		  else (k, S)
	      end
		  
	  val (k, S) = loop1 (0, R, M, InfRat.one);
	  val H = k - 1;
	  
	  (* assert S = pow(B, H+1) *)

	  fun loop2 (k, R, M, S, D) = 
	      let
		 val k = k - 1;
		 val S = InfRat.quot (S, B);
		 val U = InfRat.floor (InfRat.quot(R, S));
		 val R = InfRat.rem(R, S);
	      in
		if InfRat.gte(InfRat.mul(InfRat.two, R), M) andalso InfRat.lte(InfRat.mul(InfRat.two, R), InfRat.sub(InfRat.mul(InfRat.two, S), M))
		then loop2 (k, R, M, S, U::D)
		else (k, R, S, U::D)
	      end;

	  val (k, R, S, D) = loop2(k, R, M, S, []);
	  
	  val D = 
	      case InfRat.compare (InfRat.mul(InfRat.two, R), S) of
		  LESS => D
		| EQUAL => D
		| GREATER => (InfRat.add (hd D, InfRat.one)) :: (tl D);

	  val N = k;
	  val D = List.map InfRat.toInt (List.rev D)
      in
	  {N = N, H = H, D = D}
      end


fun main (name: string, args: string list) : OS.Process.status =
      let
         fun usage () =
            (print (concat ["usage: ", name, " <B> <b> <h> <n> <d1> <d2> ... <dn>\n"]);
             OS.Process.failure)
      in
         case args of
            B::b::h::n::d => (let
                               val B = valOf (Int.fromString B)
                               val b = valOf (Int.fromString b)
                               val h = valOf (Int.fromString h)
			       val n = valOf (Int.fromString n)
                               val d = List.map (valOf o Int.fromString) d
                               val {N, H, D} = ip2 {b = b, n = n, h = h, d = d, B = B}
			       val () = print(concat["N = ", Int.toString N, "\n"])
			       val () = print "D = "
			       val () = print (String.concatWith " " (List.map Int.toString D))
                               val () = print "\n"
                            in
                               OS.Process.success
                            end handle Option => usage ())
          | _ => usage ()
      end

end
