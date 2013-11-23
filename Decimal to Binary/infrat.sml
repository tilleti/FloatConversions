structure InfRat =
struct
   datatype t = T of {num: IntInf.int,
                      den: IntInf.int
                      (* den >= 1 *)}

   val zero = T {num = 0, den = 1}
   val one = T {num = 1, den = 1}
   val two = T {num = 2, den = 1}
   val half = T {num = 1, den = 2}

   fun fromInt (i: int) =
      T {num = IntInf.fromInt i, den = 1}

   fun toInt (T {num, den}) =
      case IntInf.compare (den, 1) of
         EQUAL => IntInf.toInt num
       | _ => raise Domain

   fun fromLargeInt (i: IntInf.int) = 
       T {num = i, den = 1}

   fun toLargeInt (T {num, den}) =
      case IntInf.compare (den, 1) of
         EQUAL => num
       | _ => raise Domain

   fun toReal (T {num, den}) = 
       (Real.fromLargeInt num) / (Real.fromLargeInt den)

   fun toString (T {num, den}) =
      case IntInf.compare (den, 1) of
         EQUAL => IntInf.toString num
       | _ => concat [IntInf.toString num, "/", IntInf.toString den]

   fun gcd (x, y) =
      case IntInf.compare (x, 0) of
         EQUAL => y
       | _ => (case IntInf.compare (x, y) of
                  LESS => gcd (x, y - x)
                | EQUAL => x
                | GREATER => gcd (y, x))
   fun reduce (T {num, den}) =
      let
         val gcd = gcd (IntInf.abs num, IntInf.abs den)
         val sign = IntInf.fromInt (IntInf.sign num * IntInf.sign den)
      in
         (* T {num = sign * (num div gcd), *)
	 (* The above commented line is not necessary because,
            gcd is always positive (according to the way we calculated it)
            and the sign of num will be carried over to the 
            reduce output as well *)
         T {num = num div gcd,
            den = den div gcd}
      end

   fun neg (T {num, den}) =
      T {num = ~1 * num, den = den}

   fun add (T {num = num1, den = den1},
            T {num = num2, den = den2}) =
      reduce (T {num = num1 * den2 + num2 * den1,
                 den = den1 * den2})

   fun sub (r1, r2) = add (r1, neg r2)

   fun inv (T {num, den}) =
      case IntInf.compare (den, 0) of
         EQUAL => raise Div
       | _ => reduce (T {num = den, den = num})

   fun mul (T {num = num1, den = den1},
            T {num = num2, den = den2}) =
       let
	   val num = num1 * num2
	   val den = den1 * den2
       in
	   case IntInf.compare(den, 1) of
	       EQUAL => T {num = num,
			   den = den}
	      |_  => reduce (T {num = num,
				den = den})
       end  

   fun quot (r1, r2) = mul (r1, inv r2)

   fun compare (T {num = num1, den = den1},
                T {num = num2, den = den2}) =
      IntInf.compare (num1 * den2, num2 * den1)

   fun lte (r1, r2) =
      case compare (r1, r2) of
         LESS => true
       | EQUAL => true
       | GREATER => false

   fun gte (r1, r2) =
      case compare (r1, r2) of
         LESS => false
       | EQUAL => true
       | GREATER => true

  fun lt (r1, r2) = 
      case compare(r1, r2) of
	  LESS => true
	| _ => false

  fun gt (r1, r2) =
      case compare(r1, r2) of
	  GREATER => true
	| _ => false

   fun pow (r, i) =
      case Int.compare (i, 0) of
         LESS => inv (pow (r, ~ i)) 
       | EQUAL => one
       | GREATER => mul (r, pow (r, i - 1))

   fun floor (T {num, den}) =
      T {num = IntInf.quot (num, den), den = 1}

  (* fun ceil (T) = add(floor(T), one) *)

  fun ceil ( T{ num, den  } ) =
      T { num = IntInf.quot(num, den) + 1, den = 1  }

   fun frac r = sub (r, floor r)

   fun rem (r1, r2) = 
       sub ( r1, 
	     mul ( r2, 
		   floor ( ( quot (r1, r2) ) ) ) )

  fun shift (i, n, b) = 
      floor( mul ( i, pow(b, toInt n) ) )

  fun max (r1, r2) = 
      case compare(r1, r2) of
	  LESS => r2
	| EQUAL => r1
	| GREATER => r1

  fun min (r1, r2) = 
      case compare(r1, r2) of
	  LESS => r1
	| EQUAL => r2
	| GREATER => r2
end
