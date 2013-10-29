structure FP3 =
struct

   fun fp3 {b: int, n: int, fs: int list
            (* length fs <= n *),
            B: int} : {N: int, Fs: int list
                      (* length Fs = N *)} =
      let
         val b = InfRat.fromInt b
         val fs = List.map InfRat.fromInt fs
         val B = InfRat.fromInt B
         val (R, _) =
            List.foldl
            (fn (f, (R, p)) => 
             (InfRat.add (R, InfRat.mul (f, p)),
              InfRat.quot (p, b)))
            (InfRat.zero, InfRat.inv b)
            fs
         val M = InfRat.quot (InfRat.pow (b, ~n), InfRat.two)
         fun loop (k, R, M, Fs) =
            let
               val k = k + 1
               val U = InfRat.floor (InfRat.mul (R, B))
               val R = InfRat.frac (InfRat.mul (R, B))
               val M = InfRat.mul (M, B)
            in
               if InfRat.gte (R, M) andalso InfRat.lte (R, InfRat.sub (InfRat.one, M))
                  then loop (k, R, M, U::Fs)
               else (k, R, U::Fs)
            end
         val (k, R, Fs) = loop (0, R, M, [])
         val Fs =
            case InfRat.compare (R, InfRat.half) of
               LESS => Fs
             | EQUAL => Fs
             | GREATER => (InfRat.add (hd Fs, InfRat.one)) :: (tl Fs)
         val N = k
         val Fs = List.map InfRat.toInt (List.rev Fs)
      in
         {N = N, Fs = Fs}
      end

   fun main (name: string, args: string list) : OS.Process.status =
      let
         fun usage () =
            (print (concat ["usage: ", name, " <B> <b> <n> <f1> <f2> ... <fn>\n"]);
             OS.Process.failure)
      in
         case args of
            B::b::n::fs => (let
                               val B = valOf (Int.fromString B)
                               val b = valOf (Int.fromString b)
                               val n = valOf (Int.fromString n)
                               val fs = List.map (valOf o Int.fromString) fs
                               val {N, Fs} = fp3 {b = b, n = n, fs = fs, B = B}
			       val () = print(concat["N = ", Int.toString N, "\nF = "])
                               val () = print (String.concatWith " " (List.map Int.toString Fs))
                               val () = print "\n"
                            in
                               OS.Process.success
                            end handle Option => usage ())
          | _ => usage ()
      end

end
