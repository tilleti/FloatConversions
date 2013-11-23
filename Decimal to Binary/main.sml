structure Main = 
struct 

  fun main (name: string, args: string list) : OS.Process.status = 
      let
	  fun usage () = 
	      (print (concat["usage: ",
			     name,
			     " <f> <e>", 
			     "\n"]);
	       OS.Process.failure)
      in
	  case args of 
	      f::e::_ => (let
			     val e = valOf (Int.fromString e);
			     val f = valOf (IntInf.fromString f);
			     
			     val F = Bellerophon.bellerophon (f, e);
			     val () = print F;
			     val () = print "\n";
			 in
			     OS.Process.success
			 end handle Option => usage ())
	     |_ => usage ()
      end;

end
