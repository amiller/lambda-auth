module Num = struct
  module Int = struct
    let (+) = Pervasives.(+)
  end
  
  module Float = struct
    let (+) = Pervasives.(+.)
  end
end

module String = struct
  let (+) = Pervasives.(^)
end

module Loaded = struct
  module Num = Num
  module String = String
  external (+) : 'a -> 'a -> 'a = "OVERLOADED"
end

module Test = struct
  open Loaded
  let () = 
    assert (1 + 2 = 3);
    assert (1.2 + 3.4 = 4.6);
    assert ("hello" + "world" = "helloworld")
end

let _ = (Loaded.(+) : int -> int -> int)
