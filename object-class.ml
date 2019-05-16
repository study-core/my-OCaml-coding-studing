
(* 
使用链表定义一个 堆栈类

类型为:


class stack_of_ints :
  object
    val mutable the_list : int list
    method peek : int
    method pop : int
    method push : int -> unit
    method size : int
  end



 *)

 class stack_of_ints =
    object (self)
      val mutable the_list = ( [] : int list ) (* instance variable *)
      method push x =                        (* push method *)
        the_list <- x :: the_list
      method pop =                           (* pop method *)
        let result = List.hd the_list in
        the_list <- List.tl the_list;
        result
      method peek =                          (* peek method *)
        List.hd the_list
      method size =                          (* size method *)
        List.length the_list
    end;;