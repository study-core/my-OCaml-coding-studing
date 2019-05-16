(* 先定义一个模块接口 A *)
module type OrderedType = 
	sig 
		(* 声明一个任意类型 t 字段  *)
		type t 
		(* 声明一个函数类型  *)
		val compare : t -> t -> int
	
end ;;

(* 
上述类型为：

module type OrderedType = sig type t val compare : t -> t -> int end
 *)

(* 定义一个 函子； 入参为 模块接口A *)
module OrdereSet = 
	(* 入参为 模块接口  *)
	functor (S: OrderedType) ->
		struct 

			(* 定义一个嵌套 函数，在 set 是否能找到 e *)
			let rec mem e set = 
				match set with
				| hd::tl -> 
					if S.compare hd e < 0 
					then mem e tl
					else S.compare hd e = 0
				| [] -> false

			(* 定义一个 空集合 *)
			let empty = []
			
			(* 定义一个递归在 适当地位置 添加 元素 e *)
			let rec add e set = 
				match set with
				| hd::tl -> 

					if S.compare hd e < 0
					then hd::(add e tl)
					else if S.compare hd e = 0
					then set
					else e::set

				| [] -> [e]	


			(* 对比两个 set 是否完全一样  *)
			let  rec subset set1 set2 =

				match set1, set2 with
				| hd1::tl1, hd2::tl2 -> 

					S.compare hd1 hd2 = 0 && subset tl1 tl2
				| [], _ -> true

				| _, []	-> false


			(* 把subset 封装成了 equal  *)
			let equal set1 set2 = 
				
				subset set1 set2 && subset set2 set1	

			(* 把两个 集合合并，并且元素从小到大排序 *)
			let rec union set1 set2 =

				match set1, set2 with
					| hd1::tl1, hd2::tl2 -> 

						(* 把两个 集合合并，并且元素从小到大排序 *)
						if S.compare hd1 hd2 = 0
						then hd1::(union tl1 tl2)
						else if S.compare hd1 hd2 < 0
						then hd1::(uion tl1 set2)
						else  hd2::(uion set1 tl2) 

					| [], _ -> set2
					| _, [] -> set1	

			(* 滤出 两个 set中相等的元素 组成一个新的组合 *)
			let rec inter set1 set2 =

				match set1, set2 with
						| hd1::tl1, hd2::tl2 -> 

							if S.compare hd1 hd2 = 0
							then hd1::(inter tl1 tl2)
							else if S.compare hd1 hd2 < 0
							then inter tl1 set2
							else inter set1 tl2 
						| [], _ -> []
						| _, [] -> []

end ;;							
(* 
上述类型为：

functor (S: OrderedType) ->
	sig
		val mem : S.t -> S.t list -> bool
		val empty 'a list
		val add : S.t -> S.t list -> S.t list
		val subset : S.t list -> S.t list -> bool
		val equal : S.t list -> S.t list -> bool
		val union : S.t list -> S.t list -> S.t list
		val inter : S.t list -> S.t list -> S.t list
end

 *)


 (* 定义一个实现了 模块接口A的 模块B *)
module InitOrderedType = struct
	(* 实现接口中的字段  *)
	type t = int
	(* 实现接口中的 函数 *)
	(* 
		就这么写就可以知道 > 0;  == 0;  < 0 ??
	 *)
	let compare a b = -(compare a b)

end ;;


(* 实例化一个 函子 类型的 模块， 函数的入参为 模块B *)

module  B  = OrdereSet(InitOrderedType);;
(* 
上述类型为：

module B :
	sig 
		val mem : InitOrderedType.t -> InitOrderedType.t list -> bool
		val empty 'a list
		val add : InitOrderedType.t -> InitOrderedType.t list -> InitOrderedType.t list
		val subset : InitOrderedType.t list -> InitOrderedType.t list -> bool
		val equal : InitOrderedType.t list -> InitOrderedType.t list -> bool
		val union : InitOrderedType.t list -> InitOrderedType.t list -> InitOrderedType.t list
		val inter : InitOrderedType.t list -> InitOrderedType.t list -> InitOrderedType.t list
 *)


 (* 使用B *)

 let set1 = B.add 3 (B.add 5 (B.add 1 B.empty)) ;;
 (* val set1 : InitOrderedType.t list = [3; 5; 1] *)

 let set2 = B.add 4 (B.add 2 (B.add 1 B.empty)) ;;
 (* val set2 ： InitOrderedType.t list = [4; 2; 1] *)

 let set3 = B.union set1 set2 ;;
 (* val set3 : InitOrderedType.t list = [5; 4; 3; 2; 1] *)


(* 	
模块类似 无构造的类	
函子 类似 有构造器的 类

模块接口 就是 无构造类的接口
函子 接口 就是 有参构造类的接口

 *)

 (* 函子接口 C *)

 module type SigSetFunctor = 
 	functor (S : OrderedType) ->
 		sig 
 			val mem : S.t -> S.t list -> bool
			val empty 'a list
			val add : S.t -> S.t list -> S.t list
			val subset : S.t list -> S.t list -> bool
			val equal : S.t list -> S.t list -> bool
			val union : S.t list -> S.t list -> S.t list
			val inter : S.t list -> S.t list -> S.t list
end ;;				

(* d对引用的函子实例加上 接口类型检查*)
mudule C = (OrdereSet: SigSetFunctor)

(* 类型Wie： module C :  SigSetFunctor *)
