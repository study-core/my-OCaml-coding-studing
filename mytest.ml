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

(* 把 函子 OrdereSet 限制到 函子接口 SigSetFunctor 上*)
mudule C = (OrdereSet: SigSetFunctor)

(* 类型Wie： module C :  SigSetFunctor *)




(* 
###################
###################

多参数 函子

##################
##################
 *)


(* 先定义一个 模块接口 *)
module type B =  sig val b : int end ;;

(* 定义一个多参数的 函子 *)
module F = 
	functor (X1 : B) ->
		functor (X2 : B) ->
	struct
		let c = X1.b + X2.b 
end ;;

(* 类型为： nodule F ： functor （X1 : B） -> functor (X2 : 	B) -> sig val c : int end  *)

(* 这里来使用起来 *)

(* 定义一个B的实现 模块 B1 和 B2 *)

module B1 = struct let b = 1 end;;

module B2 = struct let b = 2 end;;

(* 使用 F B1 B2 生成一个新的模块 C *)
module C = F(B1)(B2);;

(* 类型 module C  : sig val c : int end *)

C.c ;; (* -：int = 3 *)



(* 
##################
##################

模块和第一类值的互转

##################
##################

 *)

 module type TestSig = 
 	sig 
 		type  ele 
 		val add1 : ele -> ele 
 end;;
 
 module Test = 
 	struct 
 		type ele = float
 		let add1 （x : float） ： float = x +. 1.
 end;;	
 
(* 根据 模块接口和模块转成 第一类值 *)
let mT1 = (module Test : TestSig);;

(* 第一类值 转回 模块 *)

module T1 = (val mT1 : TestSig);;

(* 
###################
###################

动态构造 模块

##################
##################

 *)

module type TestSigBak = 
	sig 
		type ele
		val add1 : float -> float
end;;


let mT3 = (module  Test : TestSigBak);;  (* val mT3 : (module TestSigBak) = <module> *)		


let mkTest m = let module M = (val m : TestSigBak) in
					
					(module struct 
						type ele = float * float 
						let add1 (a, b) = (M.add1 a, b)
					
					end : TestSig);;
(* 上述类型为： val mkTest : (module TestSigBak) -> (module TestSig) = <fun>  *)

let mTpair = mkTest mT3;;  (* 类型为： val mTpair : (module TestSig) = <module> *)

module = Tpair = (val mTpair : TestSig);; (* module Tpair : TestSig *)



(* 
#####################
#####################

定义  私有抽象类型 (伪抽象)

#####################
#####################
 *)

 (* 接口 *)
 module  type HourSig = 
 	sig 
 		type hour = private int (* 这个就是 私有抽象类型  hour  *)
 		val zero : hour    (* 一个 hour 类型的变量 zero *)
 		val inc : hour -> hour
 end ;;
 
(* 模块 *)
module Hour : HourSig = 
	struct
		type hour = int
		let zero = 0
		let inc n = (n + 1) mod 24
end ;;
(*  使用 *)

let start_hour = Hour.zero  (* val start_hour : Hour.hour = 0 *)

let next_hour = Hour.inc start_hour;; (* val next_hour : Hour.hour = 1 *)

(* 这里的 inc 不能直接作用在 int 类型上，而是只能作用在 hour 类型上 *)

(* 
&&&&&&&&&&&&&&&&&&&&&
&&&&&&&&&&&&&&&&&&&&&
类型强转 
&&&&&&&&&&&&&&&&&&&&&
&&&&&&&&&&&&&&&&&&&&&
 *)

let start_int = (start_hour :> int) ;; (* :> 类型强转 *)

(start_hour :> int) = 0 ;; (* 结构化比较   - : bool = true *)


(* 
#####################
#####################

定义  局部抽象类型 (用在 func  中)

#####################
#####################
 *)

 let sort_uniq (type ele) (cem : ele -> ele -> int) (li : ele list) = 
 	let module S = Set.Make(struct 
 								type t = ele 
 								let compare = cmp
 							end) in 
 		S.elements (List.fold_right S.add li S.empty);;

 (* 类型： val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list = <fun> *)
 (* 这个函数 入参 一个 某类型的比较func  和 一个某类型的 list， 将list去重 *)

 let sorted_charList = sort_uniq Char.compare ['a'; 'b'; 'a'; 'c'; 'z'; 'z'] ;;
 (* val sorted_charList : Char.t list = ['a'; 'b'; 'c'; 'z']  *)


(* 
######################
######################

如果自定义了 模块， 那么我们可以基于已有的模块 推出她的 接口类型

######################
######################
 *)

 module type TestSig4 = module type of Test;; (* 其中 test 是模块的类型 *)
 (* 类型为： module type TestSig4 = sig type ele = float val add1 : float -> float end *)

 module type TestSig5 = module type of Test with type ele := float;;  
 (* 类型： module type TestSig5 = sig val add1 : float -> float *)

 module type TestSig6 = 
 	sig
 		include module type of struct include Test end with type ele := float 
end;;
(* 类型: module type TestSig6 = sig val add1 : float -> float end *)





