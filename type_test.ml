

(* 
类型定义

type 类型参数（可选的，即 泛型)   类型名 = 类型表达式
 *)

 type positive = int

(* 定义一个具有 泛型 'a 的类型 typeName *)
 type 'a typeName = 'a * 'a
(* 含有多个 泛型的 类型为 *)
type ('a, 'b) typeName = {value: 'a; key: 'b}
(* type (泛型1， 泛型2， ..., 泛型n)  变量名 = 类型表达式 *)



 (* 根据类型，显示定义变量 *)
 let args ： positive = 12 + 12

 (* 显示类型的定义函数 *)
 let myFunc (args: positive)  = args + 14


(* 定义具备显示类型的返回值函数 *)
let myFunc inArgs : positive = args + 12
(* 指定 函数表达式的类型 *)
let myFunc inArgs = （2.0 : float)

(* 定义一个显示声明类型的函数 *)
let myFunc （x : int） = （if x < 0 then -x else x : positive）
(* 或者 写成 *)
let myFunc （x : int）: positive = if x < 0 then -x else x ;;



(* 
定义一个 记录类型

即: 结构体
 *)

type typeName = {name: string; age: int; gender: int }

(* 实例化为: 系统会自动判断出是 typeName 类型 *)

let xiaoming = {name = "小明"; age = 12; gender = 1}


(* 

联合类型

类似 枚举
 *)

 (* 
 如自定义季节 联合类型， 由 4 个构造子组成
分别是两个无参 构造子 和两个 有参构造子 组成哦
  *)
type season = Spring | Summer | Autumn of int | Winter of float


(* 具备 多态变体的 构造子 , 直接定义即可 直接用 ，无需事先声明*)

`AName ;; (* 类型为 [>`AName] *)

`BName ("我的名字", 12);; (*类型为： [>`BName  of string * int]*) 



(* 我们来写一个具备多个 不同参数构造子的 联合类型入参 函数 *)

type num = MyInt of int | MyFloat of float

(* 
注意: function 定义的匿名函数只能有一个入参
和 func 定义的匿名函数有区别哦
请自行查看 function 和 func 定义 匿名函数的 定义
 *)
let addFunc = function （MyInt args1, MyInt args2）-> MyInt (args1 + args2) | （MyInt args1, MyFloat args2）-> MyFloat ((float_of_int args1) +. args2)
						|(MyFloat args1, MyInt args2) -> MyFloat (args1 +. (float_of_int args2)) | (MyFloat args1, MyFloat args2) -> MyFloat (args1 +. args2)

(* 
上面这个就是 入参为 num 联合类型的元组类型 的一个 匿名函数的定义 
整个函数类型为
val addFunc : num * num -> num = <func>
*)

(* 联合类型的其他用处
定义一颗 树
 *)

 type myTree = Leaf of int | Node of myTree * myTree

 (* 加上 泛型就是 :  type  'a myTree = Leaf of 'a | Node of 'a myTree * 'a myTree*)

 (* 实例化一棵树 *)

 Node (Leaf 3, Node (Leaf 4, Leaf 5));; 
 (* 

这其实是这样的一棵树:

		
			/\
		   /  \	
		  3   /\
    		 /  \
			4	 5
  *)


  (* 计算书上节点之和 *)

  let rec leafTotalFunc = function (Leaf n) -> n | (Node (leaf1, leaf2)) -> leafTotalFunc (leaf1) + leafTotalFunc(leaf2) ;;  
  (*类型为: val leafTotalFunc : myTree -> int = <func> *)




(* 定义一个 option 类型 【一个联合类型哦】*)

type 'a option = None | Some of 'a ;;

(* 一个具备 option 返回的函数 *)

let rec optonFunc flag alist = 
	match alist with
	 | hd :: tl -> if flag then Some hd else optonFunc flag tl
	 | [] -> None ;; 
(* 上述 函数 类型为: val optonFunc : ('a -> bool) -> 'a list -> 'a option = <func> 注意 条件 flag 被看成了是由某个表达式或者函数产生的哦  ('a -> bool) *)





(* 又如， 在一个关联表中寻找 某个元素 v 对应的值 y ; 关联表是由 对偶组成的表， 类型为  ('a, 'b) list *)

let myFunc  v alist = 
	let va = optionFunc (function (x, y) -> x = v ) alist in 
		match va  with
		| Some (x, y) -> Some y 
		| None -> None;;

(* 上述函数类型为: val  myFunc :  'a -> ('a * 'b) list -> 'b option = <func> *)







(* list  表 *)
type 'a list = Nil | Cons of 'a * 'a list;;  (* Cons of 'a * 'a list  == ('a, 'a) *)

(* 实例化一个 list *)
3 :: [] ;; (*将3 加到 [] 中 *) 
(*或者 *)
[3; 4] 
(*或者 *)
[[1; 2]; [3; 4]] (*类型为： int list list *)


let alist = [1; 4; 7] ;;

(* 读 首元素 *)
let first = List.hd alist ;;

(* 移除 首元素 *)
let second = List.tl alist;; (* 返回值为: [4; 7] *)

(* 读最后一个元素 *)
let last = match alist with
| [_; _; x] -> x
| _ -> 0;; (* 返回了 7 *)














(* 创建一个 map[string]string *)
module MyUsers = Map.Make(String);;
(* 现在已经创建了一个新的模块，叫做 MyUsers *)
(* 首先，我们先 新建一个空的映射
	类型为:
	val m : 'a MyUsers.t = <abstr>
*)

let m = MyUsers.empty;;

  (* 再往里面加数据
	类型变化为:
	val m : string MyUsers.t = <abstr>
*)
let m = MyUsers.add "fred" "sugarplums" m;;
   (* 
   m是一个全新的映射，
   因此前一个m已经被隐藏掉。
   这个m比前一个多了用户 "fred" 和他的密码 "sugarplums"。

   有一点很值得指出的是，当我们加入字符串 "sugarplums" 的时候，
   我们已经固定了映射的目标类型。这也就是说，
   我们的模块MyUsers成为了一个只能从字符串到字符串 的映射。
   如果我们想插入一个整数作为键还是值，都必须创建一个新的映射。 
*)

(* 
定义一个函数，查看 map 中的数据
函数类型为:

val print_users : string -> string -> unit = <fun>
*)
let print_users key password =
	print_string(key ^ " " ^ password ^ "\n");;


(* 

使用该函数。

这里不明白为什么该函数 是个中间操作符 ?
*)

MyUsers.iter print_users m;;


(* 返回某个 key 对应的 value

- : string = "sugarplums"
*)
MyUsers.find "fred" m;;





(* 构建一个 元素可变的 list *)
type list = Nil | Cons of cell
  and cell = { mutable hd : int; tl : list };;



(* 构建一个 引用可变的 list *)
 type list = Nil | Cons of cell
  and cell = {mutable hd : int; mutable tl : list};;








(* 定义一个指针 *)

type 'a pointer = Null | Pointer of 'a ref;;

(* 
用 !^ 来解引用，
和一个中缀操作符  ^:=  来赋值
 *)
let ( !^ ) = function
    | Null -> invalid_arg "Attempt to dereference the null pointer"
    | Pointer r -> !r;;

(* 类型  *)
val ( !^ ) : 'a pointer -> 'a = <fun>



let ( ^:= ) p v =
    match p with
    | Null -> invalid_arg "Attempt to assign the null pointer"
    | Pointer r -> r := v;;

(* 类型  *)    
val ( ^:= ) : 'a pointer -> 'a -> unit = <fun>




(* 定义一个指针的分配和初始化 *)
let new_pointer x = Pointer (ref x);;

(* 类型  *) 
val new_pointer : 'a -> 'a pointer = <fun>


(* 调用函数， 返回一个指针 *)
let p = new_pointer 0;;
(* 类型 *)
val p : int pointer = Pointer {contents = 0}
(* 给指针解引用，并赋值 *)
p ^:= 1;;

(* 类型 *)
- : unit = ()

(* 读 解引用 *)
!^p;;

(* 类型 *)
- : int = 1




(* 用指针定义链表 *)
type ilist = cell pointer
  and cell = {mutable hd : int; mutable tl : ilist};;
































 (* 
声明一个 Set 模块 ?
*)


module SS = Set.Make(String);;

(* 
新建一个空集 
类型为

val s : SS.t = <abstr>
*)

let s = SS.empty;;

(* 创建只包含一个元素的 集合，类型同上 *)
let s = SS.singleton "hello";;


(* 添加元素 *)

let s = List.fold_right SS.add ["hello"; "world"; "community"; "manager"; "stuff"; "blue"; "green"] s;;


(* 
通过remove函数移除某个元素。
使用filter 移除很多元素的时候
*)

(*
filter掉长度大于5的字符串
val my_filter : string -> bool = <fun>
*)

let my_filter str = String.length str <= 5;;
let s2 = SS.filter my_filter s;;

(* 或者 用匿名函数*)
let s2 = SS.filter (fun str -> String.length str <= 5) s;;

(* 查看某元素是否存在     - : bool = true *)
SS.mem "hello" s2;; 



(* Hash 表 *)
let my_hash = Hashtbl.create 123456;;  (* 123456 是 长度的意思 *)

(* 类型： 
val my_hash : ('_a, '_b) Hashtbl.t = <abstr> 
其中 '_a 和 '_b 分别是键和值的类型
这里的下划线表示如果类型一经确定就会被固定下来

和Map不一样的是，哈希表是直接更新数据结构，而不是每次都新建一个表

*)


(* 一次性往 Hash表中追加 N 对键值对 *)
Hashtbl.add my_hash "h" "hello";
Hashtbl.add my_hash "h" "hi";
Hashtbl.add my_hash "h" "hug";
Hashtbl.add my_hash "h" "hard";
Hashtbl.add my_hash "w" "wimp";
Hashtbl.add my_hash "w" "world";
Hashtbl.add my_hash "w" "wine";; (* 注意： 只有最后一个 键值对有用了 ;;  这里表达式的类型为：  unit = () *)



(* 找出my_hash中"h"对应的元素 语句只返回加入my_mash的最后一个元素*)
Hashtbl.find my_hash "h";;
(* 类型为： *)
- : string = "hard"


(* 返回所有 键为 h 的元素 *)
 Hashtbl.find_all my_hash "h";;
 (* 类型为   从后往前 排序 *)
- : string list = ["hard"; "hug"; "hi"; "hello"]


(* 检查是否存在 某个字母时 *)
Hashtbl.mem my_hash "h";;
(* 类型为 *)
- : bool = true

















(* failwith "error message"来抛出一个Failure异常 *)
match Sys.os_type with
| "Unix" | "Cygwin" ->   (* code omitted *)
| "Win32" ->             (* code omitted *)
| "MacOS" ->             (* code omitted *)
| _ -> failwith "this system is not supported"