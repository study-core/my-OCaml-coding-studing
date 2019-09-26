(* 
定义 引用变量
 *)

 let a = ref 0;;
 (* 类型为： val  a : int ref = {contents = 0} *)

 (* 赋值 *)
 a := 5;;

(* 读取 *)
let b = !a;;


(* 可更改的记录分量 *)
type myPen {
	mutable color : myColor;
	mutable width : int;
}


(* 定义颜色枚举 *)
type myColor = Red | Blue | Green | Yollow | Purple | Pink

(* 实例化一只画笔 *)
let mPen : myPen = {
	color = Red;
	width = 1;
};;

(* 修改画笔颜色 *)
mPen.color <- Yollow;;


(* 一个函数 两个 显示类型的 入参 *)
let color_change (pen : myPen) (color : myColor) = 
	pen.color <- color;;

(* 调用 *)
color_change mPen Purple;;


(* 数组 *)
let arr = [|1; 3; 5; 7|] ;;
(* 类型： val arr : int array = [|1; 3; 5; 7|] *)

(* 使用 *)

let ele = arr.(2);;

arr.(2) <- 8;;

(* 一个函数 *)
let arr_get (arr : 'a array) (i : int) = 

	try 
		arr.(i)
	with Invalid_argument _ ->
		let err = Printf.sprintf "Array access at %i is invalid" i  in failwith err;; (* failwith 为 抛出一个异常 关键字 *)
(* 类型： val arr_get : 'a array -> int -> 'a = <fun> *)

