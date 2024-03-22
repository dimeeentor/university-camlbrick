#use "CPtest.ml";; 
#load "CPtestfonc.cma";;
open CPtestfonc;;

test_reset_report();;

type t_vec2 = {dx : int ; dy : int};;

let make_vec2(x,y : int * int) : t_vec2 = 
  {dx = x ; dy = y}
;;

let test_make_vec2 () : unit =
  let res : t_vec2 = test_exec(make_vec2, "test avec coordonés 3, 5" ,(3,5)) in
  assert (res = (3,5))
;;
  
test_report();;  
