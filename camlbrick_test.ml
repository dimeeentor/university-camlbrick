
#use "CPtest.ml";;

#load "CPtestfonc.cma";;
open CPtestfonc;;
#use "camlbrick.ml";;


(*------------------------------------------------| Test brick_get |------------------------------------------------*)

 let test_brick_get () : unit = 

  let res_1 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(0, 0)", ([|[| BK_empty|]; [|BK_empty|]|], 0, 0)) in 
  assert_equals_result(BK_empty, res_1); 
  assert_true(test_is_success(res_1));
  
  let res_2 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(1, 1)", ([|[| BK_empty|]; [|BK_empty|]|], 1, 1)) in
  assert_equals_result(BK_empty, res_2); 
  assert_true(test_is_success(res_2));

  let res_3 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(-1, 0)", ([|[| BK_empty|]; [|BK_empty|]|], -1, 0)) in 
  assert_equals_result(BK_empty, res_3); 
  assert_true(test_is_success(res_3));

  let res_4 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(0, -1)", ([|[| BK_empty|]; [|BK_empty|]|], 0, -1)) in 
  assert_equals_result(BK_empty, res_4); 
  assert_true(test_is_success(res_4));

  let res_5 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(2, 0)", ([|[| BK_empty|]; [|BK_empty|]|], 2, 0)) in 
  assert_equals_result(BK_empty, res_5); 
  assert_true(test_is_success(res_5));

  let res_6 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(0, 2)", ([|[| BK_empty|]; [|BK_empty|]|], 0, 2)) in 
  assert_equals_result(BK_empty, res_6); 
  assert_true(test_is_success(res_6))
 ;; 

(*------------------------------------------------| Test Brick_hit |------------------------------------------------*)

let test_brick_hit () : unit = 

  let res_1 : t_brick_kind t_test_result = test_exec(brick_hit, "brick_hit(0, 0)", ([|[|BK_simple; BK_double|]; [|BK_bonus; BK_block|]|], 0, 0)) in 
    assert_equals_result(BK_empty, res_1);
    assert_true(test_is_success(res_1));

  let res_2 : t_brick_kind t_test_result = test_exec(brick_hit, "brick_hit(1, 1)", ([|[|BK_block; BK_bonus|]; [|BK_simple; BK_double|]|], 1, 1)) in 
  assert_equals_result(BK_simple, res_2);
  assert_true(test_is_success(res_2))
;; 

(*------------------------------------------------| Test Brick_color |------------------------------------------------*)

let test_brick_color () : unit = 

  let res_1 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(0, 0)", ([|[|BK_simple; BK_double|]; [|BK_bonus; BK_block|]|], 0, 0)) in 
    assert_equals_result(YELLOW, res_1); 
    assert_true(test_is_success(res_1));

    let res_2 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(1, 1)", ([|[|BK_block; BK_bonus|]; [|BK_simple; BK_double|]|], 1, 1)) in 
    assert_equals_result(BLUE, res_2); 
    assert_true(test_is_success(res_2));

    let res_3 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(0, 1)", ([|[|BK_double; BK_simple|]; [|BK_block; BK_bonus|]|], 0, 1)) in 
    assert_equals_result(BLUE, res_3); 
    assert_true(test_is_success(res_3));
  
    let res_4 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(1, 0)", ([|[|BK_bonus; BK_block|]; [|BK_simple; BK_double|]|], 1, 0)) in 
    assert_equals_result(GREEN, res_4); 
    assert_true(test_is_success(res_4));
  
    (*indices en dehors des limites *)
    let res_5 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(-1, 0)", ([|[|BK_bonus; BK_block|]; [|BK_simple; BK_double|]|], -1, 0)) in 
    assert_notequals_result(BLACK, res_5); 
    assert_true(test_is_success(res_5));
  
    let res_6 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(2, 3)", ([|[|BK_bonus; BK_block|]; [|BK_simple; BK_double|]|], 2, 3)) in 
    assert_notequals_result(BLACK, res_6); 
    assert_true(test_is_success(res_6))
  ;;


(*------------------------------------------------| Test report |------------------------------------------------*)

test_reset_report();;

test_brick_get ();; 
test_brick_hit ();;
test_brick_color ();;

test_report();;