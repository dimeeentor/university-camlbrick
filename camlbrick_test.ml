
#use "CPtest.ml";;
#load "CPtestfonc.cma";;
open CPtestfonc;;
#use "camlbrick.ml";;

(*------------------------------------------------
   | Test brick_get |
------------------------------------------------*)

(**
  La fonction test_brick_get teste la fonction brick_get, qui prend en entrée une matrice de briques game ainsi que les 
  coordonnées (i, j) d'une brique dans cette matrice, et renvoie le type de brique à ces coordonnées. Ces tests brick_get 
  dans différentes situations, notamment lorsque les coordonnées pointent vers des briques valides, des briques invalides 
  ou des briques en dehors de la matrice.

  @author Edouard GONET
  @return teste si la fonction brick_get fonctionne normalemnt avec les valeurs attendues 
*)

 let test_brick_get () : unit = 

  (* Cas 1: Coordonnées valides pointant vers une brique vide *)
  let res_1 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(0, 0)", ([|[| BK_empty|]; [|BK_empty|]|], 0, 0)) in 
  assert_equals_result(BK_empty, res_1); 
  assert_true(test_is_success(res_1));
  
  (* Cas 2: Coordonnées valides pointant vers une brique vide *)
  let res_2 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(1, 1)", ([|[| BK_empty|]; [|BK_empty|]|], 1, 1)) in
  assert_equals_result(BK_empty, res_2); 
  assert_true(test_is_success(res_2));

  (* Cas 3: Coordonnées invalides (indice négatif) *)
  let res_3 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(-1, 0)", ([|[| BK_empty|]; [|BK_empty|]|], -1, 0)) in 
  assert_equals_result(BK_empty, res_3); 
  assert_true(test_is_success(res_3));

  (* Cas 4: Coordonnées invalides (indice négatif) *)
  let res_4 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(0, -1)", ([|[| BK_empty|]; [|BK_empty|]|], 0, -1)) in 
  assert_equals_result(BK_empty, res_4); 
  assert_true(test_is_success(res_4));

  (* Cas 5: Coordonnées pointant vers une brique en dehors de la matrice *)
  let res_5 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(2, 0)", ([|[| BK_empty|]; [|BK_empty|]|], 2, 0)) in 
  assert_equals_result(BK_empty, res_5); 
  assert_true(test_is_success(res_5));

  (* Cas 6: Coordonnées pointant vers une brique en dehors de la matrice *)
  let res_6 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(0, 2)", ([|[| BK_empty|]; [|BK_empty|]|], 0, 2)) in 
  assert_equals_result(BK_empty, res_6); 
  assert_true(test_is_success(res_6))
 ;; 

(*------------------------------------------------
   | Test Brick_hit |
------------------------------------------------*)

(**
  La fonction test_brick_hit teste brick_hit avec deux cas de test différents. Le premier cas teste une brique simple et 
  vérifie si elle devient une brique vide après avoir été touchée. Le deuxième cas teste une brique double et vérifie si 
  elle devient une brique simple après avoir été touchée.

  @author Dmytro HONCHARENKO
  @return teste si la fonction brick_hit fonctionne normalement avec les valeurs attendues 
*)

let test_brick_hit () : unit = 

  (* Cas 1: Brique simple touchée, devrait devenir une brique vide *)
  let res_1 : t_brick_kind t_test_result = test_exec(brick_hit, "brick_hit(0, 0)", ([|[|BK_simple; BK_double|]; [|BK_bonus; BK_block|]|], 0, 0)) in 
    assert_equals_result(BK_empty, res_1);
    assert_true(test_is_success(res_1));

  (* Cas de test 2: Brique double touchée, devrait devenir une brique simple *)
  let res_2 : t_brick_kind t_test_result = test_exec(brick_hit, "brick_hit(1, 1)", ([|[|BK_block; BK_bonus|]; [|BK_simple; BK_double|]|], 1, 1)) in 
  assert_equals_result(BK_simple, res_2);
  assert_true(test_is_success(res_2))
;; 

(*------------------------------------------------
   | Test Brick_color |
------------------------------------------------*)

(**
  Cette fonction teste la fonction brick_color avec plusieurs cas de test pour différents types de briques. 
  Chaque cas de test vérifie si la couleur retournée par brick_color correspond à celle attendue pour une brique spécifique 
  à des coordonnées données.

  @author Edouard GONET 
  @return teste si la fonction brick_color fonctionne normalement avec les valeurs attendues    
*)

let test_brick_color () : unit = 

  (* Cas 1: Brique simple *)
  let res_1 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(0, 0)", ([|[|BK_simple; BK_double|]; [|BK_bonus; BK_block|]|], 0, 0)) in 
  assert_equals_result(YELLOW, res_1); 
  assert_true(test_is_success(res_1));

  (* Cas 2: Brique double *)
  let res_2 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(1, 1)", ([|[|BK_block; BK_bonus|]; [|BK_simple; BK_double|]|], 1, 1)) in 
  assert_equals_result(BLUE, res_2); 
  assert_true(test_is_success(res_2));

  (* Cas 3: Brique double différente *)
  let res_3 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(0, 1)", ([|[|BK_double; BK_simple|]; [|BK_block; BK_bonus|]|], 0, 1)) in 
  assert_notequals_result(BLUE, res_3); 
  assert_true(test_is_success(res_3));
  
  (* Cas 4: Brique simple différente *)
  let res_4 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(1, 0)", ([|[|BK_bonus; BK_block|]; [|BK_simple; BK_double|]|], 1, 0)) in 
  assert_notequals_result(GREEN, res_4); 
  assert_true(test_is_success(res_4))
;; 

(*------------------------------------------------
   | Test string_of_gamestate |
------------------------------------------------*)
(** 
  La fonction test_string_of_gamestate teste la fonction string_of_gamestate. Elle vérifie si la représentation sous forme de chaîne de l'état du jeu correspond aux valeurs attendues pour les différents paramètres du jeu. Elle vérifie également que l'état du jeu est correctement mis à jour après modification des paramètres.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @return teste si la fonction string_of_gamestate fonctionne normalement avec les valeurs attendues
*)

let test_string_of_gamestate (status : t_test_status) : unit =

  let const : t_camlbrick = {
                              kind = BK_empty;  
                              color = BLACK;    
                              param = {
                                        world_width = 800;
                                        world_bricks_height = 600;
                                        world_empty_height = 200;
                                        
                                        brick_width = 40;
                                        brick_height = 20;
                                        
                                        paddle_init_width = 100;
                                        paddle_init_height = 20;
                                        
                                        time_speed = ref 20;
                                      }          
                            } in 
  let test_result_param : string t_test_result = test_exec(string_of_gamestate, "test_string_of_gamestate", const) in
  let test_result_world_width : string t_test_result = test_exec(string_of_gamestate, "test_string_of_gamestate", const) in
  let test_result_bricks_height : string t_test_result = test_exec(string_of_gamestate, "test_string_of_gamestate", const) in
  let test_result_empty_height : string t_test_result = test_exec(string_of_gamestate, "test_string_of_gamestate", const) in
  let test_result_brick_width : string t_test_result = test_exec(string_of_gamestate, "test_string_of_gamestate", const) in
  let test_result_bricks_height : string t_test_result = test_exec(string_of_gamestate, "test_string_of_gamestate", const) in
  let test_result_paddle_init_width : string t_test_result = test_exec(string_of_gamestate, "test_string_of_gamestate", const) in
  let test_result_paddle_init_height : string t_test_result = test_exec(string_of_gamestate, "test_string_of_gamestate", const) in
  let test_result_time_speed : string t_test_result = test_exec(string_of_gamestate, "test_string_of_gamestate", const) in
    ( 
      if test_is_success(test_result_param) && test_is_success(test_result_world_width) && test_is_success(test_result_bricks_height) && test_is_success(test_result_empty_height) && test_is_success(test_result_brick_width) && test_is_success(test_result_bricks_height) && test_is_success(test_result_paddle_init_width) && test_is_success(test_result_paddle_init_height) && test_is_success(test_result_time_speed)
      then 
        (
          let result_2 = string_of_gamestate game_2 in
          assert_true (String.contains result_2 "World width: 1000");
          assert_true (String.contains result_2 "World bricks height: 800");
          assert_true (String.contains result_2 "World empty height: 300");
          assert_true (String.contains result_2 "Brick width: 50");
          assert_true (String.contains result_2 "Brick height: 30");
          assert_true (String.contains result_2 "Paddle init width: 120");
          assert_true (String.contains result_2 "Paddle init height: 25");
          assert_true (String.contains result_2 "Time speed: 15");
        )
      else
        assert_failwith(test_step);
    )
;;

(*------------------------------------------------
   | Test report |
------------------------------------------------*)

test_reset_report();;

test_brick_get ();; 
test_brick_hit ();;
test_brick_color ();;

test_report();;