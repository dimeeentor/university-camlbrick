
#use "CPtest.ml";;
#load "CPtestfonc.cma";;
open CPtestfonc;;
#use "camlbrick.ml";;

(**
  @autor Hasna AMID
  @return teste si la fonction make_vec2 fonctionne normalement avec les valeurs attendues
*)
let test_fonc_make_vec2 () : unit =
  let res : t_vec2 t_test_result =
     test_exec(make_vec2, "test avec positive values", (9, 3)) in
  assert_equals_result_m("Test dx, dy", {dx = 9; dy = 3}, res)
;;

(**
  @autor Dmytro HONCHARENKO
  @return teste si la fonction vec2_add fonctionne normalement avec les valeurs attendues
*)
let test_fonc_vec2_add () : unit =
  let vec1 = {dx = 5; dy = 1 } in
  let vec2 = {dx = 2; dy = 3 } in
  let res : t_vec2 t_test_result =
    test_exec(vec2_add, "test de l'addition de vecteurs",  (vec1,vec2)) in
  assert_equals_result_m ("Test résultat de l'addition", {dx = 7; dy = 4}, res)
;;

(**
      @autor Aya GUEMMIE
      @return teste si la fonction vec2_sub fonctionne normalement avec les valeurs attendues
*)
let test_fonc_vec2_add_scalar () : unit =
    let vec = { dx = 1; dy = 2 } in
    let scalar_x = 3 in
    let scalar_y = 4 in
    let res : t_vec2 t_test_result =
      test_exec (vec2_add_scalar, "test d'addition scalaire au vecteur", (vec, scalar_x, scalar_y)) in
    assert_equals_result_m ("ajout scalaire x", {dx = 4; dy = 6} ,res)
  ;;

(**
  crée vecteur1,2 avec les valeurs. Si le résultat ne correspond pas au résultat attendu {dx = 3; dy = 18},
  @autor Edouard GONET
  @return teste si la fonction vec2_mult fonctionne normalement avec les valeurs attendues
*)
  let test_fonc_vec2_mult () : unit =
    let vec1 = { dx = 1; dy = 2 } in
    let vec2 = { dx = 3; dy = 9 } in
    let res : t_vec2 t_test_result =
      test_exec (vec2_mult, "test de multiplication de vecteurs", (vec1, vec2)) in
    assert_equals_result_m ("résultat de la multiplication", { dx = 3; dy = 18 }, res)
    ;;

(**
  crée un vecteur v et scalar_x, scalar_y. Si le résultat ne correspond pas au résultat attendu
  {dx = 18; dy = 21}, cela déclenchera un échec d'assertion.
  @autor Aya GUEMMIE
  @return teste si la fonction vec2_mult_scalar fonctionne normalement avec les valeurs attendues
*)
let test_fonc_vec2_mult_scalar () : unit =
      let vec = { dx = 3; dy = 3 } in
      let scalar_x = 6 in
      let scalar_y = 7 in
      let res : t_vec2 t_test_result =
        test_exec (vec2_mult_scalar, "test de multiplication scalaire au vecteur", (vec, scalar_x, scalar_y)) in
      assert_equals_result_m ("multiplication scalaire", { dx = 18; dy = 21 }, res);;

(**
  param_get prend en entrée un jeu et renvoie les paramètres du jeu.
  @autor Hasna AMID
  @return teste si la fonction param_get fonctionne normalement avec les valeurs attendues
*)
let test_fonc_param_get () : unit =
  let params = make_camlbrick_param () in
  let game = { params = params; ball = {position = make_vec2(0,0); velocity = make_vec2(0,0); size = BS_MEDIUM}; paddle = {
    position = make_vec2(0, 0);
    size = PS_SMALL;
    width = 0;
    height = 0;
  }; bricks = [|[||]|]; score = 0; state = GAMEOVER } in
    let expected_result = params in
    let res : t_camlbrick_param t_test_result =
          test_exec (param_get, "récupération des paramètres du jeu", game) in
        assert_equals_result_m ("récupération des paramètres du jeu", expected_result, res);;

(*------------------------------------------------
   | Test make camlbrick |
------------------------------------------------*)
(**
    La position initiale de la balle est (0, 0).
    La vitesse initiale de la balle est (0, 0).
    La taille de la balle est fixée à [BS_MEDIUM].
    La position initiale de la raquette est (0, 0).
    La taille de la raquette est fixée à [PS_SMALL].
    La largeur de la raquette est de 0.
    La hauteur du paddle est de 0.
    Le tableau de briques est vide.
    Le score est de 0.
    L'état du jeu est [GAMEOVER]
    @autor Dmytro HONCHARENKO
    @return teste si la fonction make_camlbrick fonctionne normalement avec les valeurs attendues
 *)
let test_make_camlbrick () : unit =
  let brick = make_camlbrick () in
  assert_equal (make_vec2 0 0) brick.ball.position;
  assert_equal (make_vec2 0 0) brick.ball.velocity;
  assert_equal BS_MEDIUM brick.ball.size;
  assert_equal (make_vec2 0 0) brick.paddle.position;
  assert_equal PS_SMALL brick.paddle.size;
  assert_equal 0 brick.paddle.width;
  assert_equal 0 brick.paddle.height;
  assert_equal [|[||]|] brick.bricks;
  assert_equal 0 brick.score;
  assert_equal GAMEOVER brick.state
;;

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

  @author Dmytro HONCHARENKO
  @author Edouard GONET
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
