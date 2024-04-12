#use "CPtest.ml";;
#use "camlbrick.ml";;

(**
  [test_balls_get] est une fonction de test unitaire qui teste la fonction [balls_get].
  Il crée un jeu de test avec deux balles et appelle la fonction [balls_get] pour récupérer les balles.
  Les balles récupérées sont ensuite comparées au résultat attendu à l'aide de la fonction [assert_equals_result].
  Enfin, il affirme que le test est réussi en utilisant la fonction [assert_true].

  @author Dmytro HONCHARENKO
  @return Cette fonction ne renvoie pas de valeur.
*)
let test_balls_get() : unit =
  let game = {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(1, 1));
      velocity = 0; 
      size = BS_MEDIUM;
    };
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  let res : t_ball list t_test_result = test_exec(balls_get,"test balls_get", game) in
  assert_equals_result ([game.ball], res)
;;

(**
  [test_ball_get] est une fonction de test unitaire qui teste la fonction [ball_get].

  @author Dmytro HONCHARENKO
*)
let test_ball_get() : unit = 
  let game = {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(1, 1));
      velocity = 0; 
      size = BS_MEDIUM;
    };
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  let res : t_ball t_test_result = test_exec(ball_get,"test ball_get", (game, 1)) in
  assert_equals_result (game.ball, res)
;;

(**
  [test_ball_x] est une fonction de test unitaire qui teste la fonction [ball_x].
  Il crée un jeu avec deux balles et vérifie la coordonnée X de la première balle.
  Le résultat attendu est 0.

  @author Dmytro HONCHARENKO
*)
let test_ball_x() : unit =
  let game = {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(1, 1));
      velocity = 0; 
      size = BS_MEDIUM;
    };
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  let res : int t_test_result = test_exec(ball_x, "test ball_x", (game, game.ball)) in
  assert_equals_result (1, res);;
;;

(**
  [test_ball_y] is a unit test function that tests the [ball_y] function.

  @author Dmytro HONCHARENKO
*)
let test_ball_y() : unit =
  let game = {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(1, 1));
      velocity = 0; 
      size = BS_MEDIUM;
    };
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  let res : int t_test_result = test_exec(ball_x, "test for ball_y", (game, game.ball)) in
  assert_equals_result (1, res);;

(**
  [test_ball_size_pixel] est une fonction de test unitaire qui teste la fonction [ball_size_pixel].
  Il crée un jeu avec deux balles, chacune avec des positions, des cadres, des tailles et des couleurs différents.
  Il récupère ensuite la première balle du jeu et calcule sa taille en pixels à l'aide de la fonction [ball_size_pixel].
  La taille calculée est comparée à la taille attendue à l'aide de la fonction [assert_equals_result].
  Enfin, il affirme que le test est réussi en utilisant la fonction [assert_true].

  @author Dmytro HONCHARENKO
*)
let test_ball_size_pixel() : unit =
  let game = {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(1, 1));
      velocity = 0; 
      size = BS_SMALL;
    };
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  let res : int t_test_result = test_exec(ball_size_pixel, "test ball_size_pixel", (game, game.ball)) in
  assert_equals_result (10, res);;

(**
  [test_ball_color] est une fonction de test unitaire qui teste la fonction [ball_color].
  Il crée un état de jeu avec deux boules, l'une de couleur BLANCHE et l'autre de couleur BLEUE.
  Il appelle ensuite la fonction [ball_color] avec l'état du jeu et la première balle.
  Le résultat attendu est BLANC.
  La fonction utilise les fonctions [assert_equals_result] et [assert_true] pour vérifier le résultat.

  @author Dmytro HONCHARENKO
*)
let test_ball_color() : unit = 
  let ball = {
    position = ref (make_vec2(0, 0));
    velocity = 0;
    size = BS_SMALL;
    } in
  let game = {
    params = make_camlbrick_param ();
    ball = ball;
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
    } in
  let res : color t_test_result = test_exec(ball_color, "test avec balle SMALL", (game, ball)) in
  assert_equals_result (YELLOW, res);
  ball.size <- BS_MEDIUM;
  let res : color t_test_result = test_exec(ball_color, "test avec balle MEDIUM", (game, ball)) in
  assert_equals_result (ORANGE, res);
  ball.size <- BS_BIG;
  let res : color t_test_result = test_exec(ball_color, "test avec balle BIG", (game, ball)) in
  assert_equals_result (RED, res);
;;

let run_tests () : unit =
  test_balls_get();
  test_ball_get();
  test_ball_x();
  test_ball_y();
  test_ball_size_pixel();
  test_ball_color();
;;