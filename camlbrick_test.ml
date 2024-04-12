#use "CPtest.ml";;
#load "CPtestfonc.cma";;
open CPtestfonc;;
(*#use "camlbrick.ml";;*)

(*------------------------------------------------| Test brick_get |------------------------------------------------*)

(**
  La fonction test_brick_get teste la fonction brick_get, qui prend en entrée une matrice de briques game ainsi que les 
  coordonnées (i, j) d'une brique dans cette matrice, et renvoie le type de brique à ces coordonnées. Ces tests brick_get 
  dans différentes situations, notamment lorsque les coordonnées pointent vers des briques valides, des briques invalides 
  ou des briques en dehors de la matrice.

  @author Edouard GONET
*)

let test_brick_get () : unit =


  (* Cas 1: Coordonnées valides pointant vers une brique vide *)
 
  let game_1 = {
    kind = BK_simple;
    array = [|[| BK_empty|]; [|BK_empty|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [];
    ball = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = LIME};
    pad = {position = {dx = 0; dy = 0}; size = PS_SMALL; speed = ref 3; x = ref 0; y = ref 0};
    speed = ref 5
                } in
  let res_1 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(0, 0)", (game_1, 0, 0)) in
  assert_equals_result(BK_empty, res_1);
  assert_true(test_is_success(res_1));
 
  (* Cas 2: Coordonnées valides pointant vers une brique vide *)
  let game_2 = {
    kind = BK_simple;
    array =  [|[| BK_empty|]; [|BK_empty|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = []; ball = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = LIME};
    pad = {position = {dx = 0; dy = 0}; size = PS_SMALL; speed = ref 3; x = ref 0; y = ref 0};
    speed = ref 5
                } in
  let res_2 :  t_brick_kind t_test_result = test_exec(brick_get, "brick_get(1, 1)", (game_2, 1, 1)) in
  assert_equals_result(BK_empty, res_2);
  assert_true(test_is_success(res_2));


  (* Cas 3: Coordonnées invalides (indice négatif) *)
  let game_3 = {
    kind = BK_simple;
    array = [|[| BK_empty|]; [|BK_empty|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [];
    ball = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = LIME};
    pad = {position = {dx = 0; dy = 0}; size = PS_SMALL; speed = ref 3; x = ref 0; y = ref 0};
    speed = ref 5
                } in
  let res_3 : t_brick_kind t_test_result = test_exec(brick_get, "brick_get(-1, 0)", (game_3, -1, 0)) in
  assert_equals_result(BK_empty, res_3);
  assert_true(test_is_success(res_3));


  (* Cas 4: Coordonnées invalides (indice négatif) *)
  let game_4 = {
    kind = BK_simple;
    array = [|[| BK_empty|]; [|BK_empty|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [];
    ball = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = LIME};
    pad = {position = {dx = 0; dy = 0}; size = PS_SMALL; speed = ref 3; x = ref 0; y = ref 0};
    speed = ref 5
               } in
  let res_4 :  t_brick_kind t_test_result = test_exec(brick_get, "brick_get(0, -1)", (game_4, 0, -1)) in
  assert_equals_result(BK_empty, res_4);
  assert_true(test_is_success(res_4));


  (* Cas 5: Coordonnées pointant vers une brique en dehors de la matrice *)
  let game_5 = {
    kind = BK_simple;
    array = [|[| BK_empty|]; [|BK_empty|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [];
    ball = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = LIME};
    pad = {position = {dx = 0; dy = 0}; size = PS_SMALL; speed = ref 3; x = ref 0; y = ref 0};
    speed = ref 5
                } in
  let res_5 :  t_brick_kind t_test_result = test_exec(brick_get, "brick_get(2, 0)", (game_5, 2, 0)) in
  assert_equals_result(BK_empty, res_5);
  assert_true(test_is_success(res_5));


  (* Cas 6: Coordonnées pointant vers une brique en dehors de la matrice *)
  let game_6 = {
    kind = BK_simple;
    array = [|[| BK_empty|]; [|BK_empty|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [];
    ball = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = LIME};
    pad = {position = {dx = 0; dy = 0}; size = PS_SMALL; speed = ref 3; x = ref 0; y = ref 0};
    speed = ref 5
                } in
  let res_6 :  t_brick_kind t_test_result = test_exec(brick_get, "brick_get(0, 2)", (game_6, 0, 2)) in
  assert_equals_result(BK_empty, res_6);
  assert_true(test_is_success(res_6))
 ;;


(*------------------------------------------------| Test Brick_hit |------------------------------------------------*)

(**
  La fonction test_brick_hit teste brick_hit avec deux cas de test différents. Le premier cas teste une brique simple et 
  vérifie si elle devient une brique vide après avoir été touchée. Le deuxième cas teste une brique double et vérifie si 
  elle devient une brique simple après avoir été touchée.

  @author Dmytro HONCHARENKO
*)

let test_brick_hit () : unit =


  (* Cas 1: Brique simple touchée, devrait devenir une brique vide *)
  let game_1 = {
    kind = BK_simple;
    array = [|[|BK_simple; BK_double|]; [|BK_bonus; BK_block|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [];
    ball = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = LIME};
    pad = {position = {dx = 0; dy = 0}; size = PS_SMALL; speed = ref 3; x = ref 0; y = ref 0};
    speed = ref 5
                } in
  let res_1 : t_brick_kind t_test_result = test_exec(brick_hit, "brick_hit(0, 0)", (game_1, 0, 0)) in
  assert_equals_result(BK_empty, res_1);
  assert_true(test_is_success(res_1));


  (* Cas de test 2: Brique double touchée, devrait devenir une brique simple *)
  let game_2 = {
    kind = BK_simple;
    array = [|[|BK_block; BK_bonus|]; [|BK_simple; BK_double|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [];
    ball = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = LIME};
    pad = {position = {dx = 0; dy = 0}; size = PS_SMALL; speed = ref 3; x = ref 0; y = ref 0};
    speed = ref 5
                } in
  let res_2 : t_brick_kind t_test_result = test_exec(brick_hit, "brick_hit(1, 1)", (game_2, 1, 1)) in
  assert_equals_result(BK_simple, res_2);
  assert_true(test_is_success(res_2))
;;

(*------------------------------------------------| Test Brick_color |------------------------------------------------*)

(**
  Cette fonction teste la fonction brick_color avec plusieurs cas de test pour différents types de briques. 
  Chaque cas de test vérifie si la couleur retournée par brick_color correspond à celle attendue pour une brique spécifique 
  à des coordonnées données.

  @author Edouard GONET    
*)

let test_brick_color () : unit =


  (* Cas 1: Brique simple *)
  let game_1 = {
    kind = BK_simple;
    array = [|[|BK_simple; BK_double|]; [|BK_bonus; BK_block|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [];
    ball = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = LIME};
    pad = {position = {dx = 0; dy = 0}; size = PS_SMALL; speed = ref 3; x = ref 0; y = ref 0};
    speed = ref 5
                } in
  let res_1 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(0, 0)", (game_1, 0, 0)) in
  assert_equals_result(YELLOW, res_1);
  assert_true(test_is_success(res_1));


  (* Cas 2: Brique double *)
  let game_2 = {
    kind = BK_simple;
    array = [|[|BK_block; BK_bonus|]; [|BK_simple; BK_double|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [];
    ball = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = LIME};
    pad = {position = {dx = 0; dy = 0}; size = PS_SMALL; speed = ref 3; x = ref 0; y = ref 0};
    speed = ref 5
                } in
  let res_2 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(1, 1)", (game_2, 1, 1)) in
  assert_equals_result(BLUE, res_2);
  assert_true(test_is_success(res_2));


  (* Cas 3: Brique double différente *)
  let game_3 = {
    kind = BK_simple;
    array = [|[|BK_double; BK_simple|]; [|BK_block; BK_bonus|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [];
    ball = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = LIME};
    pad = {position = {dx = 0; dy = 0}; size = PS_SMALL; speed = ref 3; x = ref 0; y = ref 0};
    speed = ref 5
                } in
  let res_3 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(0, 1)", (game_3, 0, 1)) in
  assert_notequals_result(BLUE, res_3);
  assert_true(test_is_success(res_3));
 
  (* Cas 4: Brique simple différente *)
  let game_4 = {
    kind = BK_simple;
    array = [|[|BK_bonus; BK_block|]; [|BK_simple; BK_double|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [];
    ball = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = LIME};
    pad = {position = {dx = 0; dy = 0}; size = PS_SMALL; speed = ref 3; x = ref 0; y = ref 0};
    speed = ref 5
                } in
  let res_4 : t_camlbrick_color t_test_result = test_exec(brick_color, "brick_color(1, 0)", (game_4, 1, 0)) in
  assert_notequals_result(GREEN, res_4);
  assert_true(test_is_success(res_4))
;;



(*------------------------------------------------| Test paddle_x |------------------------------------------------*)

(**
  Cette fonction test la fonction paddle_x, elle créer un jeu avec une raquette positionnée à x = 30. La fonction vérifie ensuite si la valeur 
  retournée par paddle_x correspond à la position x de la raquette (30). Ensuite elle vérifie si la position x de la raquette est dans les limites 
  du jeu (0 ≤ x ≤ 800).

  @author AMID HASNA 
*)

let test_paddle_x() : unit = 

  let game_1 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = []; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in 
  let res_1 : int t_test_result = test_exec(paddle_x, "paddle_x (game_1)", game_1) in 
  assert_equals_result(30, res_1); (* Vérifi si la valeur retournée par paddle_x correspond à la position x de la raquette 30*)
  assert_true((paddle_x(game_1) >= 0) && (paddle_x(game_1) <= 800)); (* Vérifi si la position x de la raquette est dans les limites du jeu 0 ≤ x ≤ 800 *)
  assert_true(test_is_success(res_1))
;;

(*------------------------------------------------| Test paddle_size_pixel |------------------------------------------------*)
  
(**
  Cette fonction test la fonction paddle_size_pixel. Elle créer différents jeux avec des tailles de raquettes différentes puis éxecute paddle_size_pixel
  pour obtenir la taille de la raquette (50, 75, 100). Elle vérifie si la taille de la raquette obtenue correspond aux valeurs attendu pour chaque taille de 
  raquette. 

  @author Aya 
*)

let test_paddle_size_pixel() : unit =

  (* Cas 1: Créer un jeu avec une raquette de taille PS_SMALL *)
  let game_1 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = []; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_1 : int t_test_result = test_exec(paddle_size_pixel, "paddle_size_pixel (game_1)", game_1) in 
  assert_equals_result(50, res_1);
  assert_true(test_is_success(res_1));

  (* Cas 2: Créer un jeu avec une raquette de taille PS_MEDIUM *)
  let game_2 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = []; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_MEDIUM; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_2 : int t_test_result = test_exec(paddle_size_pixel,"paddle_size_pixel (game_2)", game_2) in 
  assert_equals_result(75, res_2); 
  assert_true(test_is_success(res_2));

  (* Cas 3: Créer un jeu avec une raquette de taille PS_BIG *)
  let game_3 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = []; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_BIG; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_3 : int t_test_result = test_exec(paddle_size_pixel, "paddle_size_pixel (game_3)", game_3) in 
  assert_equals_result(100, res_3); 
  assert_true(test_is_success(res_3))
;;

(*------------------------------------------------| Test has_ball |------------------------------------------------*)

(**
  Cette fonction test la fonction has_ball. Ce test créer trois jeux différents, elle éxecute la fonction has_ball sur chauque jeu pour vérifier si le 
  jeu contient une balle. Elle vérifie le résultat de chaque éxecution pour s'assurer que la focntion évalue correctement la présence ou l'absence de balles. 

  @author AMID Hasna
*)

let test_has_ball () : unit = 

  (*Cas 1 : Jeu sans balle*)
  let game_1 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = []; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_1 : bool t_test_result = test_exec(has_ball, "has_ball (game_1)", game_1) in
  assert_false_result (res_1);
  assert_true(test_is_success(res_1));

  (*Cas 2 : Jeu avec une balle*)
  let game_2 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = [{
      position = {dx = 0; dy = 0};
      frame = {dx = 0; dy = 0};
      size = BS_SMALL;
      color = GRAY
    }]; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_2 : bool t_test_result = test_exec(has_ball, "has_ball (game_2)", game_2) in
  assert_true_result (res_2);
  assert_true(test_is_success(res_2));

  (*Cas 3 : Jeu avec une balle ajoutée à une liste vide de balles*)
  let game_3 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = [] @ [{
      position = {dx = 0; dy = 0};
      frame = {dx = 0; dy = 0};
      size = BS_SMALL;
      color = GRAY
    }]; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_3 : bool t_test_result = test_exec(has_ball, "has_ball (game_3)", game_3) in
  assert_true_result (res_3);
  assert_true(test_is_success(res_3))
;;

(*------------------------------------------------| Test balls_count |------------------------------------------------*)

(**
  Cette fonction test la fonction balls_count. Elle creer deux jeux différents, un sans balle et un avec deux balles. Elle execute la fonction balls_count sur 
  chque jeux pour obtenir le nombre de balles dans le jeu. Elle vérifie si le nombre de balles retournée correspond au valeurs attendues pour chaque jeux. 
  game_1 = 0 et game_2 = 2.    

  @author Aya 
*)

let test_balls_count () : unit =

  (*Cas 1 : Jeu sans balle*)
  let game_1 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = []; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_1 : int t_test_result = test_exec(balls_count, "balls_count (game_1)", game_1) in
  assert_equals_result (0, res_1);
  assert_true(test_is_success(res_1));

  (*Création de deux balles*)
  let ball_1 = { position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL; color = GRAY} in
  let ball_2 = { position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL; color = GRAY} in

  (*Cas 2 : Jeu avec deux balles*)
  let game_2 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = [ball_1; ball_2];
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in 
  let res_2 : int t_test_result = test_exec(balls_count, "balls_count (game_2)", game_2) in
  assert_equals_result (2, res_2);
  assert_true(test_is_success(res_2));
;;


(*------------------------------------------------| Test paddle_move_left |------------------------------------------------*)

let test_paddle_move_left() : unit = 

  (*Cas 1 : jeu sans balle mouvement vers la gauche*)
  let game_1 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = []; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_1 : unit t_test_result = test_exec(paddle_move_left, "paddle_move_left (game_1)", game_1) in 
  assert_equals_result ((), res_1);
  assert_true(test_is_success(res_1));

  (*Cas 2 : jeu avec une raquette positionnée à x = 30, mouvement vers la gauche*)
  let game_2 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = []; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_2 : unit t_test_result = test_exec(paddle_move_left, "paddle_move_left (game_2)", game_2) in 
  assert_equals_result ((), res_2);
  assert_true(test_is_success(res_2))
;;

(*------------------------------------------------| Test paddle_move_right |------------------------------------------------*)

(**
  Cette fonction test la fonction paddle_move_right. Elle creer un jeu sans raquette et un jeu avec une raquette positionnée à x = 30. 
  Elle execute la fonction paddle_move_right sur chaque jeu pour effectuer le mouvement de la raquette vers la droite puis vérification
  si la raquette à bien été vers la droite. 
  
  @author AMID Hasna
*)

let test_paddle_move_right() : unit = 

  (*Cas 1 : jeu sans balle mouvement vers la droite*)
  let game_1 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = []; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_1 : unit t_test_result = test_exec(paddle_move_right, "paddle_move_right (game_1)", game_1) in 
  assert_equals_result ((), res_1);
  assert_true(test_is_success(res_1));

  (*Cas 2 : jeu avec une raquette positionnée à x = 30, mouvement vers la droite*)
  let game_2 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = []; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_2 : unit t_test_result = test_exec(paddle_move_right, "paddle_move_right (game_2)", game_2) in 
  assert_equals_result ((), res_2);
  assert_true(test_is_success(res_2))
;;


(*------------------------------------------------| Test balls_get |------------------------------------------------*)

(**
  [test_balls_get] est une fonction de test unitaire qui teste la fonction [balls_get].
  Il crée un jeu de test avec deux balles et appelle la fonction [balls_get] pour récupérer les balles.
  Les balles récupérées sont ensuite comparées au résultat attendu à l'aide de la fonction [assert_equals_result].
  Enfin, il affirme que le test est réussi en utilisant la fonction [assert_true].

  @author 
*)

let test_balls_get () : unit =

  let game_1 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = []; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in (*jeux sans brique et sans balle*)
  let res_1 : t_ball list t_test_result = test_exec(balls_get, "balls_get(game_1)", game_1) in 
  assert_equals_result([], res_1);
  assert_true(test_is_success(res_1)); 

  let game_2 =  {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = [ {frame = {dx = 10; dy = 20}; position = {dx = 1; dy = 1}; size = BS_SMALL ; color = BLUE}]; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in (*jeux avec une brique et une balle *)
  let res_2 : t_ball list t_test_result = test_exec(balls_get, "balls_get(game_2)", game_2) in 
  assert_equals_result([{frame = {dx = 10; dy = 20}; position = {dx = 1; dy = 1}; size = BS_SMALL ; color = BLUE}], res_2);
  assert_true(test_is_success(res_2));
  
  let game_3 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = [{frame = {dx = 10; dy = 20}; position = {dx = 1; dy = 1}; size = BS_SMALL; color = BLUE}; {frame = {dx = 30; dy = 40}; position = {dx = -1; dy = -1}; size = BS_SMALL; color = BLUE}]; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in (* Plusieurs briques et balles *)
  let res_3 : t_ball list t_test_result = test_exec(balls_get, "balls_get(game_3)", game_3) in 
  assert_equals_result([{frame = {dx = 10; dy = 20}; position = {dx = 1; dy = 1}; size = BS_SMALL; color = BLUE}; {frame = {dx = 30; dy = 40}; position = {dx = -1; dy = -1}; size = BS_SMALL; color = BLUE}], res_3);
  assert_true (test_is_success(res_3))
;;


(*------------------------------------------------| Test ball_get |------------------------------------------------*)

(**
  [test_ball_get] est une fonction de test unitaire qui teste la fonction [ball_get].

  @author 
*)
let test_ball_get() : unit = (** couverture de l'exception (bricks = []), 1er if *) 
    
  let game_1 = {
    kind = BK_simple; 
    array = make_bricks(20, 30); 
    gamestate = PLAYING; 
    color = RED; 
    param = make_camlbrick_param (); 
    balls = []; 
    ball = {
      position = {dx = 0; dy = 0}; 
      frame = {dx = 0; dy = 0}; 
      size = BS_SMALL ; color = BLUE}; 
      pad = {position = {dx = 0; dy = 0}; 
      size = PS_SMALL; speed = ref 3; x = ref 0; y = ref 0}; 
      speed = ref 5
      } 
    in 
    let res_1 : t_ball t_test_result = test_fail_exec(ball_get, "ball_get(game_1)", (game_1, 0)) in 
    assert_failwith(res_1); 
    assert_true(test_is_success(res_1)); 
    
    (** couverture de l'exception (index < 0), 1er if*) 
    let game_2 = {
      kind = BK_simple; 
      array = make_bricks(20, 30); 
      gamestate = PLAYING; 
      color = RED; 
      param = make_camlbrick_param (); 
      balls = []; 
      ball = {
        position = {dx = 0; dy = 0}; 
        frame = {dx = 0; dy = 0}; 
        size = BS_SMALL ; 
        color = BLUE}; 
        pad = {position = {dx = 0; dy = 0}; 
        size = PS_SMALL; 
        speed = ref 3; 
        x = ref 0; 
        y = ref 0}; 
        speed = ref 5
        } 
      in 
      let res_2 : t_ball t_test_result = test_fail_exec(ball_get, "ball_get(game_2)", (game_2, -1)) in 
      assert_failwith(res_2); 
      assert_true(test_is_success(res_2)); 
      
      (** couverture du 1er else/2eme then *) 
      let ball_1 = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = BLUE} in 
      let ball_2 = {position = {dx = 0; dy = 0}; frame = {dx = 0; dy = 0}; size = BS_SMALL ; color = BLUE} in 

      let game_3 = {
        kind = BK_simple; 
        array = make_bricks(20, 30); 
        gamestate = PLAYING; 
        color = RED; 
        param = make_camlbrick_param (); 
        balls = [ball_1; ball_2]; 
        ball = {
          position = {dx = 0; dy = 0}; 
          frame = {dx = 0; dy = 0}; 
          size = BS_SMALL ; 
          color = BLUE}; 
          pad = {
            position = {dx = 0; dy = 0}; 
            size = PS_SMALL; 
            speed = ref 3; 
            x = ref 0; 
            y = ref 0}; 
            speed = ref 5
            } 
          in let res_3 : t_ball t_test_result = test_exec(ball_get, "ball_get(game_3)", (game_3, 1)) in 
          assert_equals_result(ball_1, res_3); 
          assert_true(test_is_success(res_3)) 
  ;;

(*------------------------------------------------| Test ball_x |------------------------------------------------*)

(**
  [test_ball_x] est une fonction de test unitaire qui teste la fonction [ball_x].
  Il crée un jeu avec deux balles et vérifie la coordonnée X de la première balle.
  Le résultat attendu est 0.

  @author Dmytro HONCHARENKO
*)

let test_ball_x() : unit = 

  let game_1 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = []; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in 
  let res_1 : int t_test_result = test_exec(balls_count, "ball_x(game_1)", game_1) in
  assert_equals_result (0, res_1);
  assert_true(test_is_success(res_1)); 


  let game_2 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [{frame = {dx = 10; dy = 20}; position = {dx = 1; dy = 1}; size = BS_SMALL ; color = BLUE}]; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  }  in
  let res_2 : int t_test_result = test_exec(balls_count, "balls_x(game_2)", game_2) in 
  assert_equals_result(1, res_2);
  assert_true(test_is_success(res_2));


  let game_3 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [{frame = {dx = 10; dy = 20}; position = {dx = -1 ; dy = 1}; size = BS_SMALL ; color = BLUE}]; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_3 : int t_test_result = test_exec(balls_count, "balls_x(game_3)", game_3) in 
  assert_notequals_result(-1, res_3);
  assert_true(test_is_success(res_3))
;;

(*------------------------------------------------| Test ball_y |------------------------------------------------*)

(**
  @author Dmytro HONCHARENKO
*)

let test_ball_y() : unit = 

  let game_1 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = BLACK;
    param = make_camlbrick_param ();
    balls = []; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in 
  let res_1 : int t_test_result = test_exec(balls_count, "balls_y(game_1)", game_1) in
  assert_equals_result (0, res_1);
  assert_true(test_is_success(res_1));


  let game_2 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [{frame = {dx = 10; dy = 20}; position = {dx = 1; dy = 10}; size = BS_SMALL ; color = BLUE}]; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_2 : int t_test_result = test_exec(balls_count, "balls_y(game_2)", game_2) in 
  assert_equals_result(1, res_2);
  assert_true(test_is_success(res_2));


  let game_3 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [{frame = {dx = 10; dy = 20}; position = {dx = 1 ; dy = -10}; size = BS_SMALL ; color = BLUE}]; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_3 : int t_test_result = test_exec(balls_count, "balls_y(game_3)", game_3) in 
  assert_notequals_result(-10, res_3);
  assert_true(test_is_success(res_3))
;;

(*------------------------------------------------| Test ball_size_pixel |------------------------------------------------*)

(**
  [test_ball_size_pixel] est une fonction de test unitaire qui teste la fonction [ball_size_pixel].
  Il crée un jeu avec deux balles, chacune avec des positions, des cadres, des tailles et des couleurs différents.
  Il récupère ensuite la première balle du jeu et calcule sa taille en pixels à l'aide de la fonction [ball_size_pixel].
  La taille calculée est comparée à la taille attendue à l'aide de la fonction [assert_equals_result].
  Enfin, il affirme que le test est réussi en utilisant la fonction [assert_true].

  @author Dmytro HONCHARENKO
*)

let test_ball_size_pixel () : unit =

  let game_1 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [{frame = {dx = 10; dy = 20}; position = {dx = 1; dy = 10}; size = BS_SMALL ; color = BLUE}]; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in 
  let res_1 : int t_test_result = test_exec(balls_count, "ball_size_pixel(game_1)", game_1) in
  assert_notequals_result (10, res_1);
  assert_true(test_is_success(res_1));


  let game_2 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [{frame = {dx = 10; dy = 20}; position = {dx = 1; dy = 10}; size = BS_MEDIUM ; color = BLUE}]; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  }  in
  let res_2 : int t_test_result = test_exec(balls_count, "ball_size_pixel(game_2)", game_2) in 
  assert_notequals_result(20, res_2);
  assert_true(test_is_success(res_2));


  let game_3 = {
    kind = BK_empty;
    array = [|[|BK_empty|]|];
    gamestate = PLAYING;
    color = RED;
    param = make_camlbrick_param ();
    balls = [{frame = {dx = 10; dy = 20}; position = {dx = 1 ; dy = 10}; size = BS_BIG ; color = BLUE}]; 
    ball = {
            position = {dx = 0; dy = 0};
            frame = {dx = 0; dy = 0};
            size = BS_SMALL;
            color = GRAY
          };
    pad = {size = PS_SMALL; position = {dx = 30; dy = 0}; x = ref 30; y = ref 2; speed = ref 1};
    speed = ref 1
  } in
  let res_3 : int t_test_result = test_exec(balls_count, "ball_size_pixel(game_3)", game_3) in 
  assert_notequals_result(30, res_3);
  assert_true(test_is_success(res_3))
;;

(*------------------------------------------------| Test ball_color |------------------------------------------------*)

(**
  [test_ball_color] est une fonction de test unitaire qui teste la fonction [ball_color].
  Il crée un état de jeu avec deux boules, l'une de couleur BLANCHE et l'autre de couleur BLEUE.
  Il appelle ensuite la fonction [ball_color] avec l'état du jeu et la première balle.
  Le résultat attendu est BLANC.
  La fonction utilise les fonctions [assert_equals_result] et [assert_true] pour vérifier le résultat.

  @author Dmytro HONCHARENKO
*)


let test_ball_color() : unit = 

let ball_1 = {
  position = {dx = 0; dy = 0}; 
  frame = {dx = 0; dy = 0}; 
  size = BS_SMALL; 
  color = BLUE
  } in 
  
  let game_1 = {
    kind = BK_simple; 
    array = make_bricks(20, 30); 
    gamestate = PLAYING; 
    color = RED; 
    param = make_camlbrick_param (); 
    balls = []; 
    ball = {
      position = {dx = 0; dy = 0}; 
      frame = {dx = 0; dy = 0}; 
      size = BS_SMALL ; 
      color = BLUE}; 
      pad = {
        position = {dx = 0; dy = 0}; 
        size = PS_SMALL; 
        speed = ref 3; 
        x = ref 0; 
        y = ref 0
      }; 
      speed = ref 5
      } in 
      
      let res_1 : t_camlbrick_color t_test_result = test_exec(ball_color, "ball_color(game_1)", (game_1, ball_1)) in 
      assert_equals_result (LIME, res_1); 
      assert_true(test_is_success(res_1)); 
      
      let ball_2 = {
        position = {dx = 0; dy = 0}; 
        frame = {dx = 0; dy = 0}; 
        size = BS_MEDIUM ; color = BLUE
        } in 
        
        let game_2 = {
          kind = BK_simple; 
          array = make_bricks(20, 30); 
          gamestate = PLAYING; 
          color = RED; 
          param = make_camlbrick_param (); 
          balls = []; 
          ball = {
            position = {dx = 0; dy = 0}; 
            frame = {dx = 0; dy = 0}; 
            size = BS_MEDIUM ; 
            color = BLUE  }; 
            pad = {
              position = {dx = 0; dy = 0}; 
              size = PS_SMALL; 
              speed = ref 3; 
              x = ref 0; 
              y = ref 0}; 
              speed = ref 5
              } in 
              
              let res_2 : t_camlbrick_color t_test_result = test_exec(ball_color, "ball_color(game_2)", (game_2, ball_2)) in 
              assert_equals_result (LIGHTGRAY, res_2); 
              assert_true(test_is_success(res_2)); 
              
              let ball_3 = {
                position = {dx = 0; dy = 0}; 
                frame = {dx = 0; dy = 0}; 
                size = BS_BIG; 
                color = BLUE} in 
                
                let game_3 = {
                  kind = BK_simple; 
                  array = make_bricks(20, 30); 
                  gamestate = PLAYING; 
                  color = RED; 
                  param = make_camlbrick_param (); 
                  balls = []; 
                  ball = {
                    position = {dx = 0; dy = 0}; 
                    frame = {dx = 0; dy = 0}; 
                    size = BS_BIG ; color = BLUE}; 
                    pad = {
                      position = {dx = 0; dy = 0}; 
                      size = PS_SMALL; 
                      speed = ref 3; 
                      x = ref 0; 
                      y = ref 0
                      }; 
                      speed = ref 5
                      } in 
                      
                      let res_3 : t_camlbrick_color t_test_result = test_exec(ball_color, "ball_color(game_3)", (game_3, ball_3)) in 
                      assert_equals_result (GRAY, res_3); 
                      assert_true(test_is_success(res_3)) 
;;



(*------------------------------------------------| Test make_bricks |------------------------------------------------*)



(*------------------------------------------------| Test make_camlbrick |------------------------------------------------*)



(*------------------------------------------------| Test ball_modif_speed |------------------------------------------------*)



(*------------------------------------------------| Test ball_modif_speed_sign |------------------------------------------------*)



(*------------------------------------------------| Test is_inside_circle |------------------------------------------------*)



(*------------------------------------------------| Test is_inside_quad |------------------------------------------------*)



(*------------------------------------------------| Test ball_remove_out_of_border |------------------------------------------------*)



(*------------------------------------------------| Test ball_hit_paddle |------------------------------------------------*)



(*------------------------------------------------| Test ball_hit_corner_brick |------------------------------------------------*)



(*------------------------------------------------| Test ball_hit_side_brick |------------------------------------------------*)



(*------------------------------------------------| Test test_hit_balls |------------------------------------------------*)



(*------------------------------------------------| Test report |------------------------------------------------*)

test_reset_report();;

test_brick_get ();; 
test_brick_hit ();;
test_brick_color ();;
test_paddle_x();; 
test_paddle_size_pixel();; 
test_has_ball();; 
test_balls_count ();; 
test_paddle_move_left();; 
test_paddle_move_right();; 
test_balls_get();
test_ball_get();
test_ball_x();
test_ball_y();
test_ball_size_pixel();
test_ball_color();;
(*test_make_bricks();;
test_make_camlbrick();;
test_ball_modif_speed();;
test_ball_modif_speed_sign();;
test_is_inside_circle();;
test_is_inside_quad();;
test_ball_remove_out_of_border();;
test_ball_hit_paddle();;
test_ball_hit_corner_brick();;
test_ball_hit_side_brick();;
game_test_hit_balls();; *)

test_report();;