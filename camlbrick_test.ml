(** Ce module Camlbrick représente le noyau fonctionnel du jeu de casse-brique nommé <b>camlbrick</b>
(un jeu de mot entre le jeu casse-brique et le mot ocaml).

Le noyau fonctionnel consiste à réaliser l'ensemble des structures et autres fonctions capables
d'être utilisées par une interface graphique. Par conséquent, dans ce module il n'y a aucun
aspect visuel! Vous pouvez utiliser le mode console.

Le principe du jeu de casse-brique consiste à faire disparaître toutes les briques d'un niveau
en utilisant les rebonds d'une balle depuis une raquette contrôlée par l'utilisateur.

@author Edouard GONET
@author Dmytro HONCHARENKO
@author Aya GUEMMIE
@author Hasna AMID 

@version 1
*)

(** 
  Compteur utilisé en interne pour afficher le numéro de la frame du jeu vidéo. 
  Vous pouvez utiliser cette variable en lecture, mais nous ne devez pas modifier
  sa valeur! 
*)
let frames = ref 0;;

(**
  Type [t_camlbrick_color] : type énuméré représentant les couleurs gérables par notre moteur de jeu. Vous ne pouvez pas modifier ce type!
  @deprecated Ne pas modifier ce type! 
*)
type t_camlbrick_color = WHITE | BLACK | GRAY | LIGHTGRAY | DARKGRAY | BLUE | RED | GREEN | YELLOW | CYAN | MAGENTA | ORANGE | LIME | PURPLE;;
    
(**
  [t_camlbrick_param] : Cette structure regroupe tous les attributs globaux,
  pour paramétrer notre jeu vidéo.
  <b>Attention:</b> Il doit y avoir des cohérences entre les différents paramètres:
  <ul>
  <li> la hauteur totale de la fenêtre est égale à la somme des hauteurs de la zone de briques du monde et
  de la hauteur de la zone libre.</li>
  <li>la hauteur de la zone des briques du monde est un multiple de la hauteur d'une seule bmake_camlbrickique. </li>
  <li>la largeur du monde est un multiple de la largeur d'une seule brique. </li>
  <li>initialement la largeur de la raquette doit correspondre à la taille moyenne.</li>
  <li>la hauteur initiale de la raquette doit être raisonnable et ne pas toucher un bord de la fenêtre.</li>
  <li>La variable <u>time_speed</u> doit être strictement positive. Et représente l'écoulement du temps.</li>
  </ul>
*)
type t_camlbrick_param = {
  world_width : int; (** largeur de la zone de dessin des briques *)
  world_bricks_height : int; (** hauteur de la zone de dessin des briques *)
  world_empty_height : int; (** hauteur de la zone vide pour que la bille puisse évoluer un petit peu *)
    
  brick_width : int; (** largeur d'une brique *)
  brick_height : int; (** hauteur d'une brique *)
    
  paddle_init_width : int; (** largeur initiale de la raquette *)
  paddle_init_height : int; (** hauteur initiale de la raquette *)
    
  time_speed : int ref; (** indique l'écoulement du temps en millisecondes (c'est une durée approximative) *)
}
;;
    
(** 
  Enumeration des différents types de briques. 
  Vous ne devez pas modifier ce type.    
*)
type t_brick_kind = BK_empty | BK_simple | BK_double | BK_block | BK_bonus;;
    
(**
  [make_empty_brick] : Cette fonction renvoie le type de brique pour représenter les briques de vide.
  C'est à dire, l'information qui encode l'absence de brique à un emplacement sur la grille du monde.

  @return Renvoie le type correspondant à la notion de vide.
  @deprecated  Cette fonction est utilisé en interne.    
*)
let make_empty_brick() : t_brick_kind = BK_empty;;
    
(** 
  Type [t_ball_size] Enumeration des différentes tailles des billes. La taille  normale d'une bille est [BS_MEDIUM]. 
      
  Vous pouvez ajouter d'autres valeurs sans modifier les valeurs existantes.
*)
type t_ball_size = BS_SMALL | BS_MEDIUM | BS_BIG;;
      
(** 
  Type [t_paddle_size] : Enumeration des différentes taille de la raquette. Par défaut, une raquette doit avoir la taille [PS_SMALL]. 
    
  Vous pouvez ajouter d'autres valeurs sans modifier les valeurs existantes.
*)
type t_paddle_size = PS_SMALL | PS_MEDIUM | PS_BIG;;
    
(** 
  Type [t_gamestate] : Enumération des différents états du jeu. Nous avons les trois états de base:
  <ul>
  <li>[GAMEOVER]: qui indique si une partie est finie typiquement lors du lancement du jeu</li>
  <li>[PLAYING]: qui indique qu'une partie est en cours d'exécution</li>
  <li>[PAUSING]: indique qu'une partie en cours d'exécution est actuellement en pause</li>
  </ul>
          
  Dans le cadre des extensions, vous pouvez modifier ce type pour adopter d'autres états du jeu selon
  votre besoin.
*)
type t_gamestate = GAMEOVER | PLAYING | PAUSING;;
    
(**
  Enumération des positions x et y d'un vecteur

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID 
*)
type t_vec2 = {
  dx : int; 
  dy : int
}
;; 
  
(**
  [make_vec2] : Cette fonction permet de créer un vecteur 2D à partir de deux entiers.
  Les entiers représentent la composante en X et en Y du vecteur.
    
  Vous devez modifier cette fonction.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID 
  @param x première composante du vecteur
  @param y seconde composante du vecteur
  @return Renvoie le vecteur dont les composantes sont (x,y).
*)
let make_vec2(x, y : int * int) : t_vec2 = {
  dx = x; 
  dy = y
}
;;
    
(**
  [vec2_add] : Cette fonction renvoie un vecteur qui est la somme des deux vecteurs donnés en arguments.
  
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID 
  @param a premier vecteur
  @param b second vecteur
  @return Renvoie un vecteur égale à la somme des vecteurs.
*)
let vec2_add(a, b : t_vec2 * t_vec2) : t_vec2 = {
  dx = a.dx + b.dx; 
  dy = a.dy + b.dy
}
;;
     
(**
  [vec2_add_scalar] : Cette fonction calcul la somme des composantes du vecteur a et du vecteur construit à partir de (x,y).
      
  Cette fonction est une optimisation du code suivant (que vous ne devez pas faire en l'état):
  {[
    let vec2_add_scalar(a,x,y : t_vec2 * int * int) : t_vec2 =
      vec2_add(a, make_vec2(x,y))
    ;;
  ]}
    
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID 
  @param a premier vecteur
  @param x composante en x du second vecteur
  @param y composante en y du second vecteur
  @return Renvoie un vecteur qui est la résultante du vecteur 
*)
let vec2_add_scalar(a, x, y : t_vec2 * int * int) : t_vec2 = {
  dx = a.dx + x; 
  dy = a.dy + y
}
;;
    
(**
  [vec2_mult] : Cette fonction renvoie un vecteur qui est le produit des deux vecteurs donnés en arguments.
  
  Ainsi,
  {[
    c_x = a_x * b_x
    c_y = a_y * b_y
  ]}

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID 
  @param a premier vecteur
  @param b second vecteur
  @return Renvoie un vecteur qui résulte de la multiplication des composantes. 
*)
let vec2_mult(a, b : t_vec2 * t_vec2) : t_vec2 = {
  dx = a.dx * b.dx; 
  dy = a.dy * b.dy
}
;;
   
(**
  [vec2_mult_scalar] Cette fonction calcul la multiplication des composantes du vecteur a et du vecteur construit à partir de (x,y). Cette fonction est une optimisation du code suivant (que vous ne devez pas faire en l'état):
  {[
    let vec2_mult_scalar(a,x,y : t_vec2 * int * int) : t_vec2 =
      vec2_mult(a, make_vec2(x,y))
    ;;
  ]}

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID 
  @param a vecteur de type t_vec2
  @param x entier qui va multiplier le scalaire du vecteur a 
  @param y entier qui va multiplier le scalaire du vecteur a 
  @return retourne un nouveau vecteur avec la multiplications des correspondantes du vecteurs a 
*)
let vec2_mult_scalar(a, x, y : t_vec2 * int * int) : t_vec2 = {
  dx = a.dx * x; 
  dy = a.dy * y
}
;;
  
(**
  Type [t_ball] : représente la balle du jeu.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID 
*)
type t_ball = {
  position : t_vec2 ref; (**Position de la balle*)
  frame : t_vec2; (**Cadre de la balle*)
  size : t_ball_size; (**Taille de la balle*)
  color : t_camlbrick_color; (**Couleur de la balle*)
  velocity : t_vec2 ref; (**Vitesse de la balle*)
};; 

(**
  Type [t_paddle] : représente la raquette du jeu.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID 
*)
type t_paddle = {
  position : t_vec2 ref;
  size : t_paddle_size; (**Définit la taille de notre raquette*)
  speed : int ref; (**définit sa vitesse de déplacement*)
  x : int ref; (**Position au niveau de l'absisse*)
};;

(** 
  Type [t_camlbrick] : représente l'état du jeu.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID 
*)
type t_camlbrick = {
  kind : t_brick_kind; (**Type de la brique*)
  array : t_brick_kind array array; (**Tableau contenant le type de la brique*)
  gamestate : t_gamestate; (**État du jeux*)
  color : t_camlbrick_color; (**Couleur général de notre jeu*)
  param : t_camlbrick_param; (**regroupe tous les attributs globeaux du jeu*)
  balls : t_ball list; (**Liste contenant le type de nos balle*)
  ball : t_ball; (**Type de balle*)
  pad : t_paddle; (**Type de raquette*)
  speed : int ref (**Vitesse de notre jeu*)
};;

(**
  [make_camlbrick_param] : est une fonction qui renvoie un paramétrage de jeu par défaut. La fonction crée un paramétrage de jeu avec les valeurs suivantes :

  - [world_width] : 800
  - [world_bricks_height] : 600
  - [world_empty_height] : 200
  - [brick_width] : 40
  - [brick_height] : 20
  - [paddle_init_width] : 100
  - [paddle_init_height] : 20
  - [time_speed] : 20

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID 
  @return Renvoie un paramétrage de jeu par défaut      
*)
let make_camlbrick_param() : t_camlbrick_param = {
  world_width = 800;
  world_bricks_height = 600;
  world_empty_height = 200;
    
  brick_width = 40;
  brick_height = 20;
    
  paddle_init_width = 100;
  paddle_init_height = 20;
    
  time_speed = ref 20;
};;
    
    
(**
  [param_get] : est une fonction qui renvoie le paramétrage actuel du jeu. La fonction prend en paramètre une structure de type [t_camlbrick] et renvoie le paramétrage actuel du jeu.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game jeu en cours d'exécution.
  @return Renvoie le paramétrage actuel.
*)
let param_get(game : t_camlbrick) : t_camlbrick_param = game.param;;

(**
  [generate_bricks] : est une fonction qui génère un type de brique aléatoire. La fonction utilise la fonction [Random.int] pour générer un nombre aléatoire entre 0 et 99. En fonction de la valeur générée, la fonction renvoie un type de brique aléatoire parmi les types suivants :

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @return un type de brique choisi aléatoirement
*)
let generate_bricks() : t_brick_kind = 
  Random.self_init(); (** Initialisation du générateur de nombres aléatoires *)
  let rand : int = Random.int 100 in (** Génération d'un nombre aléatoire entre 0 et 99 *)
  (
    if rand < 50
    then BK_simple
    else
      if rand < 70
      then BK_double
      else
        if rand < 80
        then BK_empty
        else
          if rand < 95
          then BK_bonus
          else BK_block  
  )
;;

(**
  [make_bricks] : est une fonction qui crée un tableau 2D de types de briques avec des dimensions de [a] lignes et [b] colonnes. Chaque élément du tableau est initialisé avec la valeur [BK_simple]. La fonction génère un type de brique aléatoire à l'aide de la fonction [generate_bricks] et l'affecte à chaque élément du tableau.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param a le nombre de lignes dans le tableau 2D.
  @param b le nombre de colonnes dans le tableau 2D.
  @return renvoie un tableau de brique
*)
let make_bricks(a, b : int * int) : t_brick_kind array array = 
  let tab : t_brick_kind array array = Array.make_matrix a b BK_simple in 
  (
    for i = 0 to a - 1
    do 
      for j = 0 to b - 1
      do 
        let rkind : t_brick_kind = generate_bricks() in 
        (tab.(i).(j) <- rkind)
      done;
    done;
    tab;
  )
;; 

(**
  [make_ball] : est une fonction qui crée une balle avec une position [(0, 0)], une taille [BS_SMALL] et une vitesse [(0, 0)]. La fonction renvoie une balle correctement initialisée.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param x entiers représentant les coordonnées de la position initiale de la balle
  @param y entiers représentant les coordonnées de la position initiale de la balle
  @param size indique la taille de la balle 
  @return retourne une nouvelle instance de la structure t_ball, qui représente une balle dans le jeu
*)
let make_ball(x, y, size : int * int * t_ball_size) : t_ball = {
  position = ref (make_vec2 (0, 0));
  frame = make_vec2 (0, 0);
  size = size;
  color = GRAY;
  velocity = ref (make_vec2 (0, 0))
};;

(**
  [make_paddle] : est une fonction qui crée une raquette avec une position [(0, 0)], une taille [PS_SMALL] et une vitesse [1]. La fonction renvoie une raquette correctement initialisée.
  
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @deprecated Cette fonction est là juste pour le debug ou pour débuter certains traitements de test.
*)
let make_paddle () : t_paddle = {
  position = ref (make_vec2(0, 0));
  size = PS_SMALL;
  speed = ref 1;
  x = ref 1;
};;     

(**
  [make_camlbrick] : est une fonction qui renvoie un jeu correctement initialisé. La fonction crée un jeu de type t_camlbrick avec une balle, une raquette et des briques. La balle est initialisée avec une position [(395, 750)] et une taille [BS_SMALL]. La raquette est initialisée avec une taille [PS_SMALL]. Les briques sont initialisées avec une largeur de [20] et une hauteur de [30]. La fonction renvoie un jeu correctement initialisé.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @return Renvoie un jeu correctement initialisé
*)
let make_camlbrick() : t_camlbrick = 
  let ball : t_ball list = make_ball(395, 750, BS_SMALL) :: []
  and paddle : t_paddle = make_paddle()
  in
  {
    kind = BK_simple; 
    color = WHITE; 
    array = make_bricks(20, 30);
    gamestate = PLAYING;
    param = make_camlbrick_param(); 
    balls = ball;
    ball = List.hd(ball);
    pad = paddle;
    speed = ref 5
  }
;;

(**
  [string_of_gamestate] : est une fonction qui renvoie une chaîne de caractères représentant l'état du jeu. La fonction prend en paramètre une structure de type t_camlbrick et renvoie une chaîne de caractères représentant l'état du jeu.
      
  Vous devez modifier cette fonction.
    
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représente le jeu en cours d'exécution.
  @return Renvoie la chaîne de caractère représentant l'état du jeu.
*)
let string_of_gamestate(game : t_camlbrick) : string = (* Itération 1,2,3 et 4 *)
  let param = param_get game in  (* Récupération des paramètres du jeu *)
  let world_width = param.world_width in  (* Largeur du monde *)
  let world_bricks_height = param.world_bricks_height in  (* Hauteur de la zone de dessin des briques *)
  let world_empty_height = param.world_empty_height in  (* Hauteur de la zone vide pour que la balle puisse évoluer *)
  let brick_width = param.brick_width in  (* Largeur d'une brique *)
  let brick_height = param.brick_height in  (* Hauteur d'une brique *)
  let paddle_init_width = param.paddle_init_width in  (* Largeur initiale de la raquette *)
  let paddle_init_height = param.paddle_init_height in  (* Hauteur initiale de la raquette *)
  let time_speed = !(param.time_speed) in  (* Durée approximative d'une frame *)

  (* Construction de la chaîne de caractères représentant l'état du jeu *)
  "World width: " ^ string_of_int world_width ^ "\n" ^ (*^ "\n" ^ est utilisé pour ajouter un saut de ligne entre chaque ligne de la chaîne de caractères*)
  "World bricks height: " ^ string_of_int world_bricks_height ^ "\n" ^
  "World empty height: " ^ string_of_int world_empty_height ^ "\n" ^
  "Brick width: " ^ string_of_int brick_width ^ "\n" ^
  "Brick height: " ^ string_of_int brick_height ^ "\n" ^
  "Paddle init width: " ^ string_of_int paddle_init_width ^ "\n" ^
  "Paddle init height: " ^ string_of_int paddle_init_height ^ "\n" ^
  "Time speed: " ^ string_of_int time_speed
;;
    
(**
  [brick_get] : est une fonction qui renvoie le type de brique à partir des coordonnées dans la zone de briques. La fonction prend en paramètre une matrice de type t_brick_kind array array, un entier i et un entier j.

  - [game] est l'état du jeu représenté par une structure t_camlbrick.
  - [i] est l'index de la ligne de la brique.
  - [j] est l'indice de colonne de la brique.

  La fonction vérifie d'abord si les indices (i, j) sont compris dans les limites du tableau de briques. Si c'est le cas, elle renvoie BK_empty, ce qui indique que la brique est vide. Sinon, elle accède à la brique à la position (i, j) dans le tableau de jeu et renvoie son type.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID 
  @param game représente une matrice de type t_brick_kind array array
  @param i coordonnée d'une brique dans le tableau 
  @param j coordonnée d'une brique dans le tableau
  @return renvoie le type de brique à partir des coordonnées dans la zone de briques
*)
let brick_get(game, i , j : t_camlbrick * int * int) : t_brick_kind =
  (* vérifie que i, j sont à l'intèrieur de bricks *)
  if i < 0 || j < 0 || i >= Array.length(game.array) || j >= Array.length(game.array.(0)) 
  then BK_empty 
  else game.array.(i).(j) (* accede a bricks a x, y et renvoie son type *)
;;
 
(**
  [brick_hit] : Cette fonction réalise des modifications dans la zone de brique pour faire évoluer une brique comme si elle était touchée par une balle.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représente une matrice de type t_brick_kind array array 
  @param i coordonée qui représente l'indice de ligne de la matrice game
  @param j coordonée qui représente l'indice de ligne de la matrice game
  @return retourne le type d'une brique après avoir été touchée par une balle
*)
let brick_hit(game, i, j : t_camlbrick * int * int) : t_brick_kind =
  if (i >= 0 && j >= 0 && i < Array.length(game.array) && j < Array.length(game.array.(0))) 
  then
    if game.array.(i).(j) = BK_simple 
    then (game.array.(i).(j) <- BK_empty; BK_empty)
    else 
      if game.array.(i).(j) = BK_double 
      then (game.array.(i).(j) <- BK_simple; BK_simple)
      else 
        if game.array.(i).(j) = BK_bonus 
        then (game.array.(i).(j) <- BK_empty; BK_empty)
        else 
          if game.array.(i).(j) = BK_block 
          then (game.array.(i).(j) <- BK_block; BK_block )
          else BK_empty
  else BK_empty
;;
       
(** 
  [brick_color] : est une fonction qui renvoie la couleur d'une brique dans le tableau de jeu à la position (i, j).

  La fonction vérifie la valeur de la brique dans le tableau de jeu à la position (i, j) et l'associe à une couleur correspondante :
  - Si la brique est BK_simple, elle renvoie JAUNE.
  - Si la brique est BK_double, elle renvoie la couleur BLEUE.
  - Si la brique est BK_bonus, elle renvoie la couleur VERTE.
  - Si la brique est BK_block, elle renvoie BLANC.
  - Si la brique a une autre valeur, elle renvoie NOIR.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représente une matrice de type t_brick_kind array array 
  @param i coordonée qui représente l'indice de ligne de la matrice game
  @param j coordonée qui représente l'indice de ligne de la matrice game
  @return retourne la couleur d'une brique associé son type
*)
let brick_color (game, i, j : t_camlbrick * int * int) : t_camlbrick_color =
  if game.array.(i).(j) = BK_simple 
  then YELLOW
  else if game.array.(i).(j) = BK_double 
  then BLUE
  else if game.array.(i).(j) = BK_bonus 
  then GREEN 
  else if game.array.(i).(j) = BK_block
  then WHITE
  else BLACK
;;

(** 
  [paddle_x] : Cette fonction renvoie la position [x] de la palette. Elle renvoie la valeur de la position [x] de la palette en fonction de la position [x] de la raquette.
  
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représentant l'état actuel du jeu
  @return Coordonnée x de la palette. 
*)
let paddle_x (game: t_camlbrick) : int = !(game.pad.x);;

(**
  [paddle_size_pixel] : Cette fonction renvoie la largeur en pixel du rectangle. 

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représentant l'état actuel du jeu
  @return retourne un entier qui représente la largeur en pixels de la raquette en fonction de sa taille 
*)
let paddle_size_pixel(game : t_camlbrick) : int =
  if game.pad.size = PS_SMALL 
  then 50 (*largeur en pixels pour une raquette small *)
  else 
    if game.pad.size = PS_MEDIUM 
    then 75 (*largeur en pixels pour une raquette medium *)
    else 100 (*largeur en pixels pour une raquette big *)
;;
    
(**
  [paddle_move_left] : La fonction déplace la raquette vers la gauche en fonction de la vitesse de la raquette. Elle vérifie si la raquette est déjà à la limite de la fenêtre. Si la raquette est à la limite de la fenêtre, elle ne la déplace pas. Sinon, elle déplace la raquette vers la gauche en fonction de la vitesse de la raquette.
  
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représentant l'état actuel du jeu
  @return ne retourne pas de valeur, elle modifie l'état du jeu
*)
let paddle_move_left(game : t_camlbrick) : unit = 
  if paddle_x(game) <= 0
  then ()
  else
      if paddle_x(game) > 0
      then game.pad.x := !(game.pad.x) - 20
      else ()
;;

(**
  [paddle_move_right] : Cette fonction va nous permettre de deplacer la raquette vers la droite sur l'axe des abscisses.
  
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représentant l'état actuel du jeu
  @return ne retourne pas de valeur, elle modifie l'état du jeu
*)
let paddle_move_right(game : t_camlbrick) : unit = 
  if paddle_x(game) >= 0
  then ()
  else
      if paddle_x(game) < 0
      then game.pad.x := !(game.pad.x) + 20
      else ()
;;

(**
  [has_ball] : La fonction vérifie si une balle est présente dans le jeu. Elle parcourt la liste de balles et vérifie si la liste est vide. La fonction renvoie vrai si une balle est présente dans le jeu, sinon elle renvoie faux.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représentant l'état actuel du jeu
  @param check parcourt les briques du jeu et vérifie si l'une d'entre elles contient des balles
  @return retourne un booléen indiquant si le jeu contient au moins une balle

*)
let rec has_ball(game : t_camlbrick) : bool =
  let rec check_ball(check : t_camlbrick list) =
    if check = [] 
    then false
    else
      let current_check = List.hd(check) in
      if current_check.balls <> [] 
      then true
      else check_ball(List.tl(check))
  in
  check_ball([game])
;; 

(**
  [balls_count] : La fonction compte le nombre total de balles en jeu en parcourant la liste de briques. Elle récupère le nombre de balles de chaque brique et les additionne pour obtenir le nombre total de balles en jeu. La fonction renvoie le nombre total de balles en jeu.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représentant l'état actuel du jeu
  @return retourne le nombre total de balle en jeux
*)
let balls_count(game : t_camlbrick) : int =
  let rec count_balls(bricks : t_camlbrick list) = 
    if bricks = []
    then 0
    else 
      let brick_ball_count = List.length(List.hd bricks).balls in
      brick_ball_count + count_balls(List.tl(bricks))in
  count_balls([game])
;; 

(**
  [balls_get] : La fonction récupère toutes les balles présentes dans le jeu en parcourant la liste de briques. Elle récupère les balles de chaque brique et les ajoute à une liste de balles. La fonction renvoie la liste de balles obtenue après avoir parcouru toutes les briques du jeu.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représentant l'état actuel du jeu
  @return retourne une liste de toutes les balles présentes dans le jeu
*)
let balls_get(game : t_camlbrick) : t_ball list =
  let rec collect_balls (bricks : t_camlbrick list) =
    if bricks = [] 
    then []
    else
      let first_brick = List.hd(bricks) in
        first_brick.balls @ collect_balls (List.tl(bricks))
  in
  collect_balls([game])
;; 

(**
  [ball_get] : La fonction récupère une balle spécifique du jeu en fonction de l'indice fourni. Elle parcourt la liste de briques et récupère la balle à l'indice spécifié. La fonction renvoie la balle spécifique du jeu en fonction de l'indice fourni.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représentant l'état actuel du jeu
  @param i qui est l'indice de la balle que l'on souhaite récupérer.
  @return retourne la balle spécifique du jeu en fonction de l'indice fourni
*)
let ball_get (game, i : t_camlbrick * int) : t_ball =
  let rec get_ball(bricks : t_camlbrick list) (index : int) =
    if index < 0 || bricks = [] 
    then failwith "Invalid index or empty game"
    else
      let first_brick = List.hd(bricks) in
      let brick_ball_count = List.length(first_brick.balls) in
      if index < brick_ball_count 
      then List.nth(first_brick.balls) index
      else get_ball (List.tl(bricks)) (index - brick_ball_count)
  in
  get_ball [game] i
;; 

(**
  [ball_x] : La fonction utilise la fonction de référence [ball.position] pour accéder à la position de la balle et renvoyer sa coordonnée horizontale [dx].

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représentant l'état actuel du jeu
  @param ball représentant la balle dont nous voulons obtenir la position
  @return retourne la valeur de position horizontale de la balle
*)
let ball_x(game, ball : t_camlbrick * t_ball) : int = 
   !(game.ball.position).dx
;;

(**
  [ball_y] Cette fonction extrait et retourne la position verticale de la balle spécifiée dans le jeu. 

  La fonction extrait la position verticale de la balle spécifiée dans le jeu en accédant à la valeur de la coordonnée y de la position de la balle. La fonction utilise la fonction de référence [ball.position] pour accéder à la position de la balle et en extraire la coordonnée y.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représentant l'état actuel du jeu
  @param ball représentant la balle dont nous voulons obtenir la position
  @return retourne la valeur de position verticale de la balle
*)    
let ball_y(game, ball : t_camlbrick * t_ball) : int = 
   !(game.ball.position).dy
;;

(**
  [ball_size_pixel] Cette fonction retourne la taille en pixels de la balle spécifiée dans le jeu, en fonction de sa taille définie.

  La fonction vérifie la taille de la balle spécifiée dans le jeu et renvoie la taille en pixels correspondante :
  - Si la balle est de taille BS_SMALL, elle renvoie 10.
  - Si la balle est de taille BS_MEDIUM, elle renvoie 20.
  - Si la balle est de taille BS_BIG, elle renvoie 30.
  
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représentant l'état actuel du jeu
  @param ball représentant la balle dont nous voulons obtenir la taille en pixels
  @return retourne un entier représentant la taille en pixels de la balle 
*)
let ball_size_pixel(game, ball : t_camlbrick * t_ball) : int =
  if game.ball.size = BS_SMALL then 10
  else if game.ball.size = BS_MEDIUM then 20
  else 30
;;

(**
  [ball_color] Cette fonction extrait et retourne la couleur de la balle.

  La fonction vérifie la taille de la balle et renvoie la couleur correspondante :
  - Si la balle est de taille BS_SMALL, elle renvoie LIME.
  - Si la balle est de taille BS_MEDIUM, elle renvoie LIGHTGRAY.
  - Si la balle est de taille BS_BIG, elle renvoie GRAY.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game représentant l'état actuel du jeu
  @param ball représentant la balle dont nous voulons obtenir la couleur
  @return retourne la couleur de la balle en jeu
*)
let ball_color(game, ball : t_camlbrick * t_ball) : t_camlbrick_color =  
  if ball.size = BS_SMALL then LIME 
  else if ball.size = BS_MEDIUM then LIGHTGRAY 
  else GRAY 
;;

(** 
  [ball_modif_speed] modifie la vitesse de la balle dans le jeu en ajoutant le vecteur [dv] à sa vitesse.

  La fonction ajoute le vecteur [dv] à la vitesse de la balle en modifiant la valeur de la vitesse de la balle. La fonction utilise la fonction [vec2_add] pour ajouter le vecteur [dv] à la vitesse de la balle. La fonction modifie la valeur de la vitesse de la balle en utilisant la fonction de référence [ball.velocity] pour accéder à la vitesse de la balle et la modifier.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game est l'état actuel du jeu
  @param ball la balle est la balle à modifier
  @param dv est le vecteur à ajouter à la vitesse de la balle
 *)
let ball_modif_speed (game, ball, dv : t_camlbrick * t_ball * t_vec2) : unit =
  ball.velocity := vec2_add (!(ball.velocity), dv)
;;

(** 
  [ball_modif_speed_sign] modifie la vitesse de la balle dans le jeu en multipliant sa vitesse par le scalaire [sv].

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game est l'état actuel du jeu
  @param ball la balle est la balle à modifier
  @param sv est le scalaire par lequel il faut multiplier la vitesse de la balle
*)
let ball_modif_speed_sign (game, ball, sv : t_camlbrick * t_ball * t_vec2) : unit =
  ball.velocity := vec2_mult (!(ball.velocity), sv)
;;

(**
  [is_inside_circle] Cette fonction détermine si un point spécifié est situé à l'intérieur d'un cercle donné en comparant la distance entre le point et le centre du cercle à la valeur du rayon. Si la distance est inférieure au rayon, le point est à l'intérieur du cercle, sinon il est à l'extérieur.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param cx coordonnées horizontale du centre du cercle
  @param cy coordonnées verticale du centre du cercle
  @param rad est le rayon du cercle 
  @param x coordonnées du point à vérifier
  @param y coordonnées du point à vérifier
  @return retourne un booléen qui indique si le point spécifié est à l'intérieur du cercle défini par son centre et son rayon
*)
let is_inside_circle(cx, cy, rad, x, y : int * int * int * int * int) : bool =
  let first_point : float = Float.pow (float_of_int (x - cx)) 2. in
  let second_point : float = float_of_int (cy - y) in
  Float.sqrt (first_point +. second_point) < (float_of_int rad)
;;
    
(**
  [is_inside_quad] Cette fonction vérifie si un point spécifié est situé à l'intérieur d'un quadrilatère défini par ses coins, en comparant les coordonnées du point avec les coordonnées des coins du quadrilatère.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param x1 coordonnées des coins du quadrilatère
  @param y1 coordonnées des coins du quadrilatère
  @param x2 coordonnées des coins du quadrilatère
  @param y2 coordonnées des coins du quadrilatère
  @param x coordonnées du point à vérifier
  @param y coordonnées du point à vérifier
  @return fonction retourne un booléen qui indique si le point spécifié est à l'intérieur du quadrilatère défini par ses coins.
*)
let is_inside_quad(x1, y1, x2, y2, x, y : int * int * int * int * int * int) : bool =
  x >= x1 && x <= x2 && y >= y1 && y <= y2
;;

(**
  La fonction [ball_remove_out_of_border] permet de supprimer de la liste [balls] les balles qui se trouvent en dehors des limites du jeu [game].
  
  La fonction parcourt la liste des boules et vérifie si les coordonnées de chaque boule se trouvent à l'intérieur des limites du jeu. Si une balle se trouve à l'intérieur des limites, elle est conservée dans la liste résultante. Dans le cas contraire, elle est supprimée de la liste.
  
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game est l'état actuel du jeu
  @param balls est la liste des balles à vérifier
*)
let ball_remove_out_of_border (game, balls : t_camlbrick * t_ball list) : t_ball list =
  let rec remove_out (game, balls) : t_ball list =
    if balls = [] 
    then []
    else
      let ball = List.hd(balls) in
      let x = ball_x(game, ball) in
      let y = ball_y(game, ball) in
      if x >= 0 && y >= 0 && x < Array.length(game.array) && y < Array.length(game.array).(0)
      then  ball :: balls
      else remove_out(game, List.tl(balls))
  in
  remove_out(game, balls)
;;

(**
  [ball_hit_paddle] est une fonction qui vérifie si la balle a touché la raquette dans le jeu.

  La fonction commence par calculer les coordonnées du centre de la raquette à partir de ses coordonnées de coin. Ensuite, elle calcule les coordonnées des points du milieu de chaque côté de la raquette. Enfin, elle vérifie si la balle se trouve à l'intérieur d'un cercle de rayon égal à la taille de la balle centré sur l'un des points du milieu des côtés de la raquette. Si c'est le cas, la fonction renvoie true, sinon elle renvoie false.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game est l'état actuel du jeu
  @param ball est la balle à vérifier
  @param paddle est la raquette à vérifier
*)
let ball_hit_paddle(game, ball, paddle : t_camlbrick * t_ball * t_paddle) : bool =
  let paddle_size : int = paddle_size_pixel(game) in
  let paddle_x : int = !(paddle.position).dx in
  let paddle_right : int = paddle_x + paddle_size in
  let paddle_y : int = game.param.world_bricks_height + game.param.world_empty_height - game.param.paddle_init_height in
  let paddle_top : int = paddle_y in
  let paddle_bottom : int = paddle_y + game.param.paddle_init_height in
  let ball_x : int = !(ball.position).dx in
  let ball_y : int = !(ball.position).dy in
  let ball_radius : int = ball_size_pixel(game, ball) in

  if ball_y - ball_radius <= paddle_bottom && ball_y + ball_radius >= paddle_top then
    if ball_x + ball_radius >= paddle_x && ball_x - ball_radius <= paddle_right then
      true
    else
      false
  else
    false
;;

(**
  [ball_hit_corner_brick] est une fonction qui vérifie si la balle a touché le coin d'une brique dans le jeu.

  La fonction commence par calculer les coordonnées du coin de la brique à partir de ses coordonnées de coin. Ensuite, elle vérifie si la balle se trouve à l'intérieur d'un cercle de rayon égal à la taille de la balle centré sur le coin de la brique. Si c'est le cas, la fonction renvoie true, sinon elle renvoie false.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game est l'état actuel du jeu
  @param ball est la balle à vérifier
  @param i est l'indice de ligne de la brique
  @param j est l'indice de colonne de la brique
*)
let ball_hit_corner_brick(game, ball, i, j : t_camlbrick * t_ball * int * int) : bool = 
  let brick_x = i * game.param.brick_width in 
  let brick_y = j * game.param.brick_height in 
  let corner_1 : int = brick_x in 
  let corner_2 : int = brick_y in 
  let corner_3 : int = brick_x + game.param.brick_width in 
  let corner_4 : int = brick_y + game.param.brick_height in 
  if is_inside_quad(corner_1, corner_2, corner_3, corner_4, ball_x(game, ball), ball_y(game, ball)) then true 
  else false
;;

(**
  [ball_hit_side_brick] Cette fonction vérifie si la balle a touché le côté d'une brique dans le jeu.

  La fonction commence par calculer les coordonnées du centre de la brique à partir de ses coordonnées de coin. Ensuite, elle calcule les coordonnées des points du milieu de chaque côté de la brique. Enfin, elle vérifie si la balle se trouve à l'intérieur d'un cercle de rayon égal à la taille de la balle centré sur l'un des points du milieu des côtés de la brique. Si c'est le cas, la fonction renvoie true, sinon elle renvoie false.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game est l'état actuel du jeu
  @param ball est la balle à vérifier
  @param i est l'indice de ligne de la brique
*)
let ball_hit_side_brick(game, ball, i, j: t_camlbrick * t_ball * int * int) : bool =
  let brick_x : int = j * game.param.brick_width in
  let brick_y : int = i * game.param.brick_height in
  let brick_center_x : int  = brick_x + game.param.brick_width / 2 in
  let brick_center_y : int = brick_y + game.param.brick_height / 2 in
  let side_points : (int * int) array= [|
    (brick_x, brick_center_y);  (* Milieu du côté gauche *)
    (brick_x + game.param.brick_width, brick_center_y);  (* Milieu du côté droit *)
    (brick_center_x, brick_y);  (* Milieu du côté supérieur *)
    (brick_center_x, brick_y + game.param.brick_height)  (* Milieu du côté inférieur *)
  |] in
  let ball_x : int = !(ball.position).dx in
  let ball_y : int = !(ball.position).dy in
  let ball_radius : int= ball_size_pixel(game, ball) in
  let collision : bool ref= ref false in
  for i = 0 to Array.length side_points - 1 do
    let (px, py) = side_points.(i) in
    let diff_x = abs (ball_x - px) in
    let diff_y = abs (ball_y - py) in
    if diff_x <= ball_radius && diff_y <= ball_radius then
      collision := true;
  done;
  !collision
;;

(* FILEPATH: /Users/dmytrohoncharenko/Documents/Programming/University/university-camlbrick/camlbrick.ml *)

(** 
  [game_test_hit_balls game balls] : est une fonction qui teste la collision entre le jeu et une liste de balles.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game est l'état actuel du jeu
  @param balls est la liste des balles à tester
*)
let game_test_hit_balls (game, balls : t_camlbrick * t_ball list) : unit =
  (* Iteration 3 *)
  ()
;;

(**
  [canvas_mouse_move] : Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
  de la souris dans la fenêtre lorsqu'elle se déplace. 
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game la partie en cours.
  @param x l'abscisse de la position de la souris
  @param y l'ordonnée de la position de la souris     
*)
let canvas_mouse_move(game,x,y : t_camlbrick * int * int) : unit = 
  ()
;;

(**
  [canvas_mouse_click_press] : Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
  de la souris dans la fenêtre lorsqu'un bouton est enfoncé. 
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game la partie en cours.
  @param button numero du bouton de la souris enfoncé.
  @param x l'abscisse de la position de la souris
  @param y l'ordonnée de la position de la souris     
*)
let canvas_mouse_click_press(game,button,x,y : t_camlbrick * int * int * int) : unit =
  ()
;;

(**
  [canvas_mouse_click_release] : Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
  de la souris dans la fenêtre lorsqu'un bouton est relaché. 
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game la partie en cours.
  @param button numero du bouton de la souris relaché.
  @param x l'abscisse de la position du relachement
  @param y l'ordonnée de la position du relachement   
*)
let canvas_mouse_click_release(game,button,x,y : t_camlbrick * int * int * int) : unit =
  ()
;;

(**
  [canvas_keypressed] : Cette fonction est appelée par l'interface graphique lorsqu'une touche du clavier est appuyée.
  Les arguments sont le jeu en cours, la touche enfoncé sous la forme d'une chaine et sous forme d'un code spécifique à labltk.
  
  Le code fourni initialement permet juste d'afficher les touches appuyées au clavier afin de pouvoir
  les identifiées facilement dans nos traitements.

  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID  
  @param game la partie en cours.
  @param keyString nom de la touche appuyée.
  @param keyCode code entier de la touche appuyée.   
*)
let canvas_keypressed(game, keyString, keyCode : t_camlbrick * string * int) : unit =
  print_string("Key pressed: ");
  print_string(keyString);
  print_string(" code=");
  print_int(keyCode);
  print_newline();
  let left_key_code : int = 65361 in
  let q_key_code : int = 113 in
  let right_key_code : int = 65363 in
  let d_right_code : int = 100 in

  if keyCode = left_key_code || keyCode = q_key_code then
    paddle_move_left game
  else if keyCode = right_key_code || keyCode = d_right_code then
    paddle_move_right game
  else
    ()
;;

(**
  [canvas_keyreleased] : Cette fonction est appelée par l'interface graphique lorsqu'une touche du clavier est relachée.
  Les arguments sont le jeu en cours, la touche relachée sous la forme d'une chaine et sous forme d'un code spécifique à labltk.
  
  Le code fourni initialement permet juste d'afficher les touches appuyées au clavier afin de pouvoir
  les identifiées facilement dans nos traitements.

  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game la partie en cours.
  @param keyString nom de la touche relachée.
  @param keyCode code entier de la touche relachée.   
*)
let canvas_keyreleased(game, keyString, keyCode : t_camlbrick * string * int) =
  print_string("Key released: ");
  print_string(keyString);
  print_string(" code=");
  print_int(keyCode);
  print_newline()
;;

(**
  Cette fonction est utilisée par l'interface graphique pour connaitre l'information
  l'information à afficher dans la zone Custom1 de la zone du menu.
*)
let custom1_text() : string =
  (* Iteration 4 *)
  "<Rien1>"
;;

(**
  Cette fonction est utilisée par l'interface graphique pour connaitre l'information
  l'information à afficher dans la zone Custom2 de la zone du menu.
*)
let custom2_text() : string =
  (* Iteration 4 *)
  "<Rien2>"
;;

(**
  [start_onclick] : Cette fonction est appelée par l'interface graphique lorsqu'on clique sur le bouton
  de la zone de menu et que ce bouton affiche "Start".

  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game la partie en cours.
*)
let start_onclick(game : t_camlbrick) : unit =
  if game.gamestate = PAUSING then
    let games : t_camlbrick = { game with gamestate = PLAYING } in
    ()
  else
    failwith "le jeu ne se met pas en pause"
;;

(**
  [stop_onclick] : Cette fonction est appelée par l'interface graphique lorsqu'on clique sur le bouton
  de la zone de menu et que ce bouton affiche "Stop".
  
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game la partie en cours.
*)
let stop_onclick(game : t_camlbrick) : unit =
  if game.gamestate = PLAYING then
    let games : t_camlbrick = { game with gamestate = PAUSING } in 
    ()
  else
    failwith "le jeu n'est pas en cours"
;;

(**
  [speed_get] : Cette fonction permet de récupérer la valeur de la vitesse du jeu. Elle est appelée par l'interface graphique pour obtenir la valeur de la vitesse du jeu.

  Vous pouvez donc renvoyer une valeur selon votre désir afin d'offrir la possibilité
  d'interagir avec le joueur.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game la partie en cours.
*)
let speed_get(game : t_camlbrick) : int = !(game.speed);;

(**
  [speed_change] : Cette fonction permet de gérer le changement de la valeur du slider Speed. Elle est appelée par l'interface graphique lorsqu'une action est effectuée sur le slider Speed. Elle modifie la valeur de la vitesse du jeu en fonction de la valeur du slider Speed. Elle affiche également la nouvelle valeur de la vitesse du jeu. Vous pouvez ajouter des traitements spécifiques pour gérer le changement de la vitesse du jeu.
  
  Ainsi, vous pourrez réagir selon le joueur.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game la partie en cours.
  @param xspeed la nouvelle valeur du slider Speed.
*)
let speed_change(game, xspeed : t_camlbrick * int) : unit=
  print_endline("Change speed : "^(string_of_int xspeed));
  game.speed := xspeed
;;

(**
  [animate_action] : Cette fonction permet de gérer les actions du jeu en cours. Elle est appelée à chaque itération du jeu pour mettre à jour l'état du jeu. Elle modifie la position de la balle en fonction de sa vitesse et des collisions avec les bords de l'écran. Elle gère également les collisions de la balle avec les briques et la raquette. Elle met à jour l'état du jeu en fonction des actions effectuées.

  @author Edouard GONET
  @author Dmytro HONCHARENKO
  @author Aya GUEMMIE
  @author Hasna AMID
  @param game l'état actuel du jeu
  @return ne retourne pas de valeur, elle modifie l'état du jeu
*)
let animate_action(game : t_camlbrick) : unit =  
  if game.gamestate = PLAYING then begin
  let ball = game.ball in
  ball.position := { 
    dx = !(ball.position).dx + !(ball.velocity).dx; 
    dy = !(ball.position).dy + !(ball.velocity).dy 
  };
  (* Gestion des collisions avec les bords de l'écran *)
  if !(ball.position).dx < 0 || !(ball.position).dx > game.param.world_width then
    ball.velocity := { !(ball.velocity) with dx = -(!(ball.velocity).dx) };
    (* Rebond sur les bords latéraux *)
  if !(ball.position).dy < 0 then
    ball.velocity := { !(ball.velocity) with dy = -(!(ball.velocity).dy) };
    (* Rebond sur le bord supérieur *)
  end
;;
    