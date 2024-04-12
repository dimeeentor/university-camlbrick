(**
Ce module Camlbrick représente le noyau fonctionnel du jeu de casse-brique nommé <b>camlbrick</b>
(un jeu de mot entre le jeu casse-brique et le mot ocaml).

Le noyau fonctionnel consiste à réaliser l'ensemble des structures et autres fonctions capables
d'être utilisées par une interface graphique. Par conséquent, dans ce module il n'y a aucun
aspect visuel! Vous pouvez utiliser le mode console.

Le principe du jeu de casse-brique consiste à faire disparaître toutes les briques d'un make_camlbrickiveau
en utilisant les rebonds d'une balle depuis une raquette contrôlée par l'utilisateur.

@author Edouard GONET
@author Dmytro HOCHARENCKO
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
  type énuméré représentant les couleurs gérables par notre moteur de jeu. Vous ne pouvez pas modifier ce type!
  @deprecated Ne pas modifier ce type! 
*)

type t_camlbrick_color = WHITE | BLACK | GRAY | LIGHTGRAY | DARKGRAY | BLUE | RED | GREEN | YELLOW | CYAN | MAGENTA | ORANGE | LIME | PURPLE;;
    
(**
  Cette structure regroupe tous les attributs globaux,
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
  Cette fonction renvoie le type de brique pour représenter les briques de vide.
  C'est à dire, l'information qui encode l'absence de brique à un emplacement sur la grille du monde.
  @return Renvoie le type correspondant à la notion de vide.
  @deprecated  Cette fonction est utilisé en interne.    
*)

let make_empty_brick() : t_brick_kind = 
  BK_empty
;;
    
(** 
  Enumeration des différentes tailles des billes. 
  La taille  normale d'une bille est [BS_MEDIUM]. 
      
  Vous pouvez ajouter d'autres valeurs sans modifier les valeurs existantes.
*)

type t_ball_size = BS_SMALL | BS_MEDIUM | BS_BIG;;
      
(** 
  Enumeration des différentes taille de la raquette. Par défaut, une raquette doit avoir la taille
  [PS_SMALL]. 
    
  Vous pouvez ajouter d'autres valeurs sans modifier les valeurs existantes.
*)

type t_paddle_size = PS_SMALL | PS_MEDIUM | PS_BIG;;
    
(** 
  Enumération des différents états du jeu. Nous avons les trois états de base:
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
*)

type t_vec2 = {dx : int; dy : int} ;; 
  
(**
  Cette fonction permet de créer un vecteur 2D à partir de deux entiers.
  Les entiers représentent la composante en X et en Y du vecteur.
    
  Vous devez modifier cette fonction.
  @autor Edouard GONET
  @param x première composante du vecteur
  @param y seconde composante du vecteur
  @return Renvoie le vecteur dont les composantes sont (x,y).
*)
   
let make_vec2(x, y : int * int) : t_vec2 = 
  {dx = x; dy = y}
;;
    
(**
  Cette fonction renvoie un vecteur qui est la somme des deux vecteurs donnés en arguments.

  @autor Dmytro Honcharenko
  @param a premier vecteur
  @param b second vecteur
  @return Renvoie un vecteur égale à la somme des vecteurs.
*)
   
let vec2_add(a,b : t_vec2 * t_vec2) : t_vec2 =
  {dx = a.dx + b.dx; dy = a.dy + b.dy}
;;
     
(**
  Cette fonction renvoie un vecteur égale à la somme d'un vecteur
  donné en argument et un autre vecteur construit à partir de (x,y).
      
  Cette fonction est une optimisation du code suivant (que vous ne devez pas faire en l'état):
  {[
    let vec2_add_scalar(a,x,y : t_vec2 * int * int) : t_vec2 =
      vec2_add(a, make_vec2(x,y))
    ;;
  ]}
    
  @autor Dmytro Honcharenko
  @param a premier vecteur
  @param x composante en x du second vecteur
  @param y composante en y du second vecteur
  @return Renvoie un vecteur qui est la résultante du vecteur 
*)

let vec2_add_scalar(a,x,y : t_vec2 * int * int) : t_vec2 =
  {dx = a.dx + x; dy = a.dy + y}
;;
    
(**
  Cette fonction calcul un vecteur où 
  ses composantes sont la résultante de la multiplication  des composantes de deux vecteurs en entrée.
  Ainsi,
  {[
    c_x = a_x * b_x
    c_y = a_y * b_y
  ]}

  @author Hasna AMID 
  @param a premier vecteur
  @param b second vecteur
  @return Renvoie un vecteur qui résulte de la multiplication des composantes. 
*)

let vec2_mult(a,b : t_vec2 * t_vec2) : t_vec2 = 
  {dx = a.dx * b.dx; dy = a.dy * b.dy}
;;
   
(**
  Cette fonction calcul la multiplication des composantes du vecteur a et du vecteur construit à partir de (x,y).
  Cette fonction est une optimisation du code suivant (que vous ne devez pas faire en l'état):
  {[
    let vec2_mult_scalar(a,x,y : t_vec2 * int * int) : t_vec2 =
      vec2_mult(a, make_vec2(x,y))
    ;;
  ]}

  @author Aya GUEMMIE
  @param a vecteur de type t_vec2
  @param x entier qui va multiplier le scalaire du vecteur a 
  @param y entier qui va multiplier le scalaire du vecteur a 
  @return retourne un nouveau vecteur avec la multiplications des correspondantes du vecteurs a 
*)

let vec2_mult_scalar(a,x,y : t_vec2 * int * int) : t_vec2 =
  {dx = a.dx * x; dy = a.dy * y}
;;
  
(**
  Cette structure regroupe tous les attributs globaux, pour paramétrer notre balle.
*)
  
type t_ball = {
              position : t_vec2; (**Position de la balle*)
              frame : t_vec2; (**Cadre de la balle*)
              size : t_ball_size; (**Taille de la balle*)
              color : t_camlbrick_color; (**Couleur de la balle*)
              }
;; 

(**
  Cette structure regroupe tous les attributs globaux, pour paramétrer notre raquette.
*)
    
type t_paddle = {
                position : t_vec2;
                size : t_paddle_size; (**Définit la taille de notre raquette*)
                speed : int ref; (**définit sa vitesse de déplacement*)
                x : int ref; (**Position au niveau de l'absisse*)
                y : int ref; (**Position au niveau de l'ordonnée*)
                }
;;

(**
   Stock les paramètres de jeux. Ce type est utile dans la modélisation des opérations géométriques 2D, comme la modélisation 
   de mouvement dans le jeux ou même le rendu graphique.
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
                    }
;;

(**
  Cette fonction construit le paramétrage du jeu, avec des informations personnalisable avec les contraintes du sujet.
  Il n'y a aucune vérification et vous devez vous assurer que les valeurs données en argument soient cohérentes.
  @return Renvoie un paramétrage de jeu par défaut      
*)

let make_camlbrick_param() : t_camlbrick_param = 
  {
    world_width = 800;
    world_bricks_height = 600;
    world_empty_height = 200;
      
    brick_width = 40;
    brick_height = 20;
      
    paddle_init_width = 100;
    paddle_init_height = 20;
      
    time_speed = ref 20;
  }
;;
    
    
(**
  Cette fonction extrait le paramétrage d'un jeu à partir du jeu donné en argument.

  @author Aya GUEMMI 
  @param game jeu en cours d'exécution.
  @return Renvoie le paramétrage actuel.
*)

let param_get(game : t_camlbrick) : t_camlbrick_param =
  game.param
;;

(**
  Cette fonction génère aléatoirement un type de brique parmi plusieurs types prédéfinis.

  @author Edouard GONET
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
  Cette fonction va créer une grille de briques choisis aléatoirement avec les dimensions données.
  
  @author Edouard GONET
  @param a
  @param b 
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
  Cette fonction crée une nouvelle balle avec la position et la taille spécifiées.

  @author
  @param x entiers représentant les coordonnées de la position initiale de la balle
  @param y entiers représentant les coordonnées de la position initiale de la balle
  @param size indique la taille de la balle 
  @return retourne une nouvelle instance de la structure t_ball, qui représente une balle dans le jeu
*)
    
let make_ball(x,y, size : int * int * t_ball_size) : t_ball = 
 {position = make_vec2 (0, 0);
  frame = make_vec2 (0, 0);
  size = size;
  color = GRAY
  }
;;

(**
  Cette fonction crée une raquette par défaut au milieu de l'écran et de taille normal.  
  @deprecated Cette fonction est là juste pour le debug ou pour débuter certains traitements de test.
*)

let make_paddle () : t_paddle = 
  {
    position = make_vec2(0, 0);
    size = PS_SMALL;
    speed = ref 1;
    x = ref 1;
    y = ref 1
  } 
;;     

(**
  Cette fonction crée une nouvelle structure qui initialise le monde avec aucune brique visible.
  Une raquette par défaut et une balle par défaut dans la zone libre.

  @author Edouard GONET
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
  Fonction utilitaire qui permet de traduire l'état du jeu sous la forme d'une chaîne de caractère.
  Cette fonction est appelée à chaque frame, et est affichée directement dans l'interface graphique.
      
  Vous devez modifier cette fonction.
    
  @autho Hasna AMID
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
  Cette fonction renvoie le type de brique à partir des coordonnées dans la zone de briques
  en vérifiant si les paramètres se trouve à l'intérieur du tableau bricks.
    
  @autor Edouard GONET 
  @param game représente une matrice de type t_brick_kind array array
  @param i coordonnée d'une brique dans le tableau 
  @param j coordonnée d'une brique dans le tableau
  @return renvoie le type de brique à partir des coordonnées dans la zone de briques
*)

let brick_get(game, i , j : t_camlbrick * int * int) : t_brick_kind =
  if i < 0 || j < 0 || i >= Array.length(game.array) || j >= Array.length(game.array.(0)) (*vérifie que i, j sont à l'intèrieur de bricks*)
  then BK_empty 
  else game.array.(i).(j) (*accede a bricks a x, y et renvoie son type*)
;;
 
(**
  Cette fonction réalise des modifications dans la zone de brique pour faire évoluer une brique comme si elle était touchée 
  par une balle.

  @author Edouard GONET
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
  Cette fonction envoie une couleur en fonction du type de brique qu'elle a.  
  
  @author Hasna AMID
  @param game représente une matrice de type t_brick_kind array array 
  @param i coordonée qui représente l'indice de ligne de la matrice game
  @param j coordonée qui représente l'indice de ligne de la matrice game
  @return retourne la couleur d'une brique associé son type 
*)

let brick_color(game, i, j : t_camlbrick * int * int) : t_camlbrick_color =
  if game.array.(i).(j) = BK_simple 
  then YELLOW
  else 
    if game.array.(i).(j) = BK_double 
    then BLUE
    else 
      if game.array.(i).(j) = BK_bonus 
      then GREEN
      else 
        if game.array.(i).(j) = BK_block
        then WHITE
        else BLACK
;;

(**
  Cette fonction renvoie la position gauche du rectangle sympolisant la position de la raquette 

  @author 
  @param game représentant l'état actuel du jeu
  @return renvoie la position gauche du rectangle qui symbolise la raquette
*)
    
let paddle_x (game: t_camlbrick) : int =
  !(game.pad.x) (* position horizontale de la raquette *)
;;

(**
  Cette fonction renvoie la largeur en pixel du rectangle 

  @author
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
  Cette fonction va nous permettre de deplacer la raquette vers la gauche sur l'axe des abscisses.
  
  @author
  @param game représentant l'état actuel du jeu
  @return ne retourne pas de valeur, elle modifie l'état du jeu
*)

let paddle_move_left(game : t_camlbrick) : unit = 
  let move_left : int = game.param.paddle_init_width in
  if paddle_x(game) <= 0
  then ()
  else
      if paddle_x(game) > 0
      then game.pad.x := !(game.pad.x) - move_left
      else ()
;;

(**
  Cette fonction va nous permettre de deplacer la raquette vers la droite sur l'axe des abscisses.
  
  @author
  @param game représentant l'état actuel du jeu
  @return ne retourne pas de valeur, elle modifie l'état du jeu
*)

let paddle_move_right(game : t_camlbrick) : unit = 
  let move_right : int = game.param.paddle_init_width in
  if paddle_x(game) >= 0
  then ()
  else
      if paddle_x(game) < 0
      then game.pad.x := !(game.pad.x) + move_right
      else ()
;;

(**
  Cette fonction indique si la partie en cours possède des balles. Nous vérifions si la brique actuelle (current_brick) 
  contient des balles. Si c'est le cas, nous renvoyons true. Sinon, nous continuons de vérifier les briques restantes 
  dans la liste check. Si aucune brique ne contient de balles, nous renvoyons false.
  
  @author
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
  Cette fonction compte le nombre total de balles dans le jeu en utilisant une fonction récursives internes pour parcourirs la liste de brique.

  @author
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
  Cette fonction récupère toutes les balles présentes dans le jeu en utilisant une fonction récursive interne sur le même principe que la fonction précédente.

  @author
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
  Cette fonction récupère une balle spécifique dans le jeu représenté par le type t_camlbrick en fonction de l'indice fourni. 
  
  @author
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
  Cette fonction extrait et retourne la position horizontale de la balle spécifiée dans le jeu. 

  @author
  @param game représentant l'état actuel du jeu
  @param ball représentant la balle dont nous voulons obtenir la position
  @return retourne la valeur de position horizontale de la balle
*)

let ball_x(game, ball : t_camlbrick * t_ball) : int = 
  game.ball.position.dx
;;

(**
  Cette fonction extrait et retourne la position verticale de la balle spécifiée dans le jeu. 

  @author
  @param game représentant l'état actuel du jeu
  @param ball représentant la balle dont nous voulons obtenir la position
  @return retourne la valeur de position verticale de la balle
*)
    
let ball_y(game, ball : t_camlbrick * t_ball) : int = 
  game.ball.position.dy
;;

(**
  cette fonction retourne la taille en pixels de la balle spécifiée dans le jeu, en fonction de sa taille définie.
  
  @author
  @param game représentant l'état actuel du jeu
  @param ball représentant la balle dont nous voulons obtenir la taille en pixels
  @return retourne un entier représentant la taille en pixels de la balle 
*)
    
let ball_size_pixel(game, ball : t_camlbrick * t_ball) : int =
if game.ball.size = BS_SMALL 
  then 10
  else 
    if game.ball.size = BS_MEDIUM 
    then 20
    else 30
;;

(**
  cette fonction extrait et retourne la couleur de la balle

  @author
  @param game représentant l'état actuel du jeu
  @param ball représentant la balle dont nous voulons obtenir la couleur
  @return retourne la couleur de la balle en jeu
*)
    
let ball_color(game, ball : t_camlbrick * t_ball) : t_camlbrick_color =  
  if ball.size = BS_SMALL 
  then LIME 
  else  
    if ball.size = BS_MEDIUM 
    then LIGHTGRAY 
    else GRAY 
;;

(*
let ball_modif_speed(game, ball, dv : t_camlbrick * t_ball * t_vec2) : unit =
  ball.speed := vec2_add (!(ball.speed), dv)
;;


let ball_modif_speed_sign(game, ball, sv : t_camlbrick * t_ball * t_vec2) : unit =
  ball.speed := vec2_mult (!(ball.speed), sv)
;;*)

(**
  cette fonction détermine si un point spécifié est situé à l'intérieur d'un cercle donné en comparant la distance entre le point et 
  le centre du cercle à la valeur du rayon. Si la distance est inférieure au rayon, le point est à l'intérieur du cercle, sinon il est à l'extérieur.

  @author
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
  cette fonction vérifie si un point spécifié est situé à l'intérieur d'un quadrilatère défini par ses coins, en comparant les coordonnées 
  du point avec les coordonnées des coins du quadrilatère.

  @author 
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
   
    
    
    
    let ball_remove_out_of_border(game,balls : t_camlbrick * t_ball list ) : t_ball list = 
      (* Itération 3 *)
      balls
    ;;
    
    let ball_hit_paddle(game,ball,paddle : t_camlbrick * t_ball * t_paddle) : unit =
      (* Itération 3 *)
      ()
    ;;
    
    
    (* lire l'énoncé choix à faire *)
    let ball_hit_corner_brick(game,ball, i,j : t_camlbrick * t_ball * int * int) : bool =
      (* Itération 3 *)
      false
    ;;

    
let ball_hit_corner_brick(game, ball, i, j : t_camlbrick * t_ball * int * int) : bool = let brick_x = i * game.param.brick_width in let brick_y = j * game.param.brick_height in let corner_1 : int = brick_x in let corner_2 : int = brick_y in let corner_3 : int = brick_x + game.param.brick_width in let corner_4 : int = brick_y + game.param.brick_height in if is_inside_quad(corner_1, corner_2, corner_3, corner_4, ball_x(game, ball), ball_y(game, ball)) then true else false ;;

    
    (* lire l'énoncé choix à faire *)
    let ball_hit_side_brick(game,ball, i,j : t_camlbrick * t_ball * int * int) : bool =
      (* Itération 3 *)
      false
    ;;
    
    let game_test_hit_balls(game, balls : t_camlbrick * t_ball list) : unit =
      (* Itération 3 *)
      ()
    ;;
    
    (**
      Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
      de la souris dans la fenêtre lorsqu'elle se déplace. 
      Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
      un impact sur les performances si vous dosez mal les temps de calcul.
      @param game la partie en cours.
      @param x l'abscisse de la position de la souris
      @param y l'ordonnée de la position de la souris     
    *)
    let canvas_mouse_move(game,x,y : t_camlbrick * int * int) : unit = 
      ()
    ;;
    
    (**
      Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
      de la souris dans la fenêtre lorsqu'un bouton est enfoncé. 
      Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
      un impact sur les performances si vous dosez mal les temps de calcul.
      @param game la partie en cours.
      @param button numero du bouton de la souris enfoncé.
      @param x l'abscisse de la position de la souris
      @param y l'ordonnée de la position de la souris     
    *)
    let canvas_mouse_click_press(game,button,x,y : t_camlbrick * int * int * int) : unit =
      ()
    ;;
    
    
    (**
      Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
      de la souris dans la fenêtre lorsqu'un bouton est relaché. 
      Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
      un impact sur les performances si vous dosez mal les temps de calcul.
      @param game la partie en cours.
      @param button numero du bouton de la souris relaché.
      @param x l'abscisse de la position du relachement
      @param y l'ordonnée de la position du relachement   
    *)
    let canvas_mouse_click_release(game,button,x,y : t_camlbrick * int * int * int) : unit =
      ()
    ;;
    
    
    
    (**
      Cette fonction est appelée par l'interface graphique lorsqu'une touche du clavier est appuyée.
      Les arguments sont le jeu en cours, la touche enfoncé sous la forme d'une chaine et sous forme d'un code
      spécifique à labltk.
      
      Le code fourni initialement permet juste d'afficher les touches appuyées au clavier afin de pouvoir
      les identifiées facilement dans nos traitements.
    
      Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
      un impact sur les performances si vous dosez mal les temps de calcul.
      @param game la partie en cours.
      @param keyString nom de la touche appuyée.
      @param keyCode code entier de la touche appuyée.   
    *)

    let canvas_keypressed(game, keyString, keyCode : t_camlbrick * string * int) : unit =
      print_string("Key pressed: ");
      print_string(keyString);
      print_string(" code=");
      print_int(keyCode);
      print_newline()
    ;;
    
    (**
      Cette fonction est appelée par l'interface graphique lorsqu'une touche du clavier est relachée.
      Les arguments sont le jeu en cours, la touche relachée sous la forme d'une chaine et sous forme d'un code
      spécifique à labltk.
      
      Le code fourni initialement permet juste d'afficher les touches appuyées au clavier afin de pouvoir
      les identifiées facilement dans nos traitements.
    
      Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
      un impact sur les performances si vous dosez mal les temps de calcul.
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
      Cette fonction est appelée par l'interface graphique lorsqu'on clique sur le bouton
      de la zone de menu et que ce bouton affiche "Start".
    
      
      Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
      un impact sur les performances si vous dosez mal les temps de calcul.
      @param game la partie en cours.
    *)
    let start_onclick(game : t_camlbrick) : unit=
      ()
    ;;
    
    (**
      Cette fonction est appelée par l'interface graphique lorsqu'on clique sur le bouton
      de la zone de menu et que ce bouton affiche "Stop".
    
      
      Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
      un impact sur les performances si vous dosez mal les temps de calcul.
      @param game la partie en cours.
    *)
    let stop_onclick(game : t_camlbrick) : unit =
      ()
    ;;
    
    (**
      Cette fonction est appelée par l'interface graphique pour connaitre la valeur
      du slider Speed dans la zone du menu.
    
      Vous pouvez donc renvoyer une valeur selon votre désir afin d'offrir la possibilité
      d'interagir avec le joueur.
    *)
    let speed_get(game : t_camlbrick) : int = 
      0
    ;;
    
    
    (**
      Cette fonction est appelée par l'interface graphique pour indiquer que le 
      slide Speed dans la zone de menu a été modifiée. 
      
      Ainsi, vous pourrez réagir selon le joueur.
    *)
    let speed_change(game,xspeed : t_camlbrick * int) : unit=
      print_endline("Change speed : "^(string_of_int xspeed));
    ;;
    
    
    
    let animate_action(game : t_camlbrick) : unit =  
      (* Iteration 1,2,3 et 4
        Cette fonction est appelée par l'interface graphique à chaque frame
        du jeu vidéo.
        Vous devez mettre tout le code qui permet de montrer l'évolution du jeu vidéo.    
      *)
      ()
    ;;
    