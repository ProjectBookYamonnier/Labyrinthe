package fr.istic.si2.labyrinthe

import LabyrintheObject._
import CelluleObject._
import Utils._

object Jeu {

  /**
   * Chemin parcouru lors d'une déambulation dans un labyrinthe
   *
   * Le chemin déjà parcouru dans la labyrinthe, position courante incluse.
   * Il n'est jamais vide (on commence toujours à l'entrée du labyrinthe).
   * Le chemin est sauvegardé dans l'ordre inverse du parcours :
   *   - premier élément de la liste : position courante
   *   - dernier élément de la liste : entrée du labyrinthe
   */
  type Chemin = List[Position]

  /**
   *
   */
  type DirectionPossible = List[Direction]

  /**
   * @param laby un labyrinthe
   * @param chemin un chemin dans laby, sans cycle, c'est-à-dire ne passant jamais
   *        deux fois sur une même case.
   * @return vrai si le chemin mène à la sortie de laby, faux sinon.
   */
  def estResolu(laby: Labyrinthe, chemin: Chemin): Boolean = {
    chemin match {
      case h :: t => (h == sortieLabyrinthe(laby))
      case _      => false
    }
  }

  /**
   * @param laby
   * @return le chemin initial, c'est-à-dire celui avec lequel on commence à jouer.
   */
  def cheminInitial(laby: Labyrinthe): Chemin = (entreeLabyrinthe(laby) :: Nil)

  /**
   * @param chemin
   * @return le chemin correspondant à l'annulation du dernier déplacement dans chemin,
   *         si des déplacements ont déjà été effectués.
   */
  def annulerBouger(chemin: Chemin): Chemin = {
    chemin match {
      case Nil      => Nil
      case t :: Nil => chemin
      case h :: t   => t
    }
  }

  /**
   *  Direction des déplacements dans un labyrinthe
   *  Utile pour simplifier quelques fonctions
   *
   */
  sealed trait Direction
  case object Haut extends Direction
  case object Bas extends Direction
  case object Droite extends Direction
  case object Gauche extends Direction

  /**
   * @param p une position
   * @param d une direction
   * @return la position voisine de pos, suivant direction
   */
  def voisine(p: Position, d: Direction): Position = {
    (p, d) match {
      case ((x, y), Haut)   => (x - 1, y)
      case ((x, y), Bas)    => (x + 1, y)
      case ((x, y), Droite) => (x, y + 1)
      case ((x, y), Gauche) => (x, y - 1)
    }
  }

  val directions: List[Direction] = List(Haut, Gauche, Droite, Bas)

  /**
   * Fonction Auxiliaires de passageOuvert
   * @param laby un labyrinthe
   * @param p une position
   * @param d une direction
   * @return Une option de direction ssi le passage est possible sinon None
   */
  def mursEtBords(laby: Labyrinthe, p: Position, d: Direction): Option[Direction] = {
    p match {
      case (x, y) =>
        if (d == Haut || d == Droite) {
          if (d == Haut) {
            if (laby.f(p).haut == Ouvert && x != 0) Some(d)
            else None
          } else {
            if (laby.f(p).droit == Ouvert && y != laby.largeur - 1) Some(d)
            else None
          }
        } else if (d == Gauche) {
          if (laby.f(voisine(p, Gauche)).droit == Ouvert && y != 0) Some(d)
          else None
        } else {
          if (laby.f(voisine(p, Bas)).haut == Ouvert && x != laby.hauteur - 1) Some(d)
          else None
        }
    }
  }

  /**
   * @param laby un labyrinthe
   * @param p une position à l'intérieur du labyrinthe
   * @param d une direction
   * @return vrai s'il y a un passage depuis la position p
   *         dans la direction d, et faux sinon.
   *
   * @note   Souvenez-vous qu'un agencement de labyrinthe ne
   *         place pas nécessairement de murs sur les bords
   *         du labyrinthe, mais que les bords d'un
   *         labyrinthe ne peuvent pas être franchis.
   *         Par exemple, passageOuvert(laby,(0,0), Haut)
   *         doit être faux, même si la cellule en (0,0) n'a
   *         pas de mur en haut.
   *
   *         Indication de longueur : moins de 10 lignes
   */
  def passageOuvert(laby: Labyrinthe, p: Position, d: Direction): Boolean = {
    if (mursEtBords(laby, p, d) == Some(d)) true
    else false
  }

  /**
   * @param laby un labyrinthe
   * @param p une position dans laby
   * @return une liste de positions voisines accessibles depuis p dans laby
   */
  def voisines(laby: Labyrinthe, p: Position): List[Position] = {
    p match {
      case (x, 0) =>
        if (x == 0) { (voisine(p, Bas) :: voisine(p, Droite) :: Nil) }
        else if (x == laby.hauteur - 1) { (voisine(p, Haut) :: voisine(p, Droite) :: Nil) }
        else { (voisine(p, Haut) :: voisine(p, Droite) :: voisine(p, Bas) :: Nil) }
      case (0, y) =>
        if (y == laby.largeur - 1) { (voisine(p, Bas) :: voisine(p, Gauche) :: Nil) }
        else { (voisine(p, Gauche) :: voisine(p, Droite) :: voisine(p, Bas) :: Nil) }
      case (x, y) =>
        if (x == laby.hauteur - 1 && y == laby.largeur - 1) { (voisine(p, Gauche) :: voisine(p, Haut) :: Nil) }
        else if (x == laby.hauteur - 1) { (voisine(p, Haut) :: voisine(p, Droite) :: voisine(p, Gauche) :: Nil) }
        else if (y == laby.largeur - 1) { (voisine(p, Haut) :: voisine(p, Gauche) :: voisine(p, Bas) :: Nil) }
        else { (voisine(p, Bas) :: voisine(p, Haut) :: voisine(p, Droite) :: voisine(p, Gauche) :: Nil) }
    }
  }

  /**
   * @param laby un labyrinthe
   * @param chemin un chemin dans le labyrinthe laby
   * @param direction une direction
   * @return l'extension du chemin de un pas dans la direction donnée,
   *         si possible, ou sinon le chemin donné
   */
  def bouge(laby: Labyrinthe, chemin: Chemin, direction: Direction): Chemin = {
    if (passageOuvert(laby, chemin.head, direction) && chemin.head != sortieLabyrinthe(laby)) voisine(chemin.head, direction) :: chemin
    else chemin
  }

  /**
   * @param laby un labyrinthe
   * @param chemin un chemin dans le labyrinthe laby
   * @return une fonction décrivant l'état de chaque cellule du labyrinthe
   *         laby
   *
   * @note cf. cellule.scala, definition de EtatCellule
   */
  def etatLabyrinthe(laby: Labyrinthe, chemin: Chemin): EtatLabyrinthe = {
    (p: Position) =>
      if (p == chemin.head) Courante
      else if (chemin.contains(p)) Visitee
      else NonVisitee
  }

  /* Résolution */

  /**
   * @param laby un labyrinthe
   * @param depart une position valide dans le labyrinthe
   * @param arrivee une position valide dans le labyrinthe
   * @return un chemin de depart à arrivee sans boucles, ou, si cela
   *         n'est pas possible, le chemin List(depart)
   *
   * @note Suggestions :
   *
   *       - définir une fonction récursive auxiliaire etendre : Chemin => Chemin qui
   *       étend un chemin donné à un chemin allant jusqu'à arrivee (si possible)
   *       et l'appliquer à List(depart).
   *
   *       - pour définir etendre(chemin1), utiliser filter et map pour exprimer l'idée suivante :
   *       une extension de chemin1 jusqu'à arrivée est obtenue en étendant chemin1 d'une position
   *       (pour chacune des voisines de la tête de chemin1) et en appliquant récursivement etendre
   *       à ces chemins augmentés. Parmi les extensions possibles obtenues, on garde la première qui
   *       va effectivement jusqu'à arrivee, s'il y en a, ou sinon on retourne chemin1.
   *
   *       - indication de longueur : moins de 10 lignes
   *
   */
  def resoudreDeA(laby: Labyrinthe, depart: Position, arrivee: Position): Chemin = {
    etendre(laby, depart, arrivee, (depart :: Nil) :: Nil)
  }

  /**
   * @param laby un labyrinthe
   * @param chemin qui est un chemin menant vers arrivee qui se completera dans cette fonction
   * @param depart est la position de depart
   * @param arrivee est la position de l'arrivee
   * @return un chemin partant de depart en menant à arrivee si c'est possible sinon List(depart)
   */
  def etendre(laby: Labyrinthe, depart: Position, arrivee: Position, listChemin: List[Chemin]): Chemin = {
    listChemin match {
      case Nil => List(depart)
      case chemin :: Nil =>
        if (chemin.head == arrivee) {
          chemin
        } else {
          etendre(laby, depart, arrivee, positionPossible(laby, List(Gauche, Bas, Droite, Haut), chemin.head).filterNot(p => chemin.contains(p)).map(p => p :: chemin))
        }
      case ((depart :: Nil) :: rest) => etendre(laby, depart, arrivee, rest)
      case chemin :: rest => if (chemin.head == arrivee) {
        chemin
      } else {
        etendre(laby, depart, arrivee, List(etendre(laby, depart, arrivee, List(chemin))) ++ rest)
      }
    }
  }

  /**
   * @param ld Une liste de Direction issue de directionPossible
   * @param p une possition dans le labyrinthe
   * @return une liste de position possible
   */
  def positionPossible(laby: Labyrinthe, ld: List[Direction], p: Position): List[Position] = {
    ld match {
      case Nil => Nil
      case (h :: s) =>
        if (passageOuvert(laby, p, h)) voisine(p, h) :: positionPossible(laby, s, p)
        else positionPossible(laby, s, p)
    }
  }

  /**
   * @param laby un labyrinthe
   * @param chemin un chemin valide dans le labyrinthe laby
   * @return si cela est possible, un chemin étendant le chemin donné,
   *         menant à la sortie de laby, sans passer deux fois par la
   *         même case (sauf s'il faut rebrousser le chemin
   *         donné).
   *         Si cela n'est pas possible, le chemin donné argument
   *         est renvoyé.
   *
   * @note utiliser resoudreDeA.
   *       Indication de longueur : 1 ou 2  lignes
   *
   */
  def resoudre(laby: Labyrinthe, chemin: Chemin): Chemin = {
    if ((resoudreDeA(laby, chemin.head, sortieLabyrinthe(laby))).contains(sortieLabyrinthe(laby))) resoudreDeA(laby, chemin.head, sortieLabyrinthe(laby))
    else chemin
  }

  /**
   * @param laby un labyrinthe
   * @param chemin un chemin valide dans le labyrinthe laby
   * @return Si cela est possible, un chemin étendant le chemin donné
   *         d'une seule case le long d'un chemin solution.
   *         Si cela n'est pas possible, le chemin donné est renvoyé.
   *
   * @note utiliser la fonction resoudreDeA
   *
   *       Indication de longueur : 1 ou 2  lignes
   *
   *       La fonction takeRight : List[T] => Int => List[T]
   *       peut être utile. xs.takeRight(n) renvoie les n derniers
   *       éléments de la liste xs.
   */
  def indice(laby: Labyrinthe, chemin: Chemin): Chemin = {
    val resolu = resoudreDeA(laby, chemin.head, sortieLabyrinthe(laby))
    (resolu.takeRight(2)).head :: chemin
  }
}