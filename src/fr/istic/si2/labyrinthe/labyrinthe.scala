package fr.istic.si2.labyrinthe

import CelluleObject._
import fr.istic.si2.scribble._
import Utils._

object LabyrintheObject {

  /**
   * Positions des cellules dans un labyrinthe
   *
   * (0,0) est la position « en haut à gauche » d'un labyrinthe
   */
  type Position = (Int, Int)

  /**
   * Agencement des murs d'un labyrinthe
   *
   * Un agencement est une fonction qui associe une cellule
   * à chaque position, indiquant ainsi les emplacements
   * des murs.
   *
   * Un agencement est défini pour toutes les positions
   * d'un plan infini et décrit en théorie un labyrinthe
   * infini dans les quatre directions.
   *
   * Cependant, pour un labyrinthe de dimensions données,
   * peu importe l'agencement des murs en dehors du labyrinthe.
   * L'agencement ne sera utilisé que pour l'intérieur du
   * labyrinthe.
   *
   */
  type Agencement = Position => Cellule

  /**
   *  Agencement avec des murs partout
   *
   *  @note Utiliser une fonction anonyme.
   *        La solution tient en une courte ligne.
   *
   */
  val agencementPlein: Agencement = { case (_, _) => Cellule(Ferme, Ferme) }

  /**
   * Agencement avec des murs nulle part
   */
  val agencementVide: Agencement = { case (_, _) => Cellule(Ouvert, Ouvert) }

  /**
   * Type des labyrinthes
   * @constructor crée un nouveau labyrinthe.
   * @param hauteur le nombre de lignes du labyrinthe
   * @param largeur le nombre de colonnes du labyrinthe
   * @param f l'agencement des murs intérieurs du labyrinthe
   *
   * @note - Par convention, la cellule (0,0) est « en haut à gauche. »
   *
   *       - L'agencement n'a besoin d'être correctement défini que
   *         sur les murs intérieurs du labyrinthe. En particulier,
   *         l'agencement peut ou non disposer des murs le long
   *         des bords du labyrinthe ; il n'en sera pas tenu compte
   *         pour afficher le labyrinthe ou lors du jeu (Les bords
   *         d'un labyrinthe sont toujours infranchissables).
   *
   */
  case class Labyrinthe(hauteur: Int, largeur: Int, f: Agencement)

  /**
   * @param hauteur nombre de lignes d'un labyrinthe
   * @param largeur nombre de colonnes d'un labyrinthe
   * @return un agencement en serpentin « horizontal »,
   *         allant de l'entrée à la sortie du labyrinthe.
   *         La ligne du bas doit s'ouvrir vers le haut à
   *         droite (en fin de ligne).
   *
   * @example serpentins de dimensions 4 x 5 et 5 x 5
   *
   *  ┏━━━━━━━━━┓           ┏━━━━━━━━━┓
   *  ┣━━━━━━━╸ ┃           ┃ ╺━━━━━━━┫
   *  ┃ ╺━━━━━━━┫           ┣━━━━━━━╸ ┃
   *  ┣━━━━━━━╸ ┃           ┃ ╺━━━━━━━┫
   *  ┗━━━━━━━━━┛           ┣━━━━━━━╸ ┃
   *                        ┗━━━━━━━━━┛
   *
   */
  def serpentinH(hauteur: Int, largeur: Int): Agencement = {
    if (hauteur % 2 == 0) {
      case (x, y) =>
        if (x % 2 == 0 && y != 0) Cellule(Ferme, Ouvert)
        else if (x % 2 == 1 && y < largeur - 1) Cellule(Ferme, Ouvert)
        else Cellule(Ouvert, Ouvert)
    } else {
      case (x, y) =>
        if (x % 2 == 0 && y < largeur - 1) Cellule(Ferme, Ouvert)
        else if (x % 2 == 1 && y != 0) Cellule(Ferme, Ouvert)
        else Cellule(Ouvert, Ouvert)
    }
  }

  /**
   * @param hauteur hauteur d'un labyrinthe
   * @param largeur largeur d'une labyrinthe
   * @return un agencement où tous les passages
   *         sont fermés sauf des couloirs en bas
   *         et à droite.
   * @example agencementBD(4,5) est représenté par
   *  ┏━┳━┳━┳━┳━┓
   *  ┣━╋━╋━╋━┫ ┃
   *  ┣━╋━╋━╋━┫ ┃
   *  ┣━┻━┻━┻━┛ ┃
   *  ┗━━━━━━━━━┛
   */
  def agencementBD(hauteur: Int, largeur: Int): Agencement = {
    case (x, y) =>
      if (x < hauteur - 1 && y < largeur - 1) Cellule(Ferme, Ferme)
      else if (x < hauteur && y < largeur - 1) Cellule(Ferme, Ouvert)
      else Cellule(Ouvert, Ouvert)
  }

  /**
   * !!! Un peu difficile et non essentiel, vous pouvez y revenir
   * !!! plus tard, si vous avez le temps.
   *
   * @param laby un labyrinthe
   * @param un labyrinthe « miroir » de laby par rapport
   *        à l'axe passant par l'entrée et la sortie.
   *        En particulier l'entrée et la sortie restent en place.
   *
   * @example deux labyrinthes et leurs transposées
   *
   *  ┏━━━━━━━┓    ┏━┳━┳━┳━┳━┓         ┏━━━━━━━━━━━┓    ┏━━━┳━━━┓
   *  ┃ ┏━┳━┳━┫    ┣━╋━╋━╋━┫ ┃         ┣━━━━━━━━━╸ ┃    ┃ ╻ ┃ ╻ ┃
   *  ┃ ┣━╋━╋━┫    ┣━╋━╋━╋━┫ ┃         ┃ ╺━━━━━━━━━┫    ┃ ┃ ┃ ┃ ┃
   *  ┃ ┣━╋━╋━┫    ┣━┻━┻━┻━┛ ┃         ┣━━━━━━━━━╸ ┃    ┃ ┃ ┃ ┃ ┃
   *  ┃ ┣━╋━╋━┫    ┗━━━━━━━━━┛         ┗━━━━━━━━━━━┛    ┃ ┃ ┃ ┃ ┃
   *  ┗━┻━┻━┻━┛                                         ┃ ┃ ╹ ┃ ┃
   *                                                    ┗━┻━━━┻━┛
   *
   *  @note remarquez que les murs en haut deviennent des murs à droite
   *        et vice-versa.
   */
  def transpose(laby: Labyrinthe): Labyrinthe = ???

  /**
   * !!! Très difficile.
   * !!! Optionnel, à ne tenter que si vous avez fini
   * !!! le reste du projet et vous avez le temps.
   *
   * @param h la hauteur d'un labyrinthe, tel que h <= l
   * @param l la largeur d'un labyrinthe, tel que h <= l
   * @return l'agencement en tourbillon d'un labyrinthe parfait
   *         comme dans les exemples ci-dessous.
   *         Lorsque cela est possible, l'unique solution doit
   *         passer par toutes les cases du labyrinthe.
   *
   * @example
   * ┏━━━━━━━━━━━━━━━┓
   * ┃ ┏━━━━━━━━━━━━━┫           ┏━━━━━━━━━┓
   * ┃ ┃ ┏━━━┳━━━┳━┓ ┃           ┃ ┏━━━━━━━┫
   * ┃ ┃ ╹ ╻ ╹ ╻ ╹ ┃ ┃           ┃ ┃ ╺━━━┓ ┃
   * ┃ ┗━━━┻━━━┻━╸ ┃ ┃           ┃ ┗━━━╸ ┃ ┃
   * ┣━━━━━━━━━━━━━┛ ┃           ┣━━━━━━━┛ ┃
   * ┗━━━━━━━━━━━━━━━┛           ┗━━━━━━━━━┛
   *
   * Ici, une case ne fait       Ici, toutes les cases
   * paspartie de la solution.   font partie de la
   *                             solution.
   */

  def tourbillon(h: Int, l: Int): Agencement = ??? // TODO

  /**
   * @param hauteur nombre de lignes d'un labyrinthe
   * @param largeur nombre de colonnes d'un labyrinthe
   * @param g une fonction donnant un agencement de dimensions données
   * @return le labyrinthe de dimensions hauteur x largeur et d'agencement
   *         g(hauteur, largeur)
   */
  def labyrinthe(
    hauteur: Int,
    largeur: Int,
    g:       (Int, Int) => Agencement): Labyrinthe = Labyrinthe(hauteur, largeur, g(hauteur, largeur))

  /* Affichage graphique d'un labyrinthe */

  /**
   * @param laby un labyrinthe
   * @return la position de l'entrée du labyrinthe laby,
   *         qui par convention est en « bas à gauche. »
   */
  def entreeLabyrinthe(laby: Labyrinthe): Position = (laby.hauteur - 1, 0)

  /**
   * @param laby un labyrinthe
   * @return la position de la sortie du labyrinthe laby,
   *         qui par convention est en « en haut à droite. »
   */
  def sortieLabyrinthe(laby: Labyrinthe): Position = (0, laby.largeur - 1)

  /**
   * EtatLabyrinthe
   *
   * L'état d'un labyrinthe décrit les états de toutes ses cellules.
   *
   * Voir la documentation de [[EtatCellule]] pour plus d'information.
   */
  type EtatLabyrinthe = Position => EtatCellule

  /**
   * @param laby un labyrinthe
   * @return l'état initial du labyrinthe laby, indiquant que
   *         la cellule d'entrée est la cellule courante
   *         et toutes les autres sont non visitées.
   */
  def etatInitial(laby: Labyrinthe): EtatLabyrinthe = {
    val hauteurMax = laby.hauteur - 1
    (laby.hauteur, laby.largeur) match {
      case (x, 0) =>
        if (hauteurMax == x) {
          case (hauteurMax, 0) => Courante
          case (_, _)          => NonVisitee
        } else { case (_, _) => NonVisitee }
      case (_, _) => { case (_, _) => NonVisitee }
    }
  }

  /**
   * @param laby un labyrinthe
   * @param marqueurE le marqueur à placer en entrée
   * @param marqueurS le marqueur à placer en sortie
   * @return une image du lab avec les marqueurs
   */
  def mettreMarqueurES(laby: Labyrinthe, p: Position): Option[Image] = {
    if (p == entreeLabyrinthe(laby)) Some(marqueurEntree)
    else if (p == sortieLabyrinthe(laby)) Some(marqueurSortie)
    else None
  }

  /**
   * @param laby Un labyrinthe
   * @param etat Une fonction décrivant le statut de chaque cellule
   * @return Une image représentant le labyrinthe.
   *
   * @note utiliser la fonction celluleToImage et les images
   *       marqueurEntree et marqueurSortie définies dans cellule.scala
   *
   *       Indication de longueur : une quinzaine de lignes au total,
   *       décomposées en fonctions ou expressions auxilaires courtes.
   *
   */
  def labToImage(laby: Labyrinthe, etat: EtatLabyrinthe): Image = {
    (laby.hauteur, laby.largeur) match {
      case (h, l) =>
        if (h == 0 || l == 0) {
          Empty
        } else {
          (0 to h - 1).foldRight(Empty: Image)(
            (y, j) => Below((0 to l - 1).foldRight(Empty: Image)(
              (x, i) => Beside(celluleToImage(laby.f(y, x), mettreMarqueurES(laby, (y, x)), etat(y, x)), i)), j))
        }
    }
  }
}