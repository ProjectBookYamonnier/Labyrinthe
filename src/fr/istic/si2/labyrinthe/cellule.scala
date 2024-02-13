package fr.istic.si2.labyrinthe
import fr.istic.si2.scribble._
object CelluleObject {

  /* Un passage ouvert indique une absence de mur.
   * Un passage fermé indique la présence d'un mur.
   */
  sealed trait Passage
  case object Ferme extends Passage
  case object Ouvert extends Passage

  /* Chaque case d'un labyrinthe est décrite par une cellule.
   * Une cellule indique la présence ou non de murs
   * « en haut » ou « à droite » de la case.
   *
   * Il y a donc quatre cellules possibles :
   *
   * Cellule(Ferme, Ferme)
   * Cellule(Ferme, Ouvert)
   * Cellule(Ouvert, Ferme)
   * Cellule(Ouvert, Ouvert)
   *
   */
  case class Cellule(haut: Passage, droit: Passage)

  /* Durant le déroulement du jeu consistant à se déplacer
   * dans le labyrinthe, on associe à chaque cellule un état
   * indiquant si cette cellule est la cellule courante (là où
   * le joueur se trouve) ou si elle a été visitée ou non.
   *
   */
  sealed trait EtatCellule
  case object Courante extends EtatCellule
  case object NonVisitee extends EtatCellule
  case object Visitee extends EtatCellule

  /* Représentation graphique d'une cellule
   *
   * Cela vous sera utile pour définir le fonction labToImage
   * dans labyrinthe.scala
   *
   * */

  val taille_cellule = 25 // taille du côté d'une cellule
  val epaisseur_mur = 1 // epaisseur des murs

  val mur_vertical = FillColor(Rectangle(epaisseur_mur + 1, taille_cellule), BLACK) // mur vertical
  val mur_horizontal = FillColor(Rectangle(taille_cellule, epaisseur_mur), BLACK) // mur horizontal

  /**
   * @param etat l'état d'une cellule
   * @return une image représentant le fond d'une cellule dans l'état etat
   *         (sans les murs)
   */
  def fondCellule(etat: EtatCellule): Image = {
    etat match {
      case NonVisitee => LineColor(FillColor(Rectangle(taille_cellule, taille_cellule), WHITE), TRANSPARENT)
      case Courante   => LineColor(On(FillColor(LineColor(Rectangle(taille_cellule - 3, taille_cellule - 3), TRANSPARENT), BLUE), FillColor(Rectangle(taille_cellule, taille_cellule), TRANSPARENT)), TRANSPARENT)
      case Visitee    => LineColor(FillColor(On(FillColor(Circle(taille_cellule / 3 - 2), BLUE), Rectangle(taille_cellule, taille_cellule)), Color(187, 187, 187, 80)), TRANSPARENT)
    }
  }

  /**
   *  @note Un margeur est une image à superposer sur celle d'une cellule,
   *  par exemple pour indiquer l'entrée et la sortie.
   *
   *  @param couleur une couleur
   *  @return un disque de la couleur indiquée, pouvant servir de marqueur
   *          à superposer sur des cellules de labyrinthes.
   */
  def marqueur(couleur: Color): Image = FillColor(LineColor(On(Circle(taille_cellule / 2 - 3), LineColor(FillColor(Rectangle(taille_cellule, taille_cellule), TRANSPARENT), TRANSPARENT)), couleur), couleur)

  /**
   * Une image de marqueur pour l'entrée d'un labyrinthe
   */
  val marqueurEntree: Image = marqueur(RED)

  /**
   * Une image de marqueur pour la sortie d'un labyrinthe
   */
  val marqueurSortie: Image = marqueur(GREEN)

  /**
   * @param c une [[Cellule]]
   * @param etat l'état de la cellule
   * @param marqueur un marqueur éventuel à surimposer sur l'image de la cellule,
   *        par exemple pour marquer l'entrée ou la sortie d'un labyrinthe
   * @return une image représentant la cellule c dans l'état etat,
   *         avec des murs là les passages de c sont fermés,
   *         et éventuellement le marqueur donné.
   *
   */
  def celluleToImage(c: Cellule, marqueur: Option[Image], etat: EtatCellule): Image = {
    val fond = fondCellule(etat)
    val img_cell = c match {
      case Cellule(Ferme, Ferme)   => OnAlign(Top, mur_horizontal, OnAlign(Right, mur_vertical, fond))
      case Cellule(Ferme, Ouvert)  => OnAlign(Top, mur_horizontal, fond)
      case Cellule(Ouvert, Ferme)  => OnAlign(Right, mur_vertical, fond)
      case Cellule(Ouvert, Ouvert) => fond
    }
    marqueur match {
      case Some(img_marqueur) => On(img_marqueur, img_cell)
      case None               => img_cell
    }
  }

}