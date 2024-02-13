package fr.istic.si2.labyrinthe

import fr.istic.si2.scribble._
import LabyrintheObject._
import CelluleObject._
import Jeu._

class GameUniverse(laby: Labyrinthe) extends Universe[Chemin] {
  val hauteur = laby.hauteur
  val largeur = laby.largeur
  val f = laby.f

  override val HEIGHT: Int = taille_cellule * hauteur
  override val WIDTH: Int = taille_cellule * largeur
  override def init: Chemin = cheminInitial(laby)
  override val name: String = "Labyrinthe"

  override def react(chemin: Chemin, evenement: Event): Chemin =
    evenement match {
      case KeyPressed(KeyUp)         => bouge(laby, chemin, Haut)
      case KeyPressed(KeyRight)      => bouge(laby, chemin, Droite)
      case KeyPressed(KeyDown)       => bouge(laby, chemin, Bas)
      case KeyPressed(KeyLeft)       => bouge(laby, chemin, Gauche)
      case KeyPressed(KeyAscii('S')) => resoudre(laby, chemin)
      case KeyPressed(KeyAscii('I')) => indice(laby, chemin)
      case KeyPressed(KeyAscii('#')) => init
      case KeyPressed(KeyAscii('A')) => annulerBouger(chemin)
      case _                         => chemin
    }

  override def stopWhen(c: Chemin): Boolean = false // Pour arrêter : utiliser ESC
  override def toImage(c: Chemin): Image = labToImage(laby, etatLabyrinthe(laby, c))

}