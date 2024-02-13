package fr.istic.si2.labyrinthe

import fr.istic.si2.scribble._

import LabyrintheObject._
import CelluleObject._
import Jeu._
import Utils._

object app1 extends App {

  bigbang(new GameUniverse(labyrintheFourni))

}