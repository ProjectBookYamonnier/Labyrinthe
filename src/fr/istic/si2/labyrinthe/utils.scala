package fr.istic.si2.labyrinthe

import LabyrintheObject._
import CelluleObject._

object Utils {
  /**
   * Un labyrinthe à utiliser
   */
  def labyrintheFourni: Labyrinthe = labyX2

  /* La suite de ce fichier est inutile pour le projet
   * et nous vous déconseillons de vous y attarder.
   */

  /**
   * @param n un nombre
   * @param k une nombre positif
   * @return le nombre n élevé à la puissance k
   */
  def power(n: BigInt, k: Int): BigInt = n.pow(math.max(k, 0))

  /**
   * @param n un entier positif
   * @param k un entier positif
   * @return le k-ième chiffre de n exprimé en base 4
   */
  def digitBase4(n: BigInt, k: Int): Int = (n / power(4, k)).mod(4).toInt

  /**
   * Enumération d'agencements de dimensions données
   *
   *  @param h une hauteur de labyrinthe
   *  @param l une largeur de labyrinthe
   *  @param n un nombre
   *  @return un agencement de labyrinthe de dimensions h x l
   *
   *  @note l'énumération est redondante mais
   *  (0 to lastCode(h,l)).filter(estStandard(h,l)) fournit une
   *  séquence sans doublons de tous agencements possibles pour les
   *  dimensions spécifiées.
   *
   *  Seuls les labyrinthes de très petites dimensions peuvent être
   *  énumérés de la sorte. Cette fonction est utile pour générer
   *  un labyrinthe dont on a par ailleurs calculé le code.
   *
   *  (C'est pour s'amuser, il y a des moyens bien plus efficaces de
   *  sérialiser des labyrinthes.)
   *
   */
  def decode(h: Int, l: Int)(n: BigInt): Agencement = {

    implicit def int2passage(x: Int): Passage =
      if (x == 0) Ferme else Ouvert
    def c(k: Int) = Cellule(digitBase4(n, k) / 2, digitBase4(n, k) % 2)
    p => p match { case (i, j) => c(i * l + j) }
  }

  // Quelques labyrinthes fournis (« difficiles»)
  def labyX1: Labyrinthe = Labyrinthe(25, 25, decode(25, 25)(BigInt("11483345119028952708926055713037067048753454828042227773098515969819933049633813425274618815720701494241000591457163855569301614963413822604058485365141184219786889783506568990929494879628483268715400560828976342838481955927181017239372336870693530931835179259715381591095234234761246441103943916023626588216479188768041956773416998977596346316232952119148323674967153687478613")))
  def labyX2: Labyrinthe = Labyrinthe(20, 40, decode(20, 40)(BigInt("31880643381570766392329605813986318266016701998965222925217548566330564171392439682330673704860514053848772220078482897396555633571039728684399603923266046080558294106964190852042897802602750535297944549746422063649319268916816759210597048081573590632197592835241937175886799419031637162828475762677453687548150389474452002833263565200731217703628707557344187984078999807472446025108122983291963724815447875313943757607210220899312948886801226491953803784134176420151816940899357781")))
  def labyX3: Labyrinthe = Labyrinthe(30, 60, decode(30, 60)(BigInt("3777820914118268580039209213923244932448880499448823112144244308797486045377927335935730021312330041985362070750665679637745671157812758968946795046386383425175793708382873620140144482499752844422170230241921609177471931190566550958103644128297399918686141586283552260770916916335783212276955896912846446319317876137694462786342782455278285416530144581827297904974438060401448652821956992899953142569566563672858148591416703780128488008925039084283017882848390602081959879510371112159158832562908335534674626517789678577762018864592671347537197972790441102403717611950803594820026164287101107165502790717989789895997981451861972123850950933591793311501015613798358783589833422526177904518327901268730404501900877905466669197655769262572905429899040321597673536371830958543326762061150541235249543988488846726232255027182807987641100209316146826579008417034309412165006112264374106020047213740121430116228235423044944442269680254054696214636277934421371383558539955936819861608913146533481640353443671322442315889318919511279038225221873989366032563520142329634402579358396894828385541")))
  def labyX4: Labyrinthe = Labyrinthe(30, 60, decode(30, 60)(BigInt("3183488788520772507385375359425290300777833237138108579883850566751571512105331118839716511174286450489318609328137747095848116849824829867630366041468278251250802067331119404510956258272422347575167306562748211320466189774368399528762537753348472244649781366695872335144498272041609571480314203865421882952594828641242721893837699839425080914952891702311863589987216319359352511560318022846836325420127837078298048660474786125220069464017021522845316057484120832136679164571207855898073026771561582751694427320652741617603092909421015114128786687973657236802466345048835269334983380170003404044124899245845340043910991309265484724392786926902920688696109838785001951639142415603873615932758221465719045618443762677458797474279291822955142920056223446528665089541441646142179806393485603651268624078380296472824065992512197191200720340574991660249733117002691687386321008271294186575166361195595716831572079193503075941761354751821740703067311765207422965346435516454088027350903083221448666802568068492111816696522036364800850223377001123681973036055767530573299316590187614884353365")))

  /**
   * @param laby un labyrinthe
   * @return une chaîne de caractère formant le dessin
   *         du labyrinthe laby, grâce aux caractères
   *         Unicode de la plage "Box Drawing"
   *
   * @example show(lab(4, 5, agencementBD)) =
   *  ┏━┳━┳━┳━┳━┓
   *  ┣━╋━╋━╋━┫ ┃
   *  ┣━╋━╋━╋━┫ ┃
   *  ┣━┻━┻━┻━┛ ┃
   *  ┗━━━━━━━━━┛
   *
   *  @note cf. https://www.w3.org/TR/xml-entity-names/025.html
   *        Le labyrinthe suivant utilise les 16 caractères utiles
   *        pour représenter toutes les configurations possibles :
   *        ┏━┳━┳━┳━┓
   *        ┣━╋━┫ ╹ ┃
   *        ┣━┻━┛ ╺━┫
   *        ┣━╸   ╻ ┃
   *        ┗━━━━━┻━┛
   *        Les 16 caractères utiles :
   *
   *        ╋ ┣ ┻ ┗ ┫ ┃ ┛ ╹ ┳ ┏ ━ ╺ ┓ ╻ ╸ [ESPACE]
   *
   */
  def show(laby: Labyrinthe): String = {
    implicit def booleanToPassage(b: Boolean): Passage = if (b) Ouvert else Ferme
    implicit def passageToBoolean(p: Passage): Boolean = p match {
      case Ouvert => true
      case Ferme  => false
    }

    /**
     * @param laby un labyrinthe
     * @return l'agencement du labyrinthe laby, étendu à toutes les positions du plan,
     *         de sorte que les murs entourant le labyrinthe soient présents et qu'il n'y ait
     *         aucun mur au-delà du labyrinthe.
     *
     */
    def agencementExt(laby: Labyrinthe): Agencement = laby match {
      case Labyrinthe(h, l, f) => {
        case (i, j) => f(i, j) match {
          case Cellule(haut, droit) => Cellule(
            j < 0 || j >= l || i > 0 && i < h && haut,
            i < 0 || i >= h || j >= 0 && j < l - 1 && droit)
        }
      }
    }

    laby match {
      case Labyrinthe(h, l, f) =>

        implicit def boolToInt(b: Boolean): Int = if (b) 1 else 0

        val intersections: Vector[String] =
          Vector("━╋", " ┣", "━┻", " ┗", "━┫", " ┃", "━┛", " ╹",
            "━┳", " ┏", "━━", " ╺", "━┓", " ╻", "━╸", "  ")

        def intersectionNE(ouest: Boolean, sud: Boolean, est: Boolean, nord: Boolean) =
          intersections(8 * nord + 4 * est + 2 * sud + ouest)

        val ext_f: Agencement = agencementExt(laby)

        def neso(i: Int, j: Int): String =
          intersectionNE(
            ext_f((i, j)).haut,
            ext_f((i, j)).droit,
            ext_f((i, j + 1)).haut,
            ext_f((i - 1, j)).droit)

        def ligne(i: Int): String =
          (-1 to l - 1).foldRight("")((j, s) => neso(i, j) ++ s)

        (0 to h).foldRight("")((i, s) => ligne(i) + "\n" + s)

    }
  }

}