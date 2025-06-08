package tip.analysis

import tip.cfg._
import tip.ast.AstNodeData.DeclarationData
import tip.lattices.IntervalLattice._
import tip.lattices._
import tip.solvers._

trait VariableSizeAnalysisWidening extends ValueAnalysisMisc with Dependencies[CfgNode] {

  val cfg: ProgramCfg

  val valuelattice: IntervalLattice.type

  val liftedstatelattice: LiftLattice[statelattice.type]

  /**
    * Int values occurring in the program, plus -infinity and +infinity.
    */
  private val B: Set[Num] = (() => {
    var result = Set[Num](MInf, 0, PInf)
    for (i <- 1 to 31) {
      result += (-math.pow(2, i)).intValue
      result += (math.pow(2, i).longValue() - 1).intValue
    }
    result
  })()

  def loophead(n: CfgNode): Boolean = indep(n).exists(cfg.rank(_) > cfg.rank(n))

  private def minB(b: IntervalLattice.Num) = B.filter(b <= _).min

  private def maxB(a: IntervalLattice.Num) = B.filter(_ <= a).max

  def widenInterval(x: valuelattice.Element, y: valuelattice.Element): valuelattice.Element =
    (x, y) match {
      case (IntervalLattice.EmptyInterval, _) => y
      case (_, IntervalLattice.EmptyInterval) => x
      case ((l1, h1), (l2, h2)) =>
        (if (l1 > l2) maxB(l2) else l1, if (h2 > h1) minB(h2) else h1)
    }

  def widen(x: liftedstatelattice.Element, y: liftedstatelattice.Element): liftedstatelattice.Element =
    (x, y) match {
      case (liftedstatelattice.Bottom, _) => y
      case (_, liftedstatelattice.Bottom) => x
      case (liftedstatelattice.Lift(xm), liftedstatelattice.Lift(ym)) =>
        liftedstatelattice.Lift(declaredVars.map { v =>
          v -> widenInterval(xm(v), ym(v))
        }.toMap)
    }
}

object VariableSizeAnalysis {

  object Intraprocedural {

    /**
      * Interval analysis, using the worklist solver with init and widening.
      */
    class WorklistSolverWithWidening(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
        extends IntraprocValueAnalysisWorklistSolverWithReachability(cfg, IntervalLattice)
        with WorklistFixpointSolverWithReachabilityAndWidening[CfgNode]
        with IntervalAnalysisWidening

    /**
      * Interval analysis, using the worklist solver with init, widening, and narrowing.
      */
    class WorklistSolverWithWideningAndNarrowing(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
        extends IntraprocValueAnalysisWorklistSolverWithReachability(cfg, IntervalLattice)
        with WorklistFixpointSolverWithReachabilityAndWideningAndNarrowing[CfgNode]
        with IntervalAnalysisWidening {

      val narrowingSteps = 5
    }
  }
}
