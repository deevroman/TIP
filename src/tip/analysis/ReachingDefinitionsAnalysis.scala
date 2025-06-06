package tip.analysis

import tip.ast.AstNodeData.DeclarationData
import tip.ast._
import tip.cfg._
import tip.lattices._
import tip.solvers._

abstract class ReachingDefinitionsAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(false) {

  val lattice: MapLattice[CfgNode, PowersetLattice[AAssignStmt]] = new MapLattice(new PowersetLattice())

  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case r: CfgStmtNode =>
        r.data match {
          case _: AVarStmt => s
          case as: AAssignStmt =>
            as.left match {
              case id: AIdentifier =>
                lattice.sublattice.lub(s.filter {
                  case AAssignStmt(id2: AIdentifier, _, _) => id2.name != id.name
                  case _ => true
                }, Set(as))
              case _ => s
            }
          case _ => s
        }
      case _: CfgFunExitNode => lattice.sublattice.bottom
      case _ => s
    }
}

class ReachingDefAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefinitionsAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with BackwardDependencies
