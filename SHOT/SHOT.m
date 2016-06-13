(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: SHOT *)
(* :Context: SHOT` *)
(* :Author: Andreas Lundell *)
(* :Date: 2016-06-01 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 Andreas Lundell *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["SHOT`"]
(* Exported symbols added here with SymbolName::usage *)

ClearAll["SHOT`"];

SolveMINLP::usage = "Solver[objectiveFunction, linearConstraints_List, nonlinearConstraints_List, integerConstraints_List,variables_List, objectiveDirection]. The parameter objectiveDirection is min or max. Can also be called as Solver[{objectiveFunction, constraints_List}, variables_List, objectiveDirection], in which case all constraints are given in the same format as e.g. in NMinimize.";

PrintDebug::usage = "PrintDebug[SolveMINLP[...]] can be used to write debug information to the current notebook.";

GetStats::usage = "GetStats[\"Type\"] returns the solution statistics for the provided type.";

Begin["`Private`"]

SolveMINLP::badargs = "The option for `1` is not valid. Setting to default value `2`";
RewriteProblemFormat::parsererrorconstr = "Cannot parse constraint `1`. Ignoring it.";
RewriteProblemFormat::parsererrorobj = "Cannot parse objective function `1`. Aborting.";
SolveMINLP::objnonlinear = "Objective function is nonlinear: `1`. Aborting.";
SolveMINLP::infeasibleMILPsol = "Relaxed MILP solution found. Try increasing parameter MaximalIterationsMILP.";

(*GetSolutionStatistics::usage ="Returns additional solution about the last solver run."*)
(*GetSolutionStatistics["city"] := obj[a][[1]]*)

StatsInteriorPoint = {};
StatsSolutionPoints = {};
StatsObjectiveValues = {};
StatsPrimalObjectiveValues = {};
StatsPrimalSolutionPoints = {};
StatsAbsoluteGaps = {};
StatsRelativeGaps = {};
MaxNonlinearConstraintIndexes = {};
MaxNonlinearConstraintValues = {};
NonlinearConstraintValues = {};
StatsHyperplanes = {};
StatsHyperplanePoints = {};
StatsDualBoundCuts = {};
StatsPrimalBoundCuts = {};

IterationNumber = 0;

Options[SolveMINLP] = {
  ConstraintToleranceLP -> 0.01,
  ConstraintToleranceMILP -> 0.0001,
  AbsoluteGapTolerance -> 0.0001,
  RelativeGapTolerance -> 0.0001,
  MinimaxAccuracyGoal -> 6,
  MinimaxPrecisionGoal -> 6,
  MaximalIterationsLP -> 20,
  MaximalIterationsMILP -> 40,
  Strategy -> "ESH",
  PrimalBoundNLPIterationGap -> 5,
  RootMethod -> "Bisection",
  InteriorPointStrategy -> "Minimax",
  UsePrimalBoundCut -> True,
  ShowProblemSummary -> False,
  ConstraintToleranceConstant -> 10^(-9),
  PositiveNonlinearObjectiveBound -> 10^5,
  NegativeNonlinearObjectiveBound -> -10^5
};

Options[RewriteProblemFormat] = {
  PositiveNonlinearObjectiveBound -> 10^5,
  NegativeNonlinearObjectiveBound -> -10^5
};

Options[SolveSubproblem] = {
  ConstraintTolerance -> 0.0001,
  MaximalIterations -> 100,
  Strategy -> "ESH",
  RootMethod -> "Bisection",
  PrimalBoundNLPIterationGap -> 10,
  AbsoluteGapTolerance -> 0.0001,
  RelativeGapTolerance -> 0.0001,
  UsePrimalBoundCut -> True,
  ConstraintToleranceConstant -> 0.00000001 (* A small constant added to the LHS of the hyperplane to avoid numerical difficulties in linear solver *)
};

GetStats[key_String]:=Module[{},

  Switch[key,
    "InteriorPoints",Return[StatsInteriorPoint],
    "ObjectiveValues",Return[StatsObjectiveValues],
    "SolutionPoints",Return[StatsSolutionPoints],
    "PrimalObjectiveValues",Return[StatsPrimalObjectiveValues],
    "PrimalSolutionPoints",Return[StatsPrimalSolutionPoints],
    "AbsoluteGaps",Return[StatsAbsoluteGaps],
    "RelativeGaps",Return[StatsRelativeGaps],
    "MaxNonlinearConstraintIndexes",Return[MaxNonlinearConstraintIndexes],
    "MaxNonlinearConstraintValues",Return[MaxNonlinearConstraintValues],
    "NonlinearConstraintValues",Return[NonlinearConstraintValues],
    "Hyperplanes",Return[StatsHyperplanes],
    "HyperplanePoints",Return[StatsHyperplanePoints],
    "DualBoundCuts",Return[StatsDualBoundCuts],
    "PrimalBoundCuts",Return[StatsPrimalBoundCuts]
    ];
];

SolveMINLP[{obj_, constrs_List}, vars_List, objectiveDirection_, options___] :=
    Module[{},

      {objective, linearConstraints, nonlinearConstraints, integerConstraints, variables} =
          RewriteProblemFormat[
            {obj, constrs},
            vars,
            NegativeNonlinearObjectiveBound -> OptionValue[NegativeNonlinearObjectiveBound],
            PositiveNonlinearObjectiveBound -> OptionValue[PositiveNonlinearObjectiveBound]
          ];

      SolveMINLP[
        objective,
        linearConstraints,
        nonlinearConstraints,
        integerConstraints,
        variables,
        objectiveDirection,
        options]
    ];

SolveMINLP[objectiveFunction_, linearConstraints_List, nonlinearConstraints_List, integerConstraints_List, variables_List, objectiveDirection_, OptionsPattern[]] :=
    Module[{timeTotal, timeNLP, timeLP, timeMILP, numVars, integerVariables, linearConstraintsIntersection, integerConstraintsIntersection, nonlinearConstraintsIntersection, nonlinearFunctions, solution, interiorPoint, hyperplanesGenerated, iterationSolution, primalSolution, isOptimal, absGap, relGap, minmaxSolution, objectiveFunctionUse,stats},

      timeTotal = AbsoluteTiming[

        timeLP = {0};timeMILP = {0};timeRootSearch = {0};timePrimalBoundSearch = {0};

        PrintToConsole["=======| SHOT (Supporting Hyperplane Optimization Toolkit) for Mathematica |========"];
        PrintToConsole["Implementation by: Andreas Lundell, \[CapitalARing]bo Akademi University, Turku, Finland."];
        PrintToConsole["Please cite: J. Kronqvist, A. Lundell and T. Westerlund, The extended supporting hyperplane algorithm for convex mixed-integer nonlinear programming Journal of Global Optimization, vol 64(2), pp 249-272,2016 (http://dx.doi.org/10.1007/s10898-015-0322-3)"];
        PrintToConsole["------------------------------------------------------------------------------------"];

        IterationNumber = 0;

        If[Not[MemberQ[{True, False}, OptionValue[ShowProblemSummary]]],
          SetOptions[SolveMINLP, ShowProblemSummary -> False];
          Message[SolveMINLP::badargs, "ShowProblemSummary", False]];

        If[OptionValue[ConstraintToleranceLP] <= 0,
          Message[SolveMINLP::badargs, "ConstraintToleranceLP", 0.5];
          SetOptions[SolveMINLP, ConstraintToleranceLP -> 0.5]];

        If[OptionValue[ConstraintToleranceMILP] <= 0,
          Message[SolveMINLP::badargs, "ConstraintToleranceMILP", 0.0001];
          SetOptions[SolveMINLP, ConstraintToleranceMILP -> 0.0001]];

        If[OptionValue[AbsoluteGapTolerance] <= 0,
          Message[SolveMINLP::badargs, "AbsoluteGapTolerance", 0.0001];
          SetOptions[SolveMINLP, AbsoluteGapTolerance -> 0.0001]];

        If[OptionValue[RelativeGapTolerance] <= 0,
          Message[SolveMINLP::badargs, "RelativeGapTolerance", 0.0001];
          SetOptions[SolveMINLP, RelativeGapTolerance -> 0.0001]];

        If[OptionValue[MinimaxAccuracyGoal] < 0,
          Message[SolveMINLP::badargs, "MinimaxAccuracyGoal", 6];
          SetOptions[SolveMINLP, MinimaxAccuracyGoal -> 6]];

        If[OptionValue[MinimaxPrecisionGoal] < 0,
          Message[SolveMINLP::badargs, "MinimaxPrecisionGoal", 6];
          SetOptions[SolveMINLP, MinimaxPrecisionGoal -> 6]];

        If[OptionValue[MaximalIterationsLP] < 0,
          Message[SolveMINLP::badargs, "MaximalIterationsLP", 20];
          SetOptions[SolveMINLP, MaximalIterationsLP -> 20]];

        If[OptionValue[MaximalIterationsMILP] < 0,
          Message[SolveMINLP::badargs, "MaximalIterationsMILP", 100];
          SetOptions[SolveMINLP, MaximalIterationsMILP -> 100]];

        If[Not[MemberQ[{"ESH", "ECP"}, OptionValue[Strategy]]],
          Message[SolveMINLP::badargs, "Strategy", "ESH"];
          SetOptions[SolveMINLP, Strategy -> "ESH"]];

        If[OptionValue[PrimalBoundNLPIterationGap] < -1,
          Message[SolveMINLP::badargs, "PrimalBoundNLPIterationGap", 5];
          SetOptions[SolveMINLP, PrimalBoundNLPIterationGap -> 5]];

        If[Not[MemberQ[{"FindRoot", "Bisection"}, OptionValue[RootMethod]]],
          Message[SolveMINLP::badargs, "RootMethod", "Bisection"];
          SetOptions[SolveMINLP, RootMethod -> "Bisection"]];

        If[Not[MemberQ[{"CuttingPlane", "Minimax"}, OptionValue[InteriorPointStrategy]]],
          Message[SolveMINLP::badargs, "InteriorPointStrategy", "Minimax"];
          SetOptions[SolveMINLP, InteriorPointStrategy -> "Minimax"]];

        If[Not[MemberQ[{True, False}, OptionValue[UsePrimalBoundCut]]],
          SetOptions[SolveMINLP, UsePrimalBoundCut -> True];
          Message[SolveMINLP::badargs, "UsePrimalBoundCut", True]];

        If[OptionValue[ConstraintToleranceConstant] <= 0,
          Message[SolveMINLP::badargs, "ConstraintToleranceConstant", 0.0000001];
          SetOptions[SolveMINLP, ConstraintToleranceConstant -> 0.0000001]];

        (* Checks if the objective function is linear *)
        If[!CheckLinear[objectiveFunction, vars],
          Message[SolveMINLP::objnonlinear, obj];
          Return[{}];
        ];

        numVars = Length[variables];

        (* Rewrite a maximization problem as a minimization one *)
        If[objectiveDirection == "max",
          objectiveFunctionUse = -objectiveFunction,
          objectiveFunctionUse = objectiveFunction;
        ];

        (* Reformulates some of the constraints *)
        linearConstraintsIntersection = And @@ linearConstraints // N;
        integerVariables = Cases[integerConstraints, Element[x_, Integers] -> x];
        nonlinearFunctions = Cases[nonlinearConstraints, LessEqual[f_, rhs_] -> (f - rhs)];

        If[OptionValue[ShowProblemSummary],
          PrintToConsole["Objective function: min.", objectiveFunction];
          PrintToConsole["Linear constraints:     ", TableForm[linearConstraints]];
          PrintToConsole["Nonlinear constraints:  ", TableForm[nonlinearConstraints]];
          PrintToConsole["Number of variables:    ", numVars];
          PrintToConsole["Number of integer vars: ", TableForm[integerConstraints]];
          PrintToConsole["------------------------------------------------------------------------------------"];
        ];

        stats = Reap[
        (* Find an interior point *)
          If[OptionValue[Strategy] == "ESH",
            timeNLP = AbsoluteTiming[
              If[OptionValue[InteriorPointStrategy] == "Minimax",
                PrintToConsole[" - Obtaining interior point with minimax strategy -"];
                If[Length[nonlinearFunctions] == 1, AppendTo[nonlinearFunctions, -Infinity]];

                minmaxSolution = FindMinMax[{Max[nonlinearFunctions], linearConstraints}, variables, PrecisionGoal -> 1, AccuracyGoal -> 1];
                If[Length[minmaxSolution] < 1 || Not[NumericQ[minmaxSolution[[1]]]], Abort[];];
                interiorPoint = variables /. minmaxSolution[[2]];
                ,
                PrintToConsole[" - Obtaining interior point with cutting plane strategy -"];
                minmaxSolution = SolveNLPProblem[linearConstraintsIntersection, Max[nonlinearFunctions], nonlinearFunctions, variables, PositiveNonlinearObjectiveBound -> OptionValue[PositiveNonlinearObjectiveBound], NegativeNonlinearObjectiveBound -> OptionValue[NegativeNonlinearObjectiveBound]];
                If[Length[minmaxSolution] < 1 || Not[NumericQ[minmaxSolution[[1]]]], Abort[];];
                interiorPoint = minmaxSolution[[2]];
              ];


              Sow[interiorPoint, "InteriorPoint"];
              PrintToConsole[ "   Minimax objective value: ", minmaxSolution[[1]]];
            ];
          ];
          hyperplanesGenerated = True;

        (*Preprocessing LP step*)
        If[OptionValue[MaximalIterationsLP] > 0,
          timeLP = AbsoluteTiming[
            PrintToConsole[" - LP step started -"];

            {hyperplanesGenerated, iterationSolution, primalSolution, isOptimal, absGap, relGap} =
                SolveSubproblem[
                  objectiveFunctionUse,
                  linearConstraintsIntersection,
                  nonlinearFunctions,
                  interiorPoint,
                  variables,
                  {},
                  objectiveDirection,
                  MaximalIterations -> OptionValue[MaximalIterationsLP],
                  ConstraintTolerance -> OptionValue[ConstraintToleranceLP],
                  Strategy -> OptionValue[Strategy],
                  RootMethod -> OptionValue[RootMethod],
                  ConstraintToleranceConstant -> OptionValue[ConstraintToleranceConstant],
                  UsePrimalBoundCut -> OptionValue[UsePrimalBoundCut]
                ];
          ];
        ];

        (*MILP step*)
        If[OptionValue[MaximalIterationsMILP] > 0,
          timeMILP = AbsoluteTiming[
            PrintToConsole[" - MILP step started -"];
            integerConstraintsIntersection = Apply[And, integerConstraints];

            {hyperplanesGenerated, iterationSolution, primalSolution, isOptimal, absGap, relGap} =
                SolveSubproblem[
                  objectiveFunctionUse,
                  linearConstraintsIntersection && hyperplanesGenerated && integerConstraintsIntersection,
                  nonlinearFunctions,
                  interiorPoint,
                  variables,
                  integerVariables,
                  objectiveDirection,
                  MaximalIterations -> OptionValue[MaximalIterationsMILP],
                  ConstraintTolerance -> OptionValue[ConstraintToleranceMILP],
                  Strategy -> OptionValue[Strategy],
                  RootMethod -> OptionValue[RootMethod],
                  PrimalBoundNLPIterationGap -> OptionValue[PrimalBoundNLPIterationGap],
                  RelativeGapTolerance -> OptionValue[RelativeGapTolerance],
                  AbsoluteGapTolerance -> OptionValue[AbsoluteGapTolerance],
                  ConstraintToleranceConstant -> OptionValue[ConstraintToleranceConstant],
                  UsePrimalBoundCut -> OptionValue[UsePrimalBoundCut]
                ];

            If[integerConstraints != {} && Not[isOptimal && (integerConstraints /. iterationSolution[[2]])[[1]]],
              Message[SolveMINLP::infeasibleMILPsol];(*Relaxed solution found*)
            ];
          ];
        ];,{"InteriorPoint","ObjectiveValue", "SolutionPoint","PrimalObjectiveValue","PrimalSolutionPoint","AbsoluteGap","RelativeGap","MaxNonlinearConstraintIndex","MaxNonlinearConstraintValue","NonlinearConstraintValues","Hyperplane","HyperplanePoint","DualBoundCut","PrimalBoundCut"}];
      ];

      StatsInteriorPoint = stats[[2,1,1]];
      StatsObjectiveValues = stats[[2,2,1]];
      StatsSolutionPoints = stats[[2,3,1]];
      StatsPrimalObjectiveValues = stats[[2,4,1]];
      StatsPrimalSolutionPoints = stats[[2,5,1]];
      StatsAbsoluteGaps = stats[[2,6,1]];
      StatsRelativeGaps = stats[[2,7,1]];
      MaxNonlinearConstraintIndexes = stats[[2,8,1]];
      MaxNonlinearConstraintValues = stats[[2,9,1]];
      NonlinearConstraintValues = stats[[2,10,1]];
      StatsHyperplanes = stats[[2,11,1]];
      StatsHyperplanePoints = stats[[2,12,1]];
      StatsDualBoundCuts = stats[[2,13,1]];
      StatsPrimalBoundCuts = stats[[2,14,1]];

      PrintToConsole["------------------------------------------------------------------------------------"];
      If[primalSolution[[1]] != Infinity,
        PrintToConsole["Primal solution:    ", primalSolution[[1]]];
        PrintToConsole["Abs. primal gap:    ", absGap];
        PrintToConsole["Rel. primal gap:    ", relGap];
      ];
      If[OptionValue[Strategy] == "ESH", PrintToConsole["Time NLP:           ", timeNLP[[1]]];];
      If[OptionValue[Strategy] == "ESH", PrintToConsole["Time root search:   ", timeRootSearch[[1]]]];;
      PrintToConsole["Time LP:            ", timeLP[[1]] ];
      PrintToConsole["Time MILP:          ", timeMILP[[1]]];
      PrintToConsole["Time primal search: ", timePrimalBoundSearch[[1]]];
      If[OptionValue[Strategy] != "ECP",
        PrintToConsole["Time without NLP    ", timeTotal[[1]] - timeNLP[[1]]];
      ];
      PrintToConsole["Time total:         ", timeTotal[[1]]];
      PrintToConsole["------------------------------------------------------------------------------------"];
      If[primalSolution[[1]] != Infinity, primalSolution, iterationSolution]

    ];


RewriteProblemFormat[{obj_, constrs_List}, vars_List, OptionsPattern[]] :=
    Module[{NN, X, LU, L, C, Cconstr, Y, newConstr, newObj, newVars},

    (*Basic initialization*)
      NN = Length[vars];
      X = {};
      LU = True;L = {};
      C = {};Cconstr = {};
      Y = {};
      newVars = vars;

      (*Rewriting nonlinear objective function*)
      If[!CheckLinear[obj, vars],
        debugPrint["Nonlinear objective function detected, rewriting it as an constraint."];
        Clear[addobjvar];
        newObj = addobjvar;
        AppendTo[Cconstr, obj - addobjvar <= 0];
        AppendTo[newVars, addobjvar];
        AppendTo[L, OptionValue[NegativeNonlinearObjectiveBound] <= addobjvar <= OptionValue[PositiveNonlinearObjectiveBound]];
        ,
        newObj = obj;
      ];

      (*Moves all terms to LHS*)
      moveTermRule = (ineq : Less | Greater | LessEqual | GreaterEqual | Equal)[lhs_, rhs_] :> ineq[lhs - rhs, 0];

      newConstr = constrs /. moveTermRule;

      (*Parses constraints*)
      For[i = 1, i <= Length[newConstr], i++,

        Monitor[
          Which[
            MatchQ[newConstr[[i]], Element[_, Integers]], (*Constraint is an integer restriction*)
            AppendTo[Y, newConstr[[i]]];
            ,
            MatchQ[newConstr[[i]], Equal[_, 0] | LessEqual[_, 0] | GreaterEqual[_, 0] | Less[_, 0] | Greater[_, 0]],
            isLinear = True;
            clist = CoefficientRules[newConstr[[i]][[1]], vars][[;; All, 1]];
            For[j = 1, j <= Length[clist], j++,
              If[Count[clist[[j]], Except[0]] > 1, isLinear = False;Break[]]
            ];
            If[PolynomialQ[newConstr[[i]][[1]], vars] && Max[Exponent[newConstr[[i]][[1]], vars]] <= 1 && isLinear,
            (*Constraint is linear*)
              AppendTo[L, newConstr[[i]]];
              ,
            (*Constraint is nonlinear*)
              AppendTo[C, newConstr[[i, 1]]];
              AppendTo[Cconstr, newConstr[[i]]];
            ];
            ,
            MatchQ[newConstr[[i]], LessEqual[_Integer | _Real, _Symbol, _Integer | _Real]],
          (*Constraint is a variable bound*)
            AppendTo[L, newConstr[[i]]];
            ,

            True,
          (*There is some problem with the constraint*)
            Message[RewriteProblemFormat::parsererrorconstr, newConstr[[i]]];
          ];
          , i];
      ];

      {newObj, L, Cconstr, Y, newVars}
    ];


CheckLinear[funct_, vars_] :=
    Module[{},
      If[!(PolynomialQ[funct, vars] && Max[Exponent[funct, vars]] <= 1),
        Return[False];
      ];

      Return[True];
    ];


(*FindMinMax[{Max[{f1,f2,..}],constraints},vars]*)
SetAttributes[FindMinMax, HoldAll];
FindMinMax[{f_Max, cons_}, vars_, opts___?OptionQ] :=
    With[{res = iFindMinMax[{f, cons}, vars, opts]},
      res /; ListQ[res]];
iFindMinMax[{ff_Max, cons_}, vars_, opts___?OptionQ] :=
    Module[{z, res, f = List @@ ff},
      res = FindMinimum[{z(*(z+100)^2*), (And @@ cons) && (And @@
          Thread[z >= f]) && (z <= 0.1)},
        Append[Flatten[{vars}, 1], z], opts];
      If[ListQ[res], {z /. res[[2]],
        Thread[vars -> (vars /. res[[2]])]}]];


SolveSubproblem[objectiveFunction_, linearConstraintIntersection_, nonlinearConstraintFunctions_, interiorPoint_, variables_, integerVariables_, objectiveDirection_, OptionsPattern[]] :=
    Module[{nonlinearConstraintsIntersection, hyperplanesGenerated, primalboundCut, iterationSolution, iterationSolutionObjective, iterationSolutionPointReplaceList, iterationConstraintValues, iterationMaxConstraintIndex, iterationMaxConstraintValue, linesearchLambdaFunction, linesearchLambdaFunctionReplaceList,
      maxFunctionLinesearch, lambdaSolution, lambda, tmpfunct, hyperplaneGenerationPointReplaceList, generationPointConstraintValues, generationPointMaxConstraintIndexes, hyperplaneGenerationPoint, hyperPlane, integerVariableReplaceVals, primalSolution, absGap, relGap, primalSolutionCandidate, objCoeff,dualboundCut},

    (*Checking value of options*)
      If[OptionValue[ConstraintTolerance] <= 0,
        Message[SolveMINLP::badargs, "ConstraintTolerance", 0.0001];
        SetOptions[SolveSubproblem, ConstraintTolerance -> 0.0001]];

      If[OptionValue[AbsoluteGapTolerance] <= 0,
        Message[SolveMINLP::badargs, "AbsoluteGapTolerance", 0.0001];
        SetOptions[SolveSubproblem, AbsoluteGapTolerance -> 0.0001]];

      If[OptionValue[RelativeGapTolerance] <= 0,
        Message[SolveMINLP::badargs, "RelativeGapTolerance", 0.0001];
        SetOptions[SolveSubproblem, RelativeGapTolerance -> 0.0001]];

      If[OptionValue[MaximalIterations] < 0,
        Message[SolveMINLP::badargs, "MaximalIterations", 40];
        SetOptions[SolveSubproblem, MaximalIterations -> 40]];

      If[Not[MemberQ[{"ESH", "ECP"}, OptionValue[Strategy]]],
        Message[SolveMINLP::badargs, "Strategy", "ESH"];
        SetOptions[SolveSubproblem, Strategy -> "ESH"]];

      If[Not[MemberQ[{"FindRoot", "Bisection"}, OptionValue[RootMethod]]],
        Message[SolveMINLP::badargs, "RootMethod", "Bisection"];
        SetOptions[SolveSubproblem, RootMethod -> "Bisection"]];

      If[OptionValue[PrimalBoundNLPIterationGap] < -1,
        Message[SolveMINLP::badargs, "PrimalBoundNLPIterationGap", 5];
        SetOptions[SolveSubproblem, PrimalBoundNLPIterationGap -> 5]];

      If[Not[MemberQ[{True, False}, OptionValue[UsePrimalBoundCut]]],
        SetOptions[SolveMINLP, UsePrimalBoundCut -> True];
        Message[SolveMINLP::badargs, "UsePrimalBoundCut", True]];

      If[OptionValue[ConstraintToleranceConstant] < 0,
        Message[SolveMINLP::badargs, "ConstraintTolerance", 0.0000001];
        SetOptions[SolveSubproblem, ConstraintTolerance -> 0.0000001]];

      If[objectiveDirection == "max", objCoeff = -1, objCoeff = 1;];

      hyperplanesGenerated = True;
      primalboundCut = True;
      dualboundCut = True;
      nonlinearConstraintsIntersection = And @@ Thread[nonlinearConstraintFunctions <= Table[0, {i, Length[nonlinearConstraintFunctions]}]];

      primalSolution = { Infinity, {}};
      relGap = Infinity;
      absGap = Infinity;

      For[i = 1, i <= OptionValue[MaximalIterations], i++,

        IterationNumber++;

        (*Solving the (MI)LP problem*)

        iterationSolution = FindMinimum[{objectiveFunction, linearConstraintIntersection && hyperplanesGenerated &&primalboundCut&&dualboundCut}, variables, Method -> "LinearProgramming"(*,AccuracyGoal\[Rule]10,PrecisionGoal\[Rule]10,MaxIterations\[Rule]1000000*)];
        (*iterationSolution=NMinimize[{objectiveFunction,linearConstraintIntersection&&hyperplanesGenerated},variables];*)

        If[Length[iterationSolution] < 1 || Not[NumericQ[iterationSolution[[1]]]], Abort[];];
        iterationSolutionObjective = iterationSolution[[1]];
        Sow[{IterationNumber,iterationSolution[[2]]},"SolutionPoint"];
        Sow[{IterationNumber,iterationSolutionObjective},"ObjectiveValue"];

        dualboundCut = objectiveFunction >= iterationSolutionObjective;
        Sow[{IterationNumber,dualboundCut},"DualBoundCut"];

        iterationSolutionPointReplaceList = iterationSolution[[2]];

        If[primalSolution[[1]] != Infinity,
          absGap = Abs[primalSolution[[1]] - iterationSolutionObjective];
          relGap = Abs[primalSolution[[1]] - iterationSolutionObjective] / (10^-10 + Abs[primalSolution[[1]]]);
        ];

        iterationConstraintValues = nonlinearConstraintFunctions /. iterationSolutionPointReplaceList;

        Sow[{IterationNumber,iterationConstraintValues},"NonlinearConstraintValues"];

        iterationMaxConstraintIndex = FindMaxPositionWithTolerance[iterationConstraintValues, 0];
        iterationMaxConstraintValue = nonlinearConstraintFunctions[[First[iterationMaxConstraintIndex]]] /. iterationSolutionPointReplaceList;

        Sow[{IterationNumber,iterationMaxConstraintIndex},"MaxNonlinearConstraintIndex"];
        Sow[{IterationNumber,iterationMaxConstraintValue},"MaxNonlinearConstraintValue"];

        debugPrint[" (MI)LP solution: ", iterationSolutionPointReplaceList];

        If[iterationMaxConstraintValue <= OptionValue[ConstraintTolerance] || absGap <= OptionValue[AbsoluteGapTolerance] || relGap <= OptionValue[RelativeGapTolerance],
          If[Length[integerVariables] > 0 && OptionValue[PrimalBoundNLPIterationGap] >= 0,
            timePrimalBoundSearch[[1]] = timePrimalBoundSearch[[1]] + AbsoluteTiming[
              integerVariableReplaceVals = Thread[integerVariables -> ( integerVariables /. iterationSolutionPointReplaceList)];
              primalSolutionCandidate = SolvePrimalBoundNLPProblem[objectiveFunction, linearConstraintIntersection && nonlinearConstraintsIntersection &&primalboundCut&&dualboundCut, nonlinearConstraintFunctions, iterationSolutionPointReplaceList, variables, integerVariableReplaceVals];
              If[(primalSolutionCandidate[[1]] < primalSolution[[1]]) && Max[nonlinearConstraintFunctions /. integerVariableReplaceVals /. primalSolutionCandidate[[2]]] < 10^-6,
                PrintToConsole["New primal bound from NLP call: ", primalSolutionCandidate[[1]]];
                primalSolution = primalSolutionCandidate;
                Sow[{IterationNumber,primalSolution[[1]]},"PrimalObjectiveValue"];
                Sow[{IterationNumber,primalSolutionCandidate[[2]]},"PrimalSolutionPoint"];
                absGap = Abs[primalSolution[[1]] - iterationSolutionObjective];
                relGap = Abs[primalSolution[[1]] - iterationSolutionObjective] / (10^-10 + Abs[primalSolution[[1]]]);
                If[OptionValue[UsePrimalBoundCut],
                  primalboundCut = objectiveFunction <= primalSolution[[1]];
                  Sow[{IterationNumber,objectiveFunction <= primalSolution[[1]]},"PrimalBoundCut"];
                ];

                ,
                PrintToConsole["No new primal bound from NLP call: ", primalSolutionCandidate[[1]]];
              ];
            ][[1]],
            PrintToConsole["Iter: ", PaddedForm[IterationNumber, 3], "\tObj. value: ", PaddedForm[objCoeff * iterationSolutionObjective, {6, 5}], "\tConstr error:\t", PaddedForm[iterationMaxConstraintValue, {6, 6}], "\trel/abs gap: ", PaddedForm[relGap, {3, 3}], "/", PaddedForm[absGap, {3, 3}]];
          ];
        ];

        Sow[{IterationNumber,absGap},"AbsoluteGap"];
        Sow[{IterationNumber,relGap},"RelativeGap"];

        If[OptionValue[Strategy] == "ESH",
        (* Begin supporting Hyperplane strategy *)
          linesearchLambdaFunction = (1 - lambda)interiorPoint + lambda (variables /. iterationSolutionPointReplaceList) // Simplify;
          linesearchLambdaFunctionReplaceList = Table[variables[[i]] -> linesearchLambdaFunction[[i]], {i, 1, Length[variables]}];
          timeRootSearch[[1]] = timeRootSearch[[1]] + AbsoluteTiming[
            Which[
              OptionValue[RootMethod] == "FindRoot",
              maxFunctionLinesearch = Max[nonlinearConstraintFunctions] /. linesearchLambdaFunctionReplaceList;
              lambdaSolution = FindRoot[maxFunctionLinesearch == 0, {lambda, 0}];
              hyperplaneGenerationPointReplaceList = linesearchLambdaFunctionReplaceList /. lambdaSolution;
              ,
              OptionValue[RootMethod] == "Bisection",
              maxFunctionLinesearch = nonlinearConstraintFunctions /. linesearchLambdaFunctionReplaceList;
              lambdaSolutionTmp = (BisectionSearch[maxFunctionLinesearch, lambda, {0.0, 1.0}, OptionValue[ConstraintTolerance] / 10]);
              If[Length[lambdaSolutionTmp] < 2,
                lambdaSolution = lambda -> 1.0;,
                lambdaSolution = lambda -> lambdaSolutionTmp[[2]];];
              hyperplaneGenerationPointReplaceList = linesearchLambdaFunctionReplaceList /. lambdaSolution;
            ]][[1]];
          PrintToConsole["Iter: ", PaddedForm[IterationNumber, 3], "\tObj. value: ", PaddedForm[objCoeff * iterationSolutionObjective, {6, 5}], "\tConstr error:\t", PaddedForm[iterationMaxConstraintValue, {6, 6}], "\t\[Lambda]: ", PaddedForm[lambda /. lambdaSolution, {3, 3}], "\trel/abs gap: ", PaddedForm[relGap, {3, 3}], "/", PaddedForm[absGap, {3, 3}]]
        (* End supporting Hyperplane strategy*)
        ];

        If[OptionValue[Strategy] == "ECP",
        (* Begin normal cutting plane strategy*)
          hyperplaneGenerationPointReplaceList = iterationSolutionPointReplaceList;
          PrintToConsole["Iter: ", PaddedForm[i, 3], "\tObj. value: ", PaddedForm[objCoeff * iterationSolutionObjective, {6, 5}], "\tConstr error:\t", PaddedForm[iterationMaxConstraintValue, {6, 6}]]
        (* End normal cutting plane strategy*)
        ];

        hyperplaneGenerationPoint = variables /. hyperplaneGenerationPointReplaceList;
        debugPrint[" HP gen. point: ", hyperplaneGenerationPoint];
        debugPrint[" Error in gen. point: ", Max[nonlinearConstraintFunctions /. hyperplaneGenerationPointReplaceList]];

        Sow[{IterationNumber,hyperplaneGenerationPointReplaceList},"HyperplanePoint"];

        generationPointConstraintValues = nonlinearConstraintFunctions /. hyperplaneGenerationPointReplaceList;
        generationPointMaxConstraintIndexes = FindMaxPositionWithTolerance[generationPointConstraintValues, 0.01];

        For[j = 1, j <= Length[generationPointMaxConstraintIndexes], j++,
          hyperPlane = CreateCuttingPlaneBase[nonlinearConstraintFunctions[[generationPointMaxConstraintIndexes[[j]]]], hyperplaneGenerationPoint, variables] - OptionValue[ConstraintToleranceConstant] <= 0;
          hyperplanesGenerated = hyperplanesGenerated && hyperPlane;
          debugPrint[" HP added: ", Expand[hyperPlane]];

          Sow[{IterationNumber,hyperPlane},"Hyperplane"];
        ];

        If[Length[integerVariables] > 0 && (OptionValue[PrimalBoundNLPIterationGap] > 0 && Mod[i, OptionValue[PrimalBoundNLPIterationGap]] == 0),
          timePrimalBoundSearch[[1]] = timePrimalBoundSearch[[1]] + AbsoluteTiming[
            integerVariableReplaceVals = Thread[integerVariables -> ( integerVariables /. iterationSolutionPointReplaceList)];
            primalSolutionCandidate = SolvePrimalBoundNLPProblem[objectiveFunction, linearConstraintIntersection && nonlinearConstraintsIntersection, nonlinearConstraintFunctions, iterationSolutionPointReplaceList, variables, integerVariableReplaceVals];
            maxPrimalConstraintValue=Max[nonlinearConstraintFunctions /. integerVariableReplaceVals /. primalSolutionCandidate[[2]]];
            If[(primalSolutionCandidate[[1]] < primalSolution[[1]]) && maxPrimalConstraintValue < 10^-6,
              PrintToConsole["New primal bound from NLP call: ", primalSolutionCandidate[[1]]];
              primalSolution = primalSolutionCandidate;
              Sow[{IterationNumber,primalSolution[[1]]},"PrimalObjectiveValue"];
              Sow[{IterationNumber,primalSolutionCandidate[[2]]},"PrimalSolutionPoint"];
              absGap = Abs[primalSolution[[1]] - iterationSolutionObjective];
              relGap = Abs[primalSolution[[1]] - iterationSolutionObjective] / (10^-10 + Abs[primalSolution[[1]]]);
              If[OptionValue[UsePrimalBoundCut],
                primalboundCut = objectiveFunction <= primalSolution[[1]];
                Sow[{IterationNumber,objectiveFunction <= primalSolution[[1]]},"PrimalBoundCut"];
              ];
              ,
              PrintToConsole["No new primal bound from NLP call: ", primalSolutionCandidate[[1]], " error: ",maxPrimalConstraintValue];
            ];
          ][[1]];
        ];

        If[iterationMaxConstraintValue <= OptionValue[ConstraintTolerance] || absGap <= OptionValue[AbsoluteGapTolerance] || relGap <= OptionValue[RelativeGapTolerance],
          Return[{hyperplanesGenerated, iterationSolution, primalSolution, True, absGap, relGap}]
        ];
      ];
      {hyperplanesGenerated, iterationSolution, primalSolution, False, absGap, relGap}
    ];


CreateCuttingPlaneBase[gFunction_, hyperplaneGenerationPoint_, variables_] :=
    Module[{nablagFunction, hyperplaneGenerationPointReplaceList, nablagkT, gk, cp, i},
      nablagFunction = D[gFunction, {variables}];
      debugPrint["    Nabla: ", nablagFunction];
      hyperplaneGenerationPointReplaceList = Table[variables[[j]] -> hyperplaneGenerationPoint[[j]], {j, 1, Length[variables]}];
      nablagkT = nablagFunction /. hyperplaneGenerationPointReplaceList;
      debugPrint["    Nabla-values: ", nablagkT];
      gk = gFunction /. hyperplaneGenerationPointReplaceList;
      cp = gk + nablagkT.(variables - (variables /. hyperplaneGenerationPointReplaceList)) // Simplify;
      cp
    ];


SolvePrimalBoundNLPProblem[objectiveFunction_, constraintIntersection_, nonlinearConstraintFunctions_, iterationSolutionPointReplaceList_, variables_, integerVariableReplaceVals_] :=
    Module[{},
      primalProblem = {objectiveFunction, constraintIntersection} /. integerVariableReplaceVals;
      primalSolutionCandidate = FindMinimum[primalProblem, Thread[List[variables, (variables /. iterationSolutionPointReplaceList)]](*,PrecisionGoal\[Rule]2,AccuracyGoal\[Rule]2*)]
    ];


BisectionSearch[functs_List, x_Symbol, {a_, b_}, eps_?Positive] :=
    Module[{c, d, e, func, x, activeFunctionIndexes, tmpActiveFunctionIndexes, valueA, valueB, valueE, pointA, pointB, pointE},
      activeFunctionIndexes = Table[i, {i, 1, Length[functs]}]; (* Contains the nonvalid constraints in the exterior point *)

      {valueB, pointB, activeFunctionIndexes} = ConstraintFunction[functs, activeFunctionIndexes, x, b];

      {valueA, pointA, tmpActiveFunctionIndexes} = ConstraintFunction[functs, activeFunctionIndexes, x, a];

      If[valueA * valueB > 0, Return[{}]];
      {c, d} = If[valueA > 0, {b, a}, {a, b}];
      While[Abs[d - c] > eps,
        With[{e = (c + d) / 2},
          {valueE, pointE, tmpActiveFunctionIndexes} = ConstraintFunction[functs, activeFunctionIndexes, x, e];
          If[valueE < 0, c = e, d = e;activeFunctionIndexes = tmpActiveFunctionIndexes]]

      ];
      {c, d}
    ];

ConstraintFunction[functs_List, activeFunctionIndexes_List, x_Symbol, point_] :=
    Module[{newActiveFunctionIndexes, maxValue, maxIndex, tmpFunctionValue, i},
      newActiveFunctionIndexes = {};
      maxValue = functs[[activeFunctionIndexes[[1]]]] /. {x -> point};
      maxIndex = activeFunctionIndexes[[1]];
      If[maxValue > 0, AppendTo[newActiveFunctionIndexes, activeFunctionIndexes[[1]]];];

      For[i = 2, i <= Length[activeFunctionIndexes], i++,
        tmpFunctionValue = (functs[[activeFunctionIndexes[[i]]]] /. {x -> point});
        If[tmpFunctionValue > maxValue,
          maxIndex = activeFunctionIndexes[[i]];
          maxValue = tmpFunctionValue;
        ];

        If[tmpFunctionValue > 0, AppendTo[newActiveFunctionIndexes, activeFunctionIndexes[[i]]];];
      ];
      If[maxValue > 0, Return[{maxValue, maxIndex, newActiveFunctionIndexes}], Return[{maxValue, maxIndex, activeFunctionIndexes}]];
    ];


FindMaxPositionWithTolerance[list_, tol_] :=
    Module[{val, x, maxval, positions},
      If[tol < 0, tol = 0];
      If[tol > 1, tol = 1];
      maxval = Max[list];
      positions = If[maxval >= 0, Position[list, x_ /; x >= maxval(1 - tol)], Position[list, x_ /; x >= maxval(1 + tol)]];
      Return[positions // Flatten];
    ];


(* Method for printing debug information, put //PrintDebug after function to activate *)
Attributes[PrintDebug] = {HoldAll};
debugPrint[expr_] := Null;
PrintDebug[expr_] := Block[{debugPrint = PrintToConsole}, expr];

$OldLine = -1;
PrintToConsole[expr_] := (SelectionMove[MessagesNotebook[], After, Cell];
NotebookWrite[MessagesNotebook[], Cell[BoxData[ToBoxes[expr]], "Print", CellLabel -> "During evaluation of In[" <> ToString@$Line <> "]:=", ShowCellLabel -> ($OldLine =!= $Line)]];
$OldLine = $Line;);
PrintToConsole[expr__] := (SelectionMove[MessagesNotebook[], After, Cell];
NotebookWrite[MessagesNotebook[], Cell[BoxData[ToBoxes[Row@{expr}]], "Print", CellLabel -> "During evaluation of In[" <> ToString@$Line <> "]:=", ShowCellLabel -> ($OldLine =!= $Line)]];
$OldLine = $Line;);


Options[SolveNLPProblem] = {
  ConstraintTolerance -> 0.0001,
  MaximalIterations -> 5,
  PositiveNonlinearObjectiveBound -> 10^5,
  NegativeNonlinearObjectiveBound -> -10^5
};

SolveNLPProblem[Omega_, obj_, g_, vars_, OptionsPattern[]] :=
    Module[{newvars, OmegaLP, xSol, newPT, Fvals, maxerror, xk, lambdasol, replist, F, OmegaTmp, maxgi, newexpr, mu, lambda, solPts},

    (*Checking value of options*)

      If[OptionValue[ConstraintTolerance] <= 0,
        Message[SolveNLPProblem::badargs, "ConstraintTolerance", 0.0001];
        SetOptions[SolveNLPProblem, ConstraintTolerance -> 0.0001]];

      If[OptionValue[MaximalIterations] < 0,
        Message[SolveNLPProblem::badargs, "MaximalIterations", 40];
        SetOptions[SolveNLPProblem, MaximalIterations -> 40]];

      solPts = {};
      newvars = Append[vars, mu];
      OmegaLP = {Omega && OptionValue[NegativeNonlinearObjectiveBound]
          <= mu <= OptionValue[PositiveNonlinearObjectiveBound] };
      For[i = 0, i <= OptionValue[MaximalIterations], i++,
        debugPrint["Constraints: ", OmegaLP[[i + 1]]];


        xSol = FindMinimum[{mu, OmegaLP[[i + 1]]}, newvars,
          Method -> "LinearProgramming"];
        AppendTo[solPts, vars /. xSol[[2]]];
        debugPrint["LP solution: ", xSol];

        If[i == 0,
          newPT = solPts[[1]];
          maxerror = Infinity;
          lambdasol = Undefined;
          Fvals = Table[g[[j]] /. xSol[[2]], {j, 1, Length[g]}];
          ,
          xk = lambda solPts[[i + 1]] + (1.0 - lambda) (solPts[[i]]);
          replist = Table[vars[[i]] -> xk[[i]], {i, 1, Length[vars]}];
          F = Max[g] /. N[replist];
          lambdasol = FindMinimum[{F}, {lambda, 0.1, 0.0, 1.0}] // Quiet;
          newPT = xk /. lambdasol[[2]];
          debugPrint["Linesearch solution: ", newPT];
          maxerror = Abs[lambdasol[[1]] - xSol[[1]]];
          Fvals = g /. (replist /. lambdasol[[2]]);
        ];


        PrintToConsole["Iteration: ", PaddedForm[i + 1, 3],
          "\t Objective value:\t", PaddedForm[xSol[[1]], 10],
          "\tMax deviation: \t", PaddedForm[maxerror, 6]];

        If[maxerror <= OptionValue[ConstraintTolerance], Break[]];

        maxgi = FindMaxPositionWithTolerance[Fvals, 0.2];
        debugPrint["Adding constraints to: ", maxgi];
        debugPrint["Value for constraints: ", Fvals, ". Maximal constraints: ", maxgi];

        OmegaTmp = OmegaLP[[i + 1]];
        For[j = 1, j <= Length[maxgi], j++,
          newexpr = CreateCuttingPlaneBase[g[[maxgi[[j]]]], newPT, vars];
          OmegaTmp = OmegaTmp && newexpr - mu <= 0;
        ];
        AppendTo[OmegaLP, OmegaTmp];
      ];

      Return[{xSol[[1]],vars/.xSol[[2]]}];
    ];

End[] (* `Private` *)

EndPackage[]