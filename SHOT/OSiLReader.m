(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: OSiLReader *)
(* :Context:  *)
(* :Author: Andreas Lundell *)
(* :Date: 2016-06-01 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 Andreas Lundell *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["OSiLReader`"]

ClearAll["OSiLReader`"];

ImportProblemFromOSiLFile::usage = "ImportProblemFromOSiLFile[filename, directory, reformulate] imports the problem in OSiL format from filename in directory. The paramter reformulate parameter is True/False and indicates whether a possible nonlinear objective function should be rewritten as a constraint. ";

ImportProblemFromOSiL::usage = "ImportProblemFromOSiL[osil, reformulate] imports an optimization problem from the OSiL XML format given as the string osil. The paramter reformulate parameter is True/False and indicates whether a possible nonlinear objective function should be rewritten as a constraint.";


Begin["`Private`"]

Options[ImportProblemFromOSiLFile] = {
  PositiveNonlinearObjectiveBound -> 10^6,
  NegativeNonlinearObjectiveBound -> -10^6
};

Options[ImportProblemFromOSiL] = {
  PositiveNonlinearObjectiveBound -> 10^6,
  NegativeNonlinearObjectiveBound -> -10^6
};


ParseVariables::usage = "ParseVariables[xml] parses the variables given in the string xml and returns a list containing, variable index, name, type (B/I/R), lower bound and upper bound.";

ParseObjective::usage = "ParseObjective[xml] parses the objective function given in the string xml and returns a list of each term in the structure {variable index, coefficient, direction (max/min)}. ";

ParseLinearValues::usage = "ParseLinearValues[xml] parses the el-elements in the linearConstraintCoefficients given as xml (taking into consideration multiplicities and increases";

ParseLinearTerms::usage = "ParseLinearTerms[xml] parses the linear terms in the linearConstraintCoefficient subtree and returns a list containing a structure with constraint (1,...,n), variable (1,...,m) and coefficient";

ParseQuadraticTerms::usage = "ParseQuadraticTerms[xml] parses the quadratic terms given as xml and returns a list of the terms {constraint index, variable one, variable two, coefficient}. ";

ParseConstraints::usage = "ParseConstraints[xml] parses the constraints and return a matrix with columns constraint name, lower bounds and upper bounds.";

ParseNonlinearTerms::usage = "ParseNonlinearTerms[xml] parses the nonlinear expressions given in xml and returns a structure with constraint index (0 if objective) and a Mathematica expression. The variables are still returned as XMLElements.";

If[False,
  DoCommand = ParallelDo;
  ParallellizeDos = True
  ,
  DoCommand = Do;
  ParallellizeDos = False];


ImportProblemFromOSiLFile::nodir = "Directory `1` does not exist.";
ImportProblemFromOSiLFile::nofile = "File `1` does not exist.";

ImportProblemFromOSiLFile[filename_String, root_String, reformulate_, options___] :=
    Module[{fullfilename = FileNameJoin[{root, filename}], filecontents},
      If[!TrueQ@DirectoryQ@root,
        Message[ImportProblemFromOSiLFile::nodir, root];
        Quit[]
      ];

      If[!TrueQ@FileExistsQ@fullfilename,
        Message[ImportProblemFromOSiLFile::nofile, filename];
        Quit[]
      ];
      filecontents = Import[fullfilename, "XML"];

      ImportProblemFromOSiL[filecontents,reformulate,options]
    ];

ImportProblemFromOSiL[osilSource_, reformulate_, OptionsPattern[]] :=
    Module[{varsParsed, objParsed, objConstant, objDirection, linValuesParsed, quadTermsParsed, nonlinearExpressionsParsed, constraintsParsed, numVars, numObjTerms, numConstraints, numLinValues, numQuadTerms, numNonlinearExprs, listVars, listVarBounds, constraintFunctions, numConstraintsNonlinear, numConstraintsLinear, ctrNonlinear, ctrLinear, finalObjective, listConstraintsLinear, listConstraintsNonlinear, isConstraintNonlinear, isObjectiveNonlinear, listVarIntegers},

      varsParsed = ParseVariables[Cases[osilSource, XMLElement["variables", _, _], 5]];

      {objParsed, objConstant, objDirection} = ParseObjective[Cases[osilSource, XMLElement["obj", _, _], 7]];

      linValuesParsed = ParseLinearTerms[Cases[osilSource, XMLElement["linearConstraintCoefficients", ___, subelements_] :> subelements, Infinity]];

      quadTermsParsed = ParseQuadraticTerms[Cases[osilSource, XMLElement["quadraticCoefficients", ___, subelements_] :> subelements, Infinity]];

      nonlinearExpressionsParsed = ParseNonlinearTerms[Cases[osilSource, XMLElement["nonlinearExpressions", ___, subelements_] :> subelements, Infinity]];

      constraintsParsed = ParseConstraints[Cases[osilSource, XMLElement["constraints", ___, subelements_] :> subelements, Infinity]];

      numObjTerms = Length[objParsed];
      listVars = varsParsed[[All, 1]];
      finalObjective = objConstant + Sum[varsParsed[[objParsed[[i, 1]], 1]] * objParsed[[i, 2]], {i, 1, numObjTerms}];

      numVars = Length[varsParsed];
      numConstraints = Length[constraintsParsed];
      numLinValues = Length[linValuesParsed];
      numQuadTerms = Length[quadTermsParsed];
      numNonlinearExprs = Length[nonlinearExpressionsParsed];

      listVarBounds = MapThread[LessEqual, {varsParsed[[All, 3]], varsParsed[[All, 1]], varsParsed[[All, 4]]}];

      If[ParallellizeDos,
      (*For parallel computation*)
        SetSharedVariable[constraintFunctions, linValuesParsed, varsParsed, quadTermsParsed, finalObjective, nonlinearExpressionsParsed, isConstraintNonlinear, ctrLinear, ctrNonlinear]
      ];

      constraintFunctions = Array[0&, numConstraints];
      isConstraintNonlinear = Array[False&, numConstraints];
      isObjectiveNonlinear = False;

      With[{Do2 = DoCommand},
        Do2[
          constraintFunctions[[linValuesParsed[[i, 1]]]] = constraintFunctions[[linValuesParsed[[i, 1]]]] + linValuesParsed[[i, 3]] * varsParsed[[linValuesParsed[[i, 2]], 1]],
          {i, 1, numLinValues}
        ];
      ];

      With[{Do2 = DoCommand},
        Do2[
          If[quadTermsParsed[[i, 1]] == 0,
            finalObjective += quadTermsParsed[[i, 4]] * varsParsed[[quadTermsParsed[[i, 2]], 1]] * varsParsed[[quadTermsParsed[[i, 3]], 1]];
            isObjectiveNonlinear = True;
            ,
            constraintFunctions[[quadTermsParsed[[i, 1]]]] += quadTermsParsed[[i, 4]] * varsParsed[[quadTermsParsed[[i, 2]], 1]] * varsParsed[[quadTermsParsed[[i, 3]], 1]];
            isConstraintNonlinear[[quadTermsParsed[[i, 1]]]] = True;
          ],
          {i, 1, numQuadTerms}
        ];
      ];

      With[{Do2 = DoCommand},
        Do2[
          If[nonlinearExpressionsParsed[[i, 1]] == 0,
            finalObjective += nonlinearExpressionsParsed[[i, 2]];
            isObjectiveNonlinear = True;
            ,
            constraintFunctions[[nonlinearExpressionsParsed[[i, 1]]]] += nonlinearExpressionsParsed[[i, 2]];
            isConstraintNonlinear[[nonlinearExpressionsParsed[[i, 1]]]] = True;
          ],
          {i, 1, numNonlinearExprs}
        ];
      ];

      constraintFunctions = constraintFunctions //. (XMLElement["variable", {v1_, v2_}, ___] :> Internal`StringToDouble[("coef" /. v2)] * varsParsed[[ToExpression[("idx" /. v1)] + 1, 1]]);

      If[isObjectiveNonlinear,
        finalObjective = finalObjective //. (XMLElement["variable", {v1_, v2_}, ___] :> Internal`StringToDouble[("coef" /. v2)] * varsParsed[[ToExpression[("idx" /. v1)] + 1, 1]]);
      ];

      numConstraintsNonlinear = Length[Cases[isConstraintNonlinear, True]];
      numConstraintsLinear = numConstraints - numConstraintsNonlinear;

      listConstraintsLinear = Array[0&, numConstraintsLinear];
      listConstraintsNonlinear = Array[0&, numConstraintsNonlinear];

      ctrLinear = 0;
      ctrNonlinear = 0;

      With[{Do2 = DoCommand},
        Do2[
          If[isConstraintNonlinear[[i]],
            ctrNonlinear++;
            If[constraintsParsed[[i, 2]] == constraintsParsed[[i, 3]], listConstraintsNonlinear[[ctrNonlinear]] = Equal[constraintFunctions[[i]], constraintsParsed[[i, 3]]],
              If[constraintsParsed[[i, 2]] == -Infinity,
                listConstraintsNonlinear[[ctrNonlinear]] = LessEqual[constraintFunctions[[i]], constraintsParsed[[i, 3]]],
                If[constraintsParsed[[i, 3]] == Infinity,
                  listConstraintsNonlinear[[ctrNonlinear]] = LessEqual[-constraintFunctions[[i]], -constraintsParsed[[i, 2]]],
                  listConstraintsNonlinear[[ctrNonlinear]] = LessEqual[constraintsParsed[[i, 2]], constraintFunctions[[i]], constraintsParsed[[i, 3]]]
                ];
              ];
            ];
            ,
            ctrLinear++;
            If[constraintsParsed[[i, 2]] == constraintsParsed[[i, 3]], listConstraintsLinear[[ctrLinear]] = Equal[constraintFunctions[[i]], constraintsParsed[[i, 3]]],
              If[constraintsParsed[[i, 2]] == -Infinity,
                listConstraintsLinear[[ctrLinear]] = LessEqual[constraintFunctions[[i]], constraintsParsed[[i, 3]]],
                If[constraintsParsed[[i, 3]] == Infinity,
                  listConstraintsLinear[[ctrLinear]] = LessEqual[-constraintFunctions[[i]], -constraintsParsed[[i, 2]]],
                  listConstraintsLinear[[ctrLinear]] = LessEqual[constraintsParsed[[i, 2]], constraintFunctions[[i]], constraintsParsed[[i, 3]]]
                ];
              ];
            ];
          ],
          {i, 1, numConstraints}
        ];
      ];

      listVarIntegers = Outer[Element, Select[varsParsed, #[[2]] == "B" || #[[2]] == "I"&][[All, 1]], {Integers}] // Flatten;

      If[reformulate && isObjectiveNonlinear,
        AppendTo[listVars, Symbol["addobjvar"]];
        AppendTo[listConstraintsNonlinear, LessEqual[finalObjective - Last[listVars], 0]];
        finalObjective = Last[listVars];
        AppendTo[listVarBounds, LessEqual[OptionValue[NegativeNonlinearObjectiveBound], finalObjective, OptionValue[PositiveNonlinearObjectiveBound]]];
      ];

      {finalObjective, listConstraintsLinear, listConstraintsNonlinear, listVarBounds, listVarIntegers, listVars, objDirection}
    ];


ParseVariables[xml_] :=
    Module[{nodes, numVars, varSymbols, varTypes, varLowerBounds, varUpperBounds},
      nodes = Cases[xml, XMLElement["var", _, _], Infinity];
      numVars = Length[nodes];  (*The individual variables*)

      (*Clear all symbols before initializing them*)
      If[varSymbols != Null, Do[Clear[varSymbols[[i]]], {i, 1, Length[varSymbols]}]];

      (*For parallel computation*)
      If[ParallellizeDos,
        SetSharedVariable[varSymbols, varTypes, varLowerBounds, varUpperBounds, varTypes];];

      varSymbols = Array[Symbol["BB"], numVars]; (*Preinitialize array for the variable symbol*)
      varTypes = Array[" ", numVars]; (*Preinitialize array for the variable type*)
      varLowerBounds = Array[0&, numVars];(*Preinitialize array for the lb*)
      varUpperBounds = Array[Infinity&, numVars];(*Preinitialize array for the ub*)

      With[{Do2 = DoCommand},
        Do2[(*Parse the variables*)
          varSymbols[[i]] = Symbol["name" /. nodes[[i, 2]]];
          varTypes[[i]] = ("type" /. nodes[[i, 2]]) /. "type" -> "R";

          varTmpBnd = ("lb" /. nodes[[i, 2]]);
          If[varTmpBnd == "lb",
            If[varTypes[[i]] == "R" || varTypes[[i]] == "",
              varLowerBounds[[i]] = 0.0,
              If[varTypes[[i]] == "B",
                varLowerBounds[[i]] = 0,
                If[varTypes[[i]] == "I",
                  varLowerBounds[[i]] = 0;
                ]]],
            varLowerBounds[[i]] = Internal`StringToDouble[varTmpBnd];
          ];

          varTmpBnd = ("ub" /. nodes[[i, 2]]);
          If[varTmpBnd == "ub",
            If[varTypes[[i]] == "R" || varTypes[[i]] == "",
              varUpperBounds[[i]] = Infinity,
              If[varTypes[[i]] == "B",
                varUpperBounds[[i]] = 1,
                If[varTypes[[i]] == "I",
                  varUpperBounds[[i]] = 100;
                ]]],
            varUpperBounds[[i]] = Internal`StringToDouble[varTmpBnd];
          ],
          {i, 1, numVars}
        ];
      ];

      (*Return as a matrix with columns {variable, type, lb, ub}*)
      Transpose[{varSymbols, varTypes, varLowerBounds, varUpperBounds}]
    ];


ParseObjective[xml_] :=
    Module[{nodes, numTerms, termCoeff, termVarIndex, objConstant, direction},

      nodes = Cases[xml, XMLElement["coef", _, _], Infinity]; (*The individual terms*)

      (*For parallel computation*)
      If[ParallellizeDos,
        SetSharedVariable[termCoeff, termVarIndex];];

      numTerms = Length[nodes];
      termCoeff = Array[0.0, numTerms]; (*Preinitialize array for the coefficient*)
      termVarIndex = Array[0, numTerms]; (*Preinitialize array for the variable index*)

      With[{Do2 = DoCommand},
        Do2[(*Parse the terms in the objective*)
          termVarIndex[[i]] = 1 + ToExpression["idx" /. nodes[[i, 2]]];
          termCoeff[[i]] = Internal`StringToDouble[nodes[[i, 3, 1]]];
          , {i, 1, numTerms}
        ];
      ];

      objConstant = Cases[xml, XMLElement["obj", {___, "constant" -> objconst_, ___}, ___] :> Internal`StringToDouble[objconst]];

      If[objConstant == {}, objConstant = 0.0, objConstant = objConstant[[1]]];

      (*Determine if the problem is a minimization or a maximization problem*)
      direction = "maxOrMin" /. xml[[1, 2, 1]];

      (*Return as a matrix with columns {variable index,coefficient}*)
      {Transpose[{termVarIndex, termCoeff}], objConstant, direction}
    ];


ParseLinearValues[xmlElements_List] :=
    Module[{numElements, mults, incrs, values, elementsExpanded},

      numElements = Length[xmlElements]; (*The number of el-elements we have*)
      mults = Array[0&, numElements]; (*Contains all mult-attributes*)
      incrs = Array[0&, numElements];(*Contains all incr-attributes*)
      values = Array[0&, numElements]; (*Contains all el-values*)

      (*Parses all XML-tags*)
      For[i = 1, i <= numElements, i++,
        mults[[i]] = ToExpression[("mult" /. xmlElements[[i, 2]]) /. "mult" -> 1];
        incrs[[i]] = ToExpression[("incr" /. xmlElements[[i, 2]]) /. "incr" -> 0];
        values[[i]] = Internal`StringToDouble[xmlElements[[i, 3, 1]]];
      ];

      elementsExpanded = Array[0&, Total[mults]]; (*Contains the expanded explicit values*)
      numelem = 0;

      For[i = 1, i <= numElements, i++,
        For[j = 0, j < mults[[i]], j++,
          numelem = numelem + 1;
          elementsExpanded[[numelem]] = values[[i]] + j * incrs[[i]];
        ];
      ];

      elementsExpanded
    ];


ParseLinearTerms[xml_] :=
    Module[{linStarts, linCols, linVals, numTerms, linStartsExpanded, linColsExpanded, linValsExpanded, allElements, numLinStarts, startidx, endidx},

      linStarts = Cases[Cases[xml, XMLElement["start", _, _], Infinity], XMLElement["el", _, _], Infinity];
      linCols = Cases[Cases[xml, XMLElement["colIdx", _, _], Infinity], XMLElement["el", _, _], Infinity];
      (*linRows=Cases[Cases[linCoeffs,XMLElement["rowIdx",_,_],Infinity],XMLElement["el",_,_],Infinity];*)
      linVals = Cases[Cases[xml, XMLElement["value", _, _], Infinity], XMLElement["el", _, _], Infinity];

      (*Contains the expanded vectors with all elements *)
      linStartsExpanded = ParseLinearValues[linStarts];
      linColsExpanded = ParseLinearValues[linCols] + 1;(*linRowsExpanded=ParseLinearValues[linRows];*)
      linValsExpanded = ParseLinearValues[linVals];

      numTerms = Length[linValsExpanded];
      allElements = Array[{0, 0, 0}&, numTerms];

      numelem = 0;
      numLinStarts = Length[linStartsExpanded];
      For[i = 1, i <= numLinStarts - 1, i++,
        startidx = linStartsExpanded[[i]] + 1;
        endidx = linStartsExpanded[[i + 1]] + 1;
        For[j = startidx, j < endidx, j++,
          numelem = numelem + 1;
          allElements[[numelem]] = {i, linColsExpanded[[numelem]], linValsExpanded[[numelem]]};
        ];
      ];
      allElements
    ];


ParseQuadraticTerms[xml_] :=
    Module[{quadCoeffs, nodes, numTerms, termConstraintIndex, termCoeff, termVarIndexOne, termVarIndexTwo},
    (*The whole quadratic coefficient XML-tree*)
      nodes = Cases[xml, XMLElement["qTerm", _, _], Infinity]; (*The individual terms*)

      (*For parallel computation*)
      If[ParallellizeDos,
        SetSharedVariable[termConstraintIndex, termVarIndexOne, termVarIndexTwo, termCoeff];];

      numTerms = Length[nodes];
      termConstraintIndex = Array[0, numTerms]; (*Preinitialize array for the first variable index*)
      termVarIndexOne = Array[0, numTerms]; (*Preinitialize array for the first variable index*)
      termVarIndexTwo = Array[0, numTerms]; (*Preinitialize array for the second variable index*)
      termCoeff = Array[0.0, numTerms]; (*Preinitialize array for the coefficient*)

      With[{Do2 = DoCommand},
        Do2[(*Parse the terms*)
          termConstraintIndex[[i]] = ToExpression["idx" /. nodes[[i, 2]]];
          termVarIndexOne[[i]] = ToExpression["idxOne" /. nodes[[i, 2]]];
          termVarIndexTwo[[i]] = ToExpression["idxTwo" /. nodes[[i, 2]]];
          termCoeff[[i]] = Internal`StringToDouble["coef" /. nodes[[i, 2]]];
          , {i, 1, numTerms}
        ];
      ];

      (*Return as a matrix with columns {constraint index, variable one, variable two, coefficient}*)
      Transpose[{termConstraintIndex + 1, termVarIndexOne + 1, termVarIndexTwo + 1, termCoeff}]
    ];


ParseNonlinearTerms[xml_] :=
    Module[{numExpressions, nlExprs, expressionConstraintIdxs, refExprs},

      nlExprs = Cases[xml, XMLElement["nl", _, _], Infinity]; (*The nonlinear expressions*)
      numExpressions = Length[nlExprs];

      (*For parallel computation*)
      If[ParallellizeDos,
        SetSharedVariable[expressionConstraintIdxs];];

      expressionConstraintIdxs = Array[0, numExpressions];
      With[{Do2 = DoCommand},
        Do2[(*Parse the terms*)
          expressionConstraintIdxs[[i]] = ToExpression["idx" /. nlExprs[[i, 2]]];
          , {i, 1, numExpressions}
        ];
      ];

      refExprs = nlExprs //. XMLElement["nl", _, {u_}] :> u;
      refExprs = refExprs
          //. (XMLElement["number", {v1_, v2_}, _] :> Internal`StringToDouble[("value" /. v1)])
          //. XMLElement["sqrt", {}, {u_}] :> Sqrt[u]
          //. XMLElement["abs", {}, {u_}] :> Abs[u]
          //. XMLElement["negate", _, {u_}] :> Minus[u]
          //. XMLElement["sin", {}, {u_}] :> Sin[u]
          //. XMLElement["ln", {}, {u_}] :> Log[u]
          //. XMLElement["cos", {}, {u_}] :> Cos[u]
          //. XMLElement["tan", {}, {u_}] :> Tan[u]
          //. XMLElement["exp", {}, {u_}] :> Exp[u]
          //. XMLElement["product", {}, u_] :> Times @@ u
          //. XMLElement["square", {}, {u_}] :> u * u
          //. XMLElement["divide", {}, {u1_, u2_}] :> Divide[u1, u2]
          //. XMLElement["sum", {}, u_] :> Total[u]
          //. XMLElement["minus", {}, u_] :> u[[1]] - u[[2]];

      Transpose[{expressionConstraintIdxs + 1, refExprs}]
    ];


ParseConstraints[xml_] :=
    Module[{nodes, numConstrs, conNames, conLowerBounds, conUpperBounds, conEqualityFlags},

      nodes = Cases[xml, XMLElement["con", _, _], Infinity]; (*The individual constraints*)

      (*For parallel computation*)
      If[ParallellizeDos,
        SetSharedVariable[conNames, conLowerBounds, conUpperBounds];];

      numConstrs = Length[nodes];
      conNames = Array[""&, numConstrs]; (*Preinitialize array for the constraintnames*)
      conLowerBounds = Array[-Infinity&, numConstrs]; (*Preinitialize array for the lower bound*)
      conUpperBounds = Array[Infinity&, numConstrs]; (*Preinitialize array for the upper bound*)

      With[{Do2 = DoCommand},
        Do2[(*Parse the constraints*)
          conNames[[i]] = "name" /. nodes[[i, 2]];
          tmplb = "lb" /. nodes[[i, 2]];
          tmpub = "ub" /. nodes[[i, 2]];
          If[tmplb != "lb", conLowerBounds[[i]] = Internal`StringToDouble[tmplb]];
          If[tmpub != "ub", conUpperBounds[[i]] = Internal`StringToDouble[tmpub]];
          , {i, 1, numConstrs}
        ]];

      (*Return as a matrix with columns {constraint name, lower bounds, upper bounds}*)
      Transpose[{conNames, conLowerBounds, conUpperBounds}]
    ];

End[] (* `Private` *)

EndPackage[]