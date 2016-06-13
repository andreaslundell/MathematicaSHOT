# MathematicaSHOT
The Supporting Hyperplane Optimization Toolkit (SHOT) solver for convex mixed-integer nonlinear programming (MINLP)

This implementation of the extended supporting hyperplane (ESH) and extended cutting plane (ECP) algorithms for solving convex mixed-integer nonlinear programming (MINLP) problems is made by Andreas Lundell, andreas.lundell@abo.fi.

The supporting hyperplane optimization toolkit (SHOT) solver is described in the journal paper: 

*Jan Kronqvist, Andreas Lundell and Tapio Westerlund, The extended supporting hyperplane algorithm for convex mixed-integer nonlinear programming, Journal of Global Optimization 64 (2), DOI: 10.1007/s10898-015-0322-3 2015*. 

If you use this solver for research, please cite the paper above. 

The SHOT solver consists of two Mathematica applications. The first is the main solver contained in the file SHOT.m and the second is contained in the file OSiLReader.m, and is a parser for the OSiL format, which is an XML-based file format for optimization problems. The second is not strictly required, and it is possible to input problems manually in Mathematica syntax as well.

Note that the performance of this solver is not as good as for the C++ version detailed in the paper above. The main reason for this is due to the fact that the LinearProgramming solver used to solve the mixed-integer linear programming (MILP) subproblems in Mathematica is not as efficient as those used in the other implementation. 
