Heuristic Search
================

Haskell package implementing various heuristic search algorithms. Whenever
possible the implementation tries to be as polymorphic as possible. All
algorithms are implemented using explicit successor functions etc., in order to
be applicable both to explicit graphs in memory as well as implicitly generated
graphs in any state space search problem.

Algorithms
~~~~~~~~~~

In no particular order

* A*
* IDA*
* Dijkstra (specialization of A*)
* RBFS
* DFS
  - Depth first Graph search
  - Depth first Tree search
  - Depth first traversal with duplicate detection
  - Depth first traversal without duplicate detection
* BFS
  - Breadth first Graph search
  - Breadth first traversal with duplicate detection
  - Breadth first traversal without duplicate detection
* Iterative Deepening Search
* Depth limited search
* Hill-climbing search
  - Standard hill-climbing
  - Anytime variant
  - Random restart hill-climbing
* Enforced Hill-climbing
* Simulated Annealing
* Beam local search
