
KD-Trey5
========

A kd-tree (https://en.wikipedia.org/wiki/K-d_tree) implementation written in Scala and designed to run on AWS DynamoDB as a persistence backend.

The code works, is decently tested, but shouldn't be considered production-grade yet as it has never been battle-tested.

A few minor implementation notes:

* The design allows for custom CoordinateSystems that define how data points measured distance-wise against each other, e.g. you could bring your own 2D, 3D, 4D or other coordinate systems.

* The implementation generalizes the typical binary-tree structure of the KD-Tree into a n-ary tree.  Nodes can have a custom fan-out.  In that respect, this implementation is closer to what is typically considered a B+Tree.

* The design doesn't optimize for duplicate keys, yet handles duplicates.

* The kd-tree construction algorithm assumes primitives of map-reduce framework / divide and conquer akin to Spark.  A simple local implementation is provided (which runs across cores in multi-threaded fashion).  There is no Spark implementation of the kdtrey5.DataSet interfaces yet.


Why the name KD-Trey5?
======================

It sounds close enough to kd-tree and happens to be Kevin Durant's (of basketball fame) twitter handle (https://twitter.com/KDtrey5).  Let's pretend it's in homage to him.  Kevin has not endorsed this project.

