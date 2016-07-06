# geekie-bayes

... is a scala framework for dealing with Bayesian networks.

## Inference

There are a few inference algorithms implemented in `geekie.pgm.inference.*`:

* `Enumeration`: simplest exact inference algorithm there is. It enumerates all possible configurations for a network and adds their joint probability together. It's exponential in the number of nodes, so it gets _really_ unmanageable _really_ fast. Mainly useful for _really_ tiny networks and testing other algorithms;


* `VariableElimination`: exact inference via a series of factors multiplications and variable sumouts. Also exponential, but smarter than `Enumeration`. Check out its [Wikipedia entry](https://en.wikipedia.org/wiki/Variable_elimination).


* `BeliefPropagation`: If we can model our Bayesian net using a tree-shaped structure, we can use Belief Propagation to perform inference in linear time! This allows us to do inference in huge networks efficiently. See `geekie.pgm.examples.BigTreeInference` for an example. Also, check out its [Wikipedia entry](https://en.wikipedia.org/wiki/Belief_propagation).

Examples are provided in `geekie.pgm.examples`.

## Learning Bayesian Networks from data

We use this framework to study ways of learning the parameters and the structure of Bayesian Networks from data.

### Learning parameters

Given a bunch of data and a Bayesian Network structure, learning the parameters for the conditional distributions is relatively easy and can be done optimally.

TODO: example of using `ModelProvider` for transforming data into `CPDs` for a network.


### Learning structure

Learning _both_ the structure and the parameters for a Bayesian Network from data is a much harder problem. In such problems, we are interested in finding a Bayesian Network that explains the data we are observing fairly well.

Several optimization techniques can be used to navigate through this insanely huge space of all possible network structures. We investigated using genetic algorithms to achieve that. In that scenario, a Bayesian Network represents an individual, who is capable of reproducing and suffer mutations. Individuals with better abilities to explain the observed data are passed to the next generation. Here, this ability to explain the data -- or "fitniess -- is the likelihood of the data.

Example of such approach can be found under `geekie.ga.examples`.
