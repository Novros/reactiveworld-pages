---
layout: post
title: Reactive programming - deeper
date: 2018-02-04 23:30:00
comments: true
excerpt: In this post, we will look at more in depth of Reactive Programming. What is the evaluation model of reactive programming and some challanges of implementation of Reactive programming.
---
In this post, we will look at more in depth of Reactive Programming. What is the evaluation model of reactive programming and some challanges of implementation of Reactive programming.

## Evaluation model
Evaluation model is a data flow graph. In simple meaning, **It defines how changes will be propagated throughout the whole dependency graph of values and calculations.** Nodes of this graph represent computations and edges represents a dependency relationship. This graph is used by runtime part of language to find which computations must recomputed again, when the input of graph change. So, as we can see, evaluation model is the most necessary object of reactive programming.

![Usage of evaluation model in Reactive programming]({{ "/assets/images/2018-02-04-Reactive_programming-deeper/evaluation_graph_usage.png" | absolute_url }})

### Implementation algorithms
There exist many algorithms for implementing propagation of changes in evaluation model, but the most common are: pull, push or pull-push algorithm.

#### Pull based model
**This model "pulls" values (when they are needed) from source.** That means,  that propagation of changes is driven by new values need. This type has one big disadvantage: there can be very big latency between event and response, because all values must computed immediately after change.

#### Push based model
**This model "push's" new values immediately when it has them.** Propagation is driven by the existence of new values.

#### Pull-push model
The pull-push model is a simple combination of pull and push model. When used strategy of propagation depends on the situation and structure of the evaluation model.

### Static or dynamic
Evaluation model can be static or dynamic. That means, that model is created before execution of the program (static model) or is modified during execution of program (dynamic model).

### Simple example of evaluation in model
On the image below, we can see a simple example of evaluation model, where are three variables two operators. Computation of this model is simple and straight forward.

![Simple example of evalution of model]({{ "/assets/images/2018-02-04-Reactive_programming-deeper/evaluation_model.png" | absolute_url }})

## Implementation challenges
Implementation of Reactive programming has some challenges, which must be resolved. The most common challenge is an evaluation model with cyclic dependencies. Also second challenge is avoiding time based leaks.

### Bad evaluation - glitch
Bad evaluation can happen, when the computation starts before all dependency expressions are evaluated. This can lead to state, when new values are combined with old values. And this lead to bad state of program and to unnecessary reevaluation of values. To eliminate this, we must reorder expressions and sort them topologically. This can exist only in implementations, which using push based model. Also other types of models should avoid the computation of expression with unchanged values.

On the image below, we can see an example of bad evaluation. State 2, can happen when first operator has one operand with long delay computation and evaluation starts before it ended. So, second operator will get together old value with the new value.

![Evalutaion glitch]({{ "/assets/images/2018-02-04-Reactive_programming-deeper/evaluation_glitch.png" | absolute_url }})

### Cyclic dependencies
In ideal world evaluation graph should be always directed acyclic graph. But in practice, a programmer or program can define cycles in the graph and implementation of reactive programming must deal with them. The common solution for this is terminating computation, before the next cycle of computation in the cycle. This is can be done by adding delay operator between first and the last element of cycle, which will move computation to next time step.

## Conclusion
In this post we looked at some information and challenges of implementing Reactive programming, which we should be aware of, but in real use, it will not happen or we cannot avoid them.

## Referencies
- [Reactive programming on wikipedia](https://en.wikipedia.org/wiki/Reactive_programming)
- Bainomugisha, E.; Carreton, A. L.; van Cutsem, T.; aj.: A Survey on Reactive Programming. ACM Comput. Surv. ISSN 0360-0300, doi:10.1145/2501654.2501666.
