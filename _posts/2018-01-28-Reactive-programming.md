---
layout: post
title: Reactive programming
date: 2018-01-28 23:30:00
comments: true
excerpt: Apps nowadays needs to be responsive and reacts almost on everything. Also event based programming was long time with us and we need some higher and better abstraction for working with events. And Reactive programming is answer.
---
Apps nowadays needs to be responsive and reacts almost on everything. Also event based programming was long time with us and we need some higher and better abstraction for working with events. And Reactive programming is answer.

## What is Reactive programming?
If you are looking for an exact definition of what reactive programming is, I have to disappoint you, because it is hard to find on the internet. But you can begin with [The Reactive Manifesto](https://www.reactivemanifesto.org/) with one drawback: understanding it is not a simple task. I must agree with many developers, which have written posts about reactive programming, that definition on Wikipedia is vague, Microsoft's definition is based only on their technology and Reactive Manifesto is maybe something for business. The very simple definition of reactive programming will be:

***"Reactive programming is using asynchronous streams, where a stream can emit any data structure or object."***

### Stream is observable
A stream is a sequence of events ordered in time. **Stream can emit three types of objects. A value, an error and completed.** The Value is emitted after the normal run of code block. The error is emitted when some exception occurs during emitting value or running code block. And completed is "signal", which is emitted at the end of the stream.

### Event functions are observers
All emitted objects, can be caught by subscribing via function. That function is called observer and stream is called observable. As we can see, this is exactly an **Observer design pattern**.

### Schedulers for asynchronous
Reactive programming is asynchronous programming. So, we need some thread management for observable and observers threads and tells them on which thread should they run.

### Stream Functions
Reactive programming also **includes a toolbox of functions**, which can create and filter streams. Input and output of those functions is a stream (also can be multiple streams) or object. For example, these functions are: merge multiple streams, map stream value into another, filter stream values and etc.

## Why use Reactive programming?
Before we answer why we should use reactive programming, we must answer question: "Why should we need asynchronous tasks?" The answer is very simple. We cannot predict when user will do something with GUI component or when we get the response from external system. So, we need to define what will happen if we get response or when some component will change state. And if we take this together with a definition of reactive programming, as a result we get, that reactive programming will give us more readable and simplier logic of our application for asynchronous tasks. And as result all of this, we will get a smooth and responsive application.

## Conclusion
Reactive programming is high level of abstraction in our code and this helps us to focus more on events from business logic, than on large amounts of implementation details. The short definition of reactive programming is:

***Reactive programming = Observers(event functions) + Observables(streams) + Schedulers(thread management) + Functions(merge, map, zip,...)***

## References
* [The introduction to Reactive Programming you've been missing](https://gist.github.com/staltz/868e7e9bc2a7b8c1f754)
* [The Only Introduction to Reactive Programming You Need](https://dzone.com/articles/only-introduction-reactive)
* [What is Reactive Programming?](https://medium.com/@kevalpatel2106/what-is-reactive-programming-da37c1611382)
* [Reactive programming](https://en.m.wikipedia.org/wiki/Reactive_programming)

