---
layout: post
title: Reactive programming
date: 2018-01-28 23:30:00
comments: true
excerpt: Apps nowadays needs to be very responsive and reacts almost on everything. Also event based programming was long time with us and we need some higher and better abstraction for this. Reactive programming is answer for this.
---

# Reactive Programming
Apps nowadays needs to be very responsive and reacts almost on everything. Also event based programming was long time with us and we need some higher and better abstraction for this. Reactive programming is answer for this.

## What is reactive programming
If you are looking for an exact definition of what reactive programming is, it is hard to find on the internet. You can begin with [The Reactive Manifesto](https://www.reactivemanifesto.org/), but understanding it is not a simple task. I must agree with many developers, which have written about RP, that definition on Wikipedia is vague, Microsoft's definition is based on their technology and Reactive Manifesto is something for business. The very simple definition of reactive programming would be:

***"Reactive programming is using asynchronous streams, where a stream can be any data structure or object."***

### Stream is observable
A stream is a sequence of events ordered in time. **Stream can emit three types of objects. A value, an error and completed.** The Value is emitted after the normal run of code block. The error is emitted when some exception occurs during is emitting value or running code block. And completed is "signal", which is emitted at the end of the stream.

### Event functions are observers
All emitted objects, can be caught by subscribing via function. That function is called observer and stream is called observable. As we can see, this is an **observer design pattern**.

### Schedulers for asynchronous
Reactive programming is asynchronous programming. So, we need some thread management for observable and observers threads and tells them on which thread should run.

### Stream Functions
Reactive programming also **includes a toolbox of functions**, which can create and filter streams, where input and output is stream (also can be multiple streams) or object. For example, these functions are: merge multiple streams, map stream value into another, filter stream values and etc.

## Why to use
Before we answer why should we use reactive programming, we must answer why should we need asynchronous tasks. The answer is very simple. We cannot predict when user will do something with component or when we get the response from external system. So, we need to define what will happen if we get response or when some component will be triggered. And if we get this together with a definition of reactive programming, as a result we get, that reactive programming will give us more readable and simpler logic of our application for asynchronous tasks. And as result all of this, we will get a smooth and responsive application..

## Conclusion
Reactive programming is high level of abstraction in our code and this helps us to focus more on events from business logic, than on large amounts of implementation details. Definition of reactive programming is:

***Reactive programming = Observers(event functions) + Observables(streams) + Schedulers(thread management) + Functions(merge, map, zip,...)***

## References
* [The introduction to Reactive Programming you've been missing](https://gist.github.com/staltz/868e7e9bc2a7b8c1f754)
* [The Only Introduction to Reactive Programming You Need](https://dzone.com/articles/only-introduction-reactive)
* [What is Reactive Programming?](https://medium.com/@kevalpatel2106/what-is-reactive-programming-da37c1611382)
* [Reactive programming](https://en.m.wikipedia.org/wiki/Reactive_programming)

