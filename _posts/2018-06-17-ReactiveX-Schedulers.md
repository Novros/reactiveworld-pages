---
layout: post
title: ReactiveX - Schedulers
date: 2018-06-17 23:30:00
comments: true
excerpt: If you want to write a responsive application with some computation, you need to use concurrency. For this purpose ReactiveX has Schedulers, which can be defined during creating data stream.
---
If you want to write a responsive application with some computation, you need to use concurrency. For this purpose ReactiveX has Schedulers, which can be defined during creating data stream.

Concurrency is hard, but we can choose to make it easier by taking advantage of ReactiveX scheduling features. But ReactiveX should not be mistaken for a concurrency framework, because it is designed primarily for querying data and creating nice data flow.

Also must be noted, that ReactiveX is multithreaded by default, but ReactiveX is single-threaded by default. But is free-threaded, which means that you are not restricted to which thread you choose to do your work and you can choose any thread you like for observing, subscription, etc. 

## What is scheduler
The scheduler is an entity, which controls when a subscription starts and when notifications are published.

## Usage of scheduler

### Operators
Usage of the Scheduler is very simple, because some Observable functions have their alternative forms, which allow you to set which scheduler operator will use for its operation. Others do not operate on Scheduler or have default Scheduler.

Also, you can schedule your own work in those Schedulers. For example, using Scheduler. Worker class with method Scheduler.

### Subscribing and observing
You can specify in which thread will be method onNext called by operator ```observeOn```. This method at default tries run ```onNext``` method in the current thread as many times as possible. But beware that, if you use ```observerOn``` method, it can potentially change timing information and can put additional stress on the system.

Simple definition of methods:

- ```SubscribeOn``` - invocation of the subscription - "iterating over items"
- ```ObserveOn``` - observing notifications - "iterating over observers"

Both of these methods are very useful when you are writing UI applications, because you do not want to block the UI thread and you need to update UI objects on the UI thread.

### Recommendation for responsive UI
Pattern for responsive UI:

- respond to any user action
- do long work on the background thread
- pass work from background thread to UI thread
- update UI

If you think about this, you can see here great fit for Rx: responding to events, passing data to chained method calls, defining schedulers for each part of the processing chain.

## Types of schedulers
In this section we will look at the types of schedulers which are implemented in ReactiveX. A base class for all of schedulers is ```Scheduler```.

- **Scheduler.computation** - Scheduler with single-thread pool which has size of available processors to the Java VM.
- **Scheduler.io** - Scheduler similar to computation, but this is for asznchronous work with blocking IO. Use single-threaded pool and tries to reuse previously started instances.
- **Scheduler.from** - Wrap given ```ExecutorService``` and delegates work to it.
- **Scheduler.newThread** - Always schedule action in new thread.
- **Scheduler.single** - Schedule work into one thread. (event-loops)
- **Scheduler.trampoline** - Schedule work into FIFO and use current thread to perform that work.
- **TestScheduler** - Allows you to change the time when you want. It is very useful and necessary for testing Observables and Observers.

## Which Scheduler use
There exists many types of Schedulers and it can be hard to know which Scheduler to use and when. So I will give you my own recommendations with two views on this problem. One table is based on time of operation And second view Is based on layers of application of which is an operation called.

### When to use which Scheduler
| Operation | Scheduler |
| -------- | ------------- |
| Constant time | single |
| Iteration | defualt/computation |
| Time-based | default |
| Asynchronious | computation/newThread |
| IO blocking | io |
| Tests | TestScheduler |

### Where to use which Scheduler
| Layer | Scheduler | Reason |
| --------- | ---------------- | ------------ |
| Presentation layer | observe on currentThread/single| allows to update ViewModels |
| ViewModel | subcribe on background thread | preventing the UI from becoming unresponsive |
| Service | blocking less than 50 ms -> computation; else -> use newThread | |
| IO Service | io | will not block other thread with blocking io work |
