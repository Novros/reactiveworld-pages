---
layout: post
title: RxJava vs Java stream api
date: 2018-04-29 23:30:00
comments: true
excerpt: In this post, we will look differences between RxJava and Java stream. Where which use and if they are compatible with each other.
---
In this post, we will look differences between RxJava and Java stream. Where which use and if they are compatible with each other.

## RxJava
RxJava is an implementation of Reactive programming in Java under project [ReactiveX](http://reactivex.io/). It uses Observable as stream of items and Observers for consuming emitted items. It contains also rich api for operating with Observables and many more. Some examples of [creating Observable](https://www.reactiveworld.net/2018/03/04/Observable-creation.html), [transforming Observable](https://www.reactiveworld.net/2018/03/18/Observable-transformation.html) and [filtering Observable](https://www.reactiveworld.net/2018/04/15/Observable-filtering.html).

### Few operators
- **creation operators**: empty, never, throw, from, interval, just, range, repeat, create
- **transforming operators**: flatMap, map, scan, buffer, groupBy, window
- **filtering operators**:  debounce, distinct, elementAt, filter, first, ignoreElements, last, Skip, ...
- **mathematical and aggregate operatos**: avarage, concat, count, max, min, reduce, sum
- **combining**: and, then, when, join, merge, zip, switch

### Example
``` java
Observable.range(0, 5)
	.map(operand -> operand + 3)
	.filter(value -> value % 2 == 0)
	.take(2)
	.subscribe(value -> System.out.println("value = " + value));
```

## Java stream api
Stream API in Java was introduced in Java 8. It uses Stream as a wrapper around source entity and after terminal operator, it will traverse over source entity and then returns the result. More about [Java stream api](https://www.reactiveworld.net/2018/04/22/Java-stream-api.html).

### Few operators:
- **creation operators**: empty, generate, of
- **transforming operators**: flatMap, map, sorted
- **filtering operators**: allMatch, anyMatch, distinct,  filter, findAny, findFirst, limit, noneMatch, peek, skip, 
- **mathematical and aggregate operatos**:  count, max, min
- **collecting operators**: collect, forEach, forEachOrdered, iterate, reduce, toArray

### Example
``` java
IntStream.range(0, 5)
	.map(operand -> operand + 3)
	.filter(value -> value % 2 == 0)
	.limit(2)
	.forEach(value -> System.out.println("value = " + value));
```

## Java Flow api
In Java 9 was also added Flow Api, which is the Reactive Streams definition and is very similar to RxJava. It contains only four interfaces as specification, so it should be used for interoperability with different implementations of Flow API(Reactive programming).

Contained interfaces are in package java.util.concurrent.Flow:
- **Publisher<T>** - produces items T
- **Subscriber<T>** - consumes T items
- **Processor<T, R>** - acts as Publisher and Subscriber
- **Subscription** - connection for Publisher and Subscriber (controls messaging)

## Differences
In this section we will look at the main differences between RxJava and Java stream API.

| Java Stream API | RxJava |
| -------- | ------------- |
| pull-based (terminal operator) | push-based (when you are subscribed) |
| can be used only once (one subscriber) | can be used many times (many subscribers) |
| can be run in parallel (many threads) | run in one thread or in another thread |
| parallel can not have defined scheduler (not easily) | scheduler can be defined via onSubscribe() |
| does not have time-related variants of methods | have time-related variants of methods |
| sometimes hard to create | very easily to create |
| auto closable resources must be manually closed by onClose method | Everything is done by Observable by itself |
| only traverse source entity | have backpressure, is synchronized, have latch and so on |
| few methods for working with streams | have very rich API |
| can take advantage of multi-core architectures | does not take advantage of multi-core architecture unless code does not have that logic |
| can not merge/zip two streams | can be merged, zipped, joined with other Observable |
| built in Java | must be add as dependency |

## Which where to use
Both have some similar methods, but they are not built for same usage. Also ReactiveX use  publish-subscriber design, which can in some case complication. On the other hand stream is more something similar to unbounded collection. It is hard to tell where which use, it heavily depends on the situation. But can summarize it.

RxJava has less or same api in different languages(RxJs, RxRuby, ...). Uses always same thread until the scheduler is defined. Has many handy methods for working with Observables (transformation, filtering, merging, ...) and is very easy to create. And have backpressure for very fast producers sources.

Streams allow processing of very large collections efficiently, also with using many threads in parallel. Have standard basic methods for working with them and is easy to create on standard collections.

And if you decide to use ReactiveX, it is good to use Java 9 Flow Api, because then you can easily switch between many implementations of Reactive programming in Java (all frameworks, which contains reactive programming support, implements these interfaces).