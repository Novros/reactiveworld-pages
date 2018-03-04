---
layout: post
title: Observables creation functions
date: 2018-03-04 23:30:00
comments: true
excerpt: In this post we will look at functions, which creates Observables from imperative style objects or create Observable with generating items itself.
---
In this post we will look at functions, which creates Observables from imperative style objects or create Observable with generating items itself.

## Basic creation methods
The methods in this section are the basic methods for creating Observable.

### From
Simple methods how to create Observable from imperative programming style objects. For example, creating Observable from array or Callable.

*Method variantions: fromArray, fromFuture, fromCallable, fromRunnable, ...*

#### Examples
Creation of Observable from array:
``` java
final Observable<Integer> observable = Observable.fromArray(1, 2, 3);
observable.subscribe(new PrintObserver());
```
```
On subscribe.
On next: 1
On next: 2
On next: 3
On completed.
```

Creation of Observable from callable:
``` java
final Observable<Integer> observable = Observable.fromCallable(() -> 6);
observable.subscribe(new PrintObserver());
```
```
On subscribe.
On next: 2
On next: 9
On next: 4
On completed.
```

Creation of Observable from iterable:
``` java
final Collection<Integer> iterable = new ArrayList<>();
iterable.add(2); iterable.add(9); iterable.add(4);

final Observable<Integer> observable = Observable.fromIterable(iterable);
observable.subscribe(new PrintObserver());
```
```
On subscribe.
On next: 6
On completed.
```

### Just
Create observable from item or items and then the items will be emitted. 

*Method variantions*: just(arrg), just(arg, arg2), just(arg,arg2,arg3), ...

### Examples
Just method for one argument:
``` java
final Observable<Integer> observable = Observable.just(5);
observable.subscribe(new PrintObserver());
```

```
On subscribe.
On next: 5
On completed.
```

Just method with three arguments:
``` java
final Observable<Integer> observable = Observable.just(10, 3, 5);
observable.subscribe(new PrintObserver());
```

```
On subscribe.
On next: 10
On next: 3
On next: 5
On completed.
```

## Special creation methods
The methods in this section create Observable with generated value defined by the implementation.

### Range
Emits sequence of numbers defined by interval as input.

``` java
final Observable<Integer> observable = Observable.range(2, 5);
observable.subscribe(new PrintObserver())
```
```
On subscribe.
On next: 2
On next: 3
On next: 4
On next: 5
On next: 6
On completed.
```

### Empty
Create Observable, which will emit zero items and completes normally.

``` java
final Observable<Integer> observable = Observable.empty();
observable.subscribe(new PrintObserver());
```
```
On subscribe.
On completed.
```

### Never
Creates never ending Observable which will never ever emit any item.

``` java
final Observable<Integer> observable = Observable.never();
observable.subscribe(new PrintObserver());
```
```
On subscribe.
```

### Throw
Creates Observable from exception which will be thrown after subscription.

``` java
final Observable<Integer> observable = Observable.error(new IllegalArgumentException());
observable.subscribe(new PrintObserver());
```
```
On subscribe.
On error: IllegalArgumentException: null
```

## Repteat creation methods
The methods in this section create Observable with generated values from implementation of the method.

### Interval
Create Observable which will emit a sequence of numbers spaced with given interval of time.

*Method variantions*: interval, intervalRange

``` java
Observable<Long> observable = Observable.interval(1, 1, TimeUnit.SECONDS);
observable.subscribe(new PrintObserver());
Thread.sleep(3000);

observable = Observable.interval(0, 1, TimeUnit.SECONDS);
observable.subscribe(new PrintObserver());
Thread.sleep(3000);
```
```
On subscribe.
On next: 0
On next: 1
On next: 2
```

``` java
Observable<Long> observable = Observable.interval(0, 1, TimeUnit.SECONDS);
observable.subscribe(new PrintObserver());
Thread.sleep(3000);
```

```
On subscribe.
On next: 0
On next: 1
On next: 2
On next: 3
```

### Repeat
Repeatedly emits specified item.

## Timer
Emits one item (specified at calling method) after the specified span of the time.

``` java
final Observable<Long> observable = Observable.timer(1, TimeUnit.SECONDS);
observable.subscribe(new PrintObserver());
Thread.sleep(3000);
```
```
On subscribe.
//after 2 seconds
On next: 0
On completed.
```

## Other creations methods

## Create
Create Observable from scratch by function or ObservableEmitter.

``` java
final Observable<Integer> observable = Observable.create(observableEmitter -> {
    for (int i = 0; i < 3; i++) {
         observableEmitter.onNext(i);
    }
});
observable.subscribe(new PrintObserver());
```
```
On subscribe.
On next: 0
On next: 1
On next: 2
```