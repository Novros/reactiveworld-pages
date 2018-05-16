---
layout: post
title: Observable - Combining
date: 2018-05-16 23:30:00
comments: true
excerpt: Combining operators combine two Observables together by creating new Observable with items from source Observables.
---
Combining operators combine two Observables together by creating new Observable with items from source Observables.

Before we will start describing individual operators, I must point out two things. Some of the operators are static, and some of them are instance methods. And also almost all methods support delaying errors, which means that, if an error occurs during combining Observables, it will postpone error to the end of both Observable (emitting a complete event) and instead of emitting a complete event, it will emit catched error.

### StartWith
Add items before emitting items of Observable.

*Method variants: startWith, startWithArray*

``` java
final Observable<Integer> observableA = Observable.range(0, 4);
final Observable<Integer> observableB = Observable.range(10, 4);

observableA.startWith(observableB).subscribe(new PrintObserver());
```

```
On subscribe.
On next: 10
On next: 11
On next: 12
On next: 13
On next: 0
On next: 1
On next: 2
On next: 3
On completed.
```

Start with array:

``` java
final Observable<Integer> observableA = Observable.range(0, 4);
final Observable<Integer> observableB = Observable.range(10, 4);

observableA.startWithArray(110, 111, 112).subscribe(new PrintObserver());
```

```
On subscribe.
On next: 110
On next: 111
On next: 112
On next: 0
On next: 1
On next: 2
On next: 3
On completed.
```

### Concat
Add items after all emitted items of Observable.

*Method variants: concat, concatDelayError, concatEager, concatArray, contactArrayDelayError, concatArrayEager*

``` java
final Observable<Integer> observableA = Observable.range(0, 4);
final Observable<Integer> observableB = Observable.range(10, 4);

observableA.concatWith(observableB).subscribe(new PrintObserver());
```

```
On subscribe.
On next: 0
On next: 1
On next: 2
On next: 3
On next: 10
On next: 11
On next: 12
On next: 13
On completed.
```

### Merge
Simply merge two Observables into one by merging their emitted items.

*Method variants: merge, mergeDelayError, mergeArray, mergeArrayDelayError*

``` java
final Subject<String> observableA = PublishSubject.create();
final Subject<String> observableB = PublishSubject.create();

observableA.mergeWith(observableB).subscribe(new PrintObserver());
observableA.onNext("A1");
observableA.onNext("A2");
observableB.onNext("B1");
observableA.onNext("A3");
observableB.onNext("B2");
observableB.onNext("B3");
observableA.onNext("A4");
observableB.onNext("B4");
```

```
On subscribe.
On next: A1
On next: A2
On next: B1
On next: A3
On next: B2
On next: B3
On next: A4
On next: B4

```

And merge with error in source Observable.

``` java
final Subject<String> observableA = PublishSubject.create();
final Subject<String> observableB = PublishSubject.create();

observableA.mergeWith(observableB).subscribe(new PrintObserver());
observableA.onNext("A1");
observableA.onNext("A2");
observableB.onNext("B1");
observableA.onError(new IllegalArgumentException());
observableB.onNext("B2");
observableB.onNext("B3");
observableA.onNext("A4");
observableB.onNext("B4");
```

```
On subscribe.
On next: A1
On next: A1
On next: A2
On next: A2
On next: B1
On next: B1
On error: IllegalArgumentException: null
On error: IllegalArgumentException: null
```

### Switch
Can be used only on Observable which emits Observables. This operator subscribes to emit Observable and emit its items. When next Observable is emitted, it will unsubscribe from previous Observable and subscribe to new one and emit items from new Observable.

*Method variants: switchIfEmpty, switchOnNext, switchOnNextDelayError*

Example for switchIfEmpty:

``` java
final Observable<Integer> observableA = Observable.range(0, 4);
final Observable<Integer> observableB = Observable.range(10, 4);

observableA.switchIfEmpty(observableB).subscribe(new PrintObserver());
```

```
On subscribe.
On next: 0
On next: 1
On next: 2
On next: 3
On completed.
```

And now with empty Observable:

``` java
final Observable<Integer> observableB = Observable.range(10, 4);

Observable.empty().switchIfEmpty(observableB).subscribe(new PrintObserver());
```

```
On subscribe.
On next: 10
On next: 11
On next: 12
On next: 13
On completed.
```

Example for switchOnNext:

``` java
final Observable<Integer> observableA = Observable.range(0, 4);
final Observable<Integer> observableB = Observable.range(10, 4);

Observable.switchOnNext(Observable.just(observableA, observableB)).subscribe(new PrintObserver());
```

```
On subscribe.
On next: 0
On next: 1
On next: 2
On next: 3
On next: 10
On next: 11
On next: 12
On next: 13
On completed.
```

### Zip
Zip two Observables together by function. It means that, it will return new Observable with items returned by applying given function on each pair of emitted items from source Observables. First emitted item from returning Observable is the item returned from applying the function on first item of first Observable with first item from second Observable. Second emitted item by returning Observable is the item returned from applying the function on the second emitted item of first Observable with a second emitted item of second Observable.

*Method variants: zip, zipArray, zipIterable*

``` java
final Observable<Integer> observableA = Observable.range(0, 4);
final Observable<Integer> observableB = Observable.range(10, 4);
final BiFunction<Integer, Integer, String> zipper = (integer, integer2) -> "A" + integer + "B" + integer2;

observableA.zipWith(observableB, zipper).subscribe(new PrintObserver("Zip"));
```

```
[Zip] On subscribe.
[Zip] On next: A0B10
[Zip] On next: A1B11
[Zip] On next: A2B12
[Zip] On next: A3B13
[Zip] On completed.
```

Zip operator with delay error = false:

``` java
final Subject<Integer> errorObservable = PublishSubject.create();
final Observable<Integer> observableB = Observable.range(10, 4);
final BiFunction<Integer, Integer, String> zipper = (integer, integer2) -> "A" + integer + "B" + integer2;

errorObservable.zipWith(observableB, zipper, false).subscribe(new PrintObserver("delayError=false"));
errorObservable.onNext(2);
errorObservable.onError(new IllegalArgumentException());
errorObservable.onNext(3);
errorObservable.onNext(4);
```

```
[delayError=false] On subscribe.
[delayError=false] On next: A2B10
[delayError=false] On error: IllegalArgumentException: null
```

And zip operator with delay error = true:

``` java
final Subject<Integer> errorObservable2 = PublishSubject.create();
final Observable<Integer> observableB = Observable.range(10, 4);
final BiFunction<Integer, Integer, String> zipper = (integer, integer2) -> "A" + integer + "B" + integer2;

errorObservable2.zipWith(observableB, zipper, true).subscribe(new PrintObserver("delayError=true"));
errorObservable2.onNext(2);
errorObservable2.onError(new IllegalArgumentException());
errorObservable2.onNext(3);
errorObservable2.onNext(4);
```

```
[delayError=true] On subscribe
[delayError=true] On next: A2B10
[delayError=true] On error: IllegalArgumentException: null
```

### CombineLatest
Combine two Observables together like zip, but instead of waiting for item on the same index, it will combine it with latest emitted from other Observable.

*Method variants: combineLatest, combineLatestDelayError*

``` java
final Subject<String> observableA = PublishSubject.create();
final Subject<String> observableB = PublishSubject.create();

Observable.combineLatest(observableA, observableB, (integer, integer2) -> integer + integer2).subscribe(new PrintObserver());

observableA.onNext("A1");
observableA.onNext("A2");
observableB.onNext("B1");
observableA.onNext("A3");
observableB.onNext("B2");
observableB.onNext("B3");
observableA.onNext("A4");
observableB.onNext("B4");
```

```
On subscribe.
On next: A2B1
On next: A3B1
On next: A3B2
On next: A3B3
On next: A4B3
On next: A4B4
```

### Join
This operator is little bit complex. It joins two Observable by overlapping their opened emitting windows. Let's start by looking on method signature:

``` java
public final <TRight,TLeftEnd,TRightEnd,R> Observable<R> join(
    ObservableSource<? extends TRight> other,
    Function<? super T,? extends ObservableSource<TLeftEnd>> leftEnd,
    Function<? super TRight,? extends ObservableSource<TRightEnd>> rightEnd,
    BiFunction<? super T,? super TRight,? extends R> resultSelector)
```

As you can see, it takes three Observable as arguments and one function. So, lets start with description of argument methods:

-  other - Observable to join with
- leftEnd - function, which must return Obervable, which close window of source Observbale - no matter of value, it just needs to emit value or complete event
- rightEnd - same as previous argument, but for joining Observable
- resultSelector - function which is used for combining the values of source and joining Observable

And last thing what we need to know, is when a window of Observables opens and which values are combined. Each value, which is emitted opens an own window, which is closed by observable returned from argument function. And values which are combined, are defined by overlapping of the open windows of values.

So, when Observable *A* emits value, window for this values is open. Until is window closed, this value will be combined with every value which will be emitted from Observable *B*.

``` java
final Subject<String> observableA = PublishSubject.create();
final Subject<String> observableB = PublishSubject.create();

observableA.join(observableB,
		source -> Observable.timer(20, TimeUnit.MILLISECONDS), // Window open for 20 millis
		joinWith -> Observable.timer(100, TimeUnit.MILLISECONDS), // Window open for 100 millis
		(source, joinWith) -> source + " - " + joinWith)
		.subscribe(new PrintObserver());

observableA.onNext("A1");
observableA.onNext("A2");
observableB.onNext("B1");
Thread.sleep(30);
observableA.onNext("A3");
Thread.sleep(30);
observableB.onNext("B2");
Thread.sleep(30);
observableB.onNext("B3");
observableA.onNext("A4");
Thread.sleep(30);
observableB.onNext("B4");
```

```
On subscribe.
On next: A1 - B1
On next: A2 - B1
On next: A3 - B1
On next: A4 - B1
On next: A4 - B2
On next: A4 - B3
```
