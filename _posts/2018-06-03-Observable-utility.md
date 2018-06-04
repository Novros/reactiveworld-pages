---
layout: post
title: Observable - Error and utility functions
date: 2018-06-03 23:30:00
comments: true
excerpt: Sometimes you do not want to onError method of Observer be called. Or you want to react only on some specific event of the lifecycle of Observable. Functions for both of these things are in this post.
---
Sometimes you do not want to onError method of Observer be called. Or you want to react only on some specific event of the lifecycle of Observable. Functions for both of these things are in this post.

## Error functions
Functions in this section operate with errors, which can occures during emitting items.

### Catch
Catch error emitted by Observable and continues with specified item, items or allowing Observable to continue.

*Method variants: onErrorResumeNext(Function), onErrorResume(ObservableSource), onErrorReturn(Function), onErrorReturnItem(T)*

#### Return item in case of error

```java
Subject<Integer> observable = PublishSubject.create();
observable.onErrorReturnItem(999).subscribe(new PrintObserver());
observable.onNext(1);
observable.onError(new IllegalArgumentException());
observable.onNext(2);
```

```
On subscribe.
On next: 1
On next: 999
On completed.
```

#### Return Observable in case of error
In this example new Observable is returned in case of error. But if you change returning of new Observable to returning our main Subject, you will get Subject, which will continue after error.

```java
observable = PublishSubject.create();
observable.onNext(1);
observable.onError(new IllegalArgumentException());
observable.onNext(2);
observable.onErrorResumeNext(Observable.just(997, 998)).subscribe(new PrintObserver());
```

```
On subscribe.
On next: 997
On next: 998
On completed.
```

### Retry
Catch error emitted by Observable and resubscribe to source Observable. So, some items can be emitted again.

*Method variants: retry, retry(times), retry(throwable_predicate), retry(BiPredicate<Integer, Throwable>), retryUntil(BooleanSupplier)*

#### Retry after two exceptions
```java
final Subject<Integer> subject = ReplaySubject.create();
subject.onNext(1);
subject.onNext(2);
subject.onError(new IllegalArgumentException());
subject.onNext(3);
subject.onComplete();

subject.retry(2).subscribe(new PrintObserver("2 times"));
```

```
On subscribe.
On next: 1
On next: 2
On next: 1
On next: 2
On next: 1
On next: 2
On error: IllegalArgumentException: null
```

#### Retry only if specific exception is thrown
```java
final Subject<Integer> subject = ReplaySubject.create();
subject.onNext(1);
subject.onNext(2);
subject.onError(new IllegalArgumentException());
subject.onNext(3);
subject.onError(new IllegalStateException());
subject.onComplete();

subject.retry(throwable -> throwable instanceof IllegalStateException)
     .subscribe(new PrintObserver());
```

```
On subscribe.
On next: 1
On next: 2
On error: IllegalArgumentException: null
```

## Utility functions
In this section we will look at some functions, which do some useful things.

### Delay
It will shift forward begin of source Observable by specified time.

*Method variants: delay(delay, timeUnit), delay(delay, timeUnit, delayError), delay(delay, timeUnit, scheduler)*

``` java
final Observable<Integer> observable = Observable.range(0, 4);
observable.delay(20, TimeUnit.MILLISECONDS).subscribe(new PrintObserver());
Thread.sleep(19);
System.out.println("After 19 millis");
Thread.sleep(1);
System.out.println("After 20 millis");
```

```
On subscribe.
After 19 millis
On next: 0
On next: 1
On next: 2
On next: 3
After 20 millis
On complete.
```

### Do
Register callbacks on a certain event of Observable lifecycle.

*Method variants: doAfterNext, doAfterTerminate, doFinally, doOnComplete, doOnDispose, doOnEach, doOnError, doOnLifeCycle, doOnNext, doOneSubscribe, doOnTerminate*

#### Registering callback on each item

``` java
final Observable<Integer> observable = Observable.range(0, 4);
observable.doOnEach(integerNotification -> System.out.println("on each: " + integerNotification.getValue()))
	.subscribe(new PrintObserver("Each"));
```

```
[Each] On subscribe.
on each: 0
[Each] On next: 0
on each: 1
[Each] On next: 1
on each: 2
[Each] On next: 2
on each: 3
[Each] On next: 3
on each: null
[Each] On completed.
```

#### Registering callback after next

``` java
final Observable<Integer> observable = Observable.range(0, 4);
observable.doAfterNext(integer -> System.out.println("After next: " + integer))
	.subscribe(new PrintObserver("AfterNext"));
```

```
[AfterNext] On subscribe.
[AfterNext] On next: 0
After next: 0
[AfterNext] On next: 1
After next: 1
[AfterNext] On next: 2
After next: 2
[AfterNext] On next: 3
After next: 3
[AfterNext] On completed.
```

### Materialize/Dematerialize
Reverse Observable emition by dividing of emitted events to groups. (Item, error event, complete event)

#### Materialize

``` java
final Observable<Integer> observable = Observable.range(0, 4);
observable.materialize().subscribe(new PrintObserver("Materialize"));
```

```
[Materialize] On subscribe.
[Materialize] On next: OnNextNotification[0]
[Materialize] On next: OnNextNotification[1]
[Materialize] On next: OnNextNotification[2]
[Materialize] On next: OnNextNotification[3]
[Materialize] On next: OnCompleteNotification
[Materialize] On completed.
```

#### Dematerialize

``` java
final Observable<Integer> observable = Observable.range(0, 4);
observable.materialize().dematerialize().subscribe(new PrintObserver("Materialize-Dematerialize"));
```

```
[Materialize-Dematerialize] On subscribe.
[Materialize-Dematerialize] On next: 0
[Materialize-Dematerialize] On next: 1
[Materialize-Dematerialize] On next: 2
[Materialize-Dematerialize] On next: 3
[Materialize-Dematerialize] On completed.
```

## Subscribe functions
In this section we will look at some basic functions for the subscription of Observer to Observable.

### Subscribe
Main operator which connects Observable with Observer.

*Method variants: subscribe(Observer), subscribe(Consumer), subscribe(Consumer, Consumer), subscribe(Consumer, Consumer, Consumer)*

``` java
final Observable<Integer> observable = Observable.range(0, 4);
observable.subscribe(new PrintConsumer(), new PrintConsumer());
observable.subscribe(new PrintObserver());
```

### SubscribeOn
Specifies the Scheduler in which will Observable operates.

Few of schedulers are: Schedulers.io(), Schedulers.computation(), Schedulers.newThread().

### ObserverOn
Specifies the Scheduler in which will Observer operates

Few of schedulers are: Schedulers.io(), Schedulers.computation(), Schedulers.newThread().

## Time functions
And in the last section we will look at functions operates with time.

### Timeinterval
Convert Observable to Observable, which emits time elapsed between emitted items instead of emitting items.

*Method variants: timeInterval(), timeInterval(TimeUnit), timeInterval(Scheduler), timeInterval(TimeUnit, Scheduler)*

``` java
final Subject<Integer> observable = PublishSubject.create();
observable.timeInterval().subscribe(new PrintObserver());

observable.onNext(1);
Thread.sleep(10);
observable.onNext(2);
observable.onNext(3);
Thread.sleep(25);
observable.onNext(4);
```

```
On subscribe.
On next: Timed[time=1, unit=MILLISECONDS, value=1]
On next: 0
On next: 1
On next: 2
On next: 3
On completed.
On next: Timed[time=11, unit=MILLISECONDS, value=2]
On next: Timed[time=0, unit=MILLISECONDS, value=3]
On next: Timed[time=25, unit=MILLISECONDS, value=4]
```

### Timeout
Wraps Observable with timeout ability. That means, if no value after defined time will not be emitted from a source Observable, then error will be emitted.

*Method variants: timeout(long, TimeUnit), timeout(long, TimeUnit, ObservableSource),  timeout(long, TimeUnit, Scheduler),  timeout(long, TimeUnit, Scheduler, ObservableSource), ...
*

``` java
final Subject<Integer> observable = PublishSubject.create();
observable.timeout(20, TimeUnit.MILLISECONDS).subscribe(new PrintObserver());
observable.onNext(1);
Thread.sleep(10);
observable.onNext(2);
observable.onNext(3);
Thread.sleep(25);
observable.onNext(4);
```

```
On subscribe.
On next: 1
On next: 2
On next: 3
On error: TimeoutException: null
```

### Timestamp
Attach a timestamp to each emitted item. Timestamp indicates the time when was item emitted.

*Method variants:  timestamp, timestamp(timeUnit), timestamp(scheduler), timestamp(timeUnit, scheduler)*

``` java
final Observable<Integer> observable = Observable.range(0, 4);
observable.timestamp().subscribe(new PrintObserver());
```

```
On subscribe.
On next: Timed[time=1527689871840, unit=MILLISECONDS, value=0]
On next: Timed[time=1527689871840, unit=MILLISECONDS, value=1]
On next: Timed[time=1527689871842, unit=MILLISECONDS, value=2]
On next: Timed[time=1527689871842, unit=MILLISECONDS, value=3]
On completed.
```
