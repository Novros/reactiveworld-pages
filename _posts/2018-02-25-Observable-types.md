---
layout: post
title: Observable - types 
date: 2018-02-25 23:30:00
comments: true
excerpt: Observables (streams) are the main part of Reactive programming. We will look at different types of streams, usages and also look at simple examples. A used reactive library for examples is ReactiveX.
---
Observables (streams) are the main part of Reactive programming. We will look at different types of streams, usages and also look at simple examples. A used reactive library for examples is ReactiveX.

## Observer
Observer subscribes to observable and reacts to observable emitted objects. It is constructed from these four methods:

- **onNext** - This method is called when Observable emits an item, where the parameter is emitted item. OnNext is main method when we are putting logic in reactive Programming.
- **onError** - Method called when Observable failed to emit expected data or Observable encountered some error in internal implementation and could not continue to emit items. After call of this method will not be method onNext or onCompleted called.
- **onCompleted** - This method signalize end of observable stream. Observable will call this method after final calling of onNext method.
- **onSubscribe** - Method called immediately after subscribing to observable.

In Reactive Streams definition is Observer called Consumer.

### Example
Simple implementation of observer which will print calling of methods to default output stream.

``` java
package cz.novros.reactiveworld.observer;

import io.reactivex.Observer;
import io.reactivex.disposables.Disposable;

public class PrintObserver implements Observer<Object> {
	@Override
	public void onSubscribe(final Disposable disposable) {
		System.out.println("On subscribe.");
	}

	@Override
	public void onNext(final Object o) {
		System.out.println("On next: " + o);
	}

	@Override
	public void onError(final Throwable throwable) {
		System.out.println("On error: " + throwable.getClass().getSimpleName() + ": " + throwable.getMessage());
	}

	@Override
	public void onComplete() {
		System.out.println("On completed.");
	}
}
```

## Consumer
The consumer is a specialized type of Observer, which have only one method accept, which is called as onNext or onError or onComplete method. On which call is called is defined by order in the subscribing method in Observable.

### Example
``` java
package cz.novros.reactiveworld.observer;

import io.reactivex.functions.Consumer;

public class PrintConsumer implements Consumer<Object> {

	@Override
	public void accept(final Object o) throws Exception {
		System.out.println("Accept: " + o);
	}
}
```

## Observable
The observable is one of the base type of streams in reactive programming. Observer subscribes to an observer and the observable emits item or sending notification about their state to the observer.

### Creation
We can create Observable by many static methods of Observable. For example:

- Observable.just(arg), Observable.just(arg, arg2), .... - Creates observable from given arguments.

``` java
final Observable<Integer> observable = Observable.just(10, 5);
observable.subscribe(new PrintObserver());
```
```
On subscribe.
On next: 10
On next: 5
On completed.
```

- Observable.fromArray(1,2,3) - Creates observable from given array.

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

There are many and many methods and also transformation functions, filter functions and so on.

### Subscribe and unsubscribe
If we want to subscribe to observable we only need to implement Observer interface described in the previous chapter. Or subscribe with one (onNext) or two(onNext and onError) or three(onNext, onError and onComplete) consumers.

In some implementations of Reactive programming you can also unsubscribe after subscribing to observable. For this function, they're specialized interface observable, which have unsubscribe method. The result of calling unsubscribe can cascade of calling unsubscribe on chain of operators, which were applied to Observable. So, it can take time to stop emitting items.

``` java
final Observable<Integer> observable = Observable.just(10, 5);
// Disposable is returned only for subscribing with Consumer.
final Disposable disposable = observable.subscribe(new PrintConsumer());
disposable.dispose(); // Unsubscribing
```

### Hot and cold observable
There are two types of Observables. One is called hot and second is called cold. Hot means that Observable will start emitting items as soon as it is created. So any observer which will subscribe after some time can start observing items somewhere in the middle. And cold is opposite of hot, that means that Observable will wait for first subscriber. So, with this type of Observable is guaranteed that we will see the whole sequence of items.

#### Example use case
If you are thinking about use cases for hot and cold observable it can be found in GUI. For example: We want to react on button click, but we do not need to react on the whole sequence of events, but only when Observer will subscribe. So hot Observable is handy here. On the other hand, when the user types text into the text box, we need to observe all key events, before we subscribes to Observable. Cold observable will help us here.

### Operators
Observables in the state as we have described would be only extended Observer pattern. The real power comes with operators and reactive functions on Observables. Operators which will allow us to transform, combine, manipulate and work with emitted items from Observable. These operators have one big benefit. It also supports chaining, so we can apply these operators one after the after (because they return Observable). And then each operator in chain modifies the Observable that results from the operation of the previous operator.

For example, we can create new Observable from two existing Observables by function Merge:

``` java
final Observable<Integer> observableA = Observable.just(1, 2);
final Observable<Integer> observableB = Observable.just(5, 6);
observableA.mergeWith(observableB).subscribe(new PrintObserver());
```
```
On subscribe.
On next: 1
On next: 2
On next: 5
On next: 6
On completed.
```

## Single
Single is something like an Observable, but instead of emitting *n* items, it always emits one value or an error. And it has also one another benefit, we do not need to implement (as observer) all methods (onNext, onError, onCompleted), but only two methods:

- **onSuccess** - Called when Single emits one item.
- **onError** - Callee when Single was unable to emit item.

Each of this methods are called only onces.

### Subscribing
For subscribing we must implement inteface SingleObserver. Also we can subscribe as Consumer. And we can not subscrive as Observer to Single. Example of SingleObserve is below:
``` java
package cz.novros.reactiveworld.observer;

import io.reactivex.disposables.Disposable;

public class SingleObserver implements io.reactivex.SingleObserver<Object> {

	@Override
	public void onSubscribe(final Disposable disposable) {
		System.out.println("On subscribe.");
	}

	@Override
	public void onSuccess(final Object o) {
		System.out.println("On success: " + o);
	}

	@Override
	public void onError(final Throwable throwable) {
		System.out.println("On error: " + throwable.getClass().getSimpleName() + ": " + throwable.getMessage());
	}
}
```

### Single example
```java
final Single<Integer> single = Single.fromCallable(() -> 2);
single.subscribe(new SingleObserver());
```
```
On subscribe.
On success: 2
```

## Subject
Subject acts as Observable and Observer. It is something like bridge between *n* Observables and *n* observers. It remits items from all subscribed observable to all subscribed Observers. But it can also emit new items. In the Reactive streams terminology, it is called Processor. There are four types of Subject, which are designed for particular use cases.

### BehaviorSubject
When observer subscribes to BehaviourSubject, it will start emitting items by last emitted value or default value defined at creation of the subject. And then will continue to emit any other items by subscribed Observables or next method on the subject.

``` java
// It can be create by also by create() method.
final Subject<Integer> subject = BehaviorSubject.createDefault(0); 
subject.subscribe(new PrintObserver());

System.out.println("- calling on next");
subject.onNext(1);
subject.onNext(2);

System.out.println("- subscribing second observer");
subject.subscribe(new PrintObserver());

System.out.println("- subscribing subject to observable");
final Observable<Integer> observable = Observable.fromArray(8, 9);
observable.subscribe(subject);
```
```
On subscribe.
On next: 0
- calling on next
On next: 1
On next: 2
- subscribing second observer
On subscribe.
On next: 2
- subscribing subject to observable
On next: 8
On next: 8
On next: 9
On next: 9
On completed.
On completed.
```

#### Errors
 If subscribed Observables ends with error or method (onError) is called, it will emit error notification and will not emit any other items.

```java
final Subject<Integer> subject = BehaviorSubject.create();
subject.subscribe(new PrintObserver());

System.out.println("- calling on error");
subject.onError(new IllegalArgumentException());

System.out.println("- calling on next");
subject.onNext(1);

```
```
On subscribe.
- calling on error
On error: IllegalArgumentException: null
- calling on next
```

### PublishSubject
PublishSubject emits only those items which are emitted after subscription time. An error will again emit error notification and stop emitting any other items.

```java
final Subject<Integer> subject = PublishSubject.create();
subject.subscribe(new PrintObserver());

System.out.println("- calling on next");
subject.onNext(1);
subject.onNext(2);

System.out.println("- subscribing second observer");
subject.subscribe(new PrintObserver());

System.out.println("- subscribing subject to observable");
final Observable<Integer> observable = Observable.fromArray(8, 9);

observable.subscribe(subject);
```
```
On subscribe.
- calling on next
On next: 1
On next: 2
- subscribing second observer
On subscribe.
- subscribing subject to observable
On next: 8
On next: 8
On next: 9
On next: 9
On completed.
On completed.
```

### ReplaySubject
ReplaySubject will emit all emitted items from the begin of subjects. If observer will subscribe after some time, subject emits to observer all items from the beginning of a subject and then all other emitted items.

```java
final Subject<Integer> subject = ReplaySubject.create();

System.out.println("- calling on next");
subject.onNext(1);

System.out.println("- subscribing observer");
subject.subscribe(new PrintObserver());

System.out.println("- calling on next");
subject.onNext(1);
subject.onNext(2);

System.out.println("- subscribing second observer");
subject.subscribe(new PrintObserver());

System.out.println("- subscribing subject to observable");
final Observable<Integer> observable = Observable.fromArray(8, 9);
observable.subscribe(subject);
```
```
On subscribe.
On next: 1
- calling on next
On next: 1
On next: 2
- subscribing second observer
On subscribe.
On next: 1
On next: 1
On next: 2
- subscribing subject to observable
On next: 8
On next: 8
On next: 9
On next: 9
On completed.
On completed.
```

### AsyncSubject
AsyncSubject is similar to BehaviourSubject in the way that it caches values. But AsyncSubject will only store last value and only publish it when emitting is completed.

``` java
final Subject<Integer> subject = AsyncSubject.create();
subject.subscribe(new PrintObserver());

System.out.println("- calling on next");
subject.onNext(1);
subject.onNext(2);

System.out.println("- subscribing second observer");
subject.subscribe(new PrintObserver());

System.out.println("- subscribing subject to observable");
final Observable<Integer> observable = Observable.fromArray(8, 9);
observable.subscribe(subject);
```
```
On subscribe.
- calling on next
- subscribing second observer
On subscribe.
- subscribing subject to observable
On next: 9
On completed.
On next: 9
On completed.
```

## Source code
All source code can be found in [repository on github](https://github.com/Novros/reactiveworld-examples).
