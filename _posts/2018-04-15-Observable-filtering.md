---
layout: post
date: 2018-04-15 23:30:00
title: Observable - filtering functions
comments: true
excerpt: After we can create and transform Observable, is good to know, how to filter emitted items and get only items, which we want or we expect.
---
After we can create and transform Observable, is good to know, how to filter emitted items and get only items, which we want or we expect.

Filtering functions filter emitted items of source Observable and return new Observable, which emits only items, which meet the condition of filtering function. Of course that functions are similar to classic filtering of Collections with Java stream api or (Guava)[https://github.com/google/guava].

## Filtering to one item
In this section will look at filter functions, which filter Observable and return Observable with only one item.

### First
As the name suggests it returns Observable with only one item, which was emitted as first.

*Method variants: first(defaultValue), firstElement, firstOrError*

Beware that each variation of the method returns Observable with the different Observer on subscribing. See the example for more info.

``` java
Observable<Integer> observable = Observable.range(1, 10);
observable.first(999).subscribe(new PrintSingleObserver());
observable.firstElement().subscribe(new PrintMaybeObserver());
observable.firstOrError().subscribe(new PrintSingleObserver());
```

```
On subscribe.
On success: 1
On subscribe.
On success: 1
On subscribe.
On success: 1
```

An example in case, where Observable is empty. Where we can see differences between each method variant.

``` java
observable = Observable.empty();
observable.first(999).subscribe(new PrintSingleObserver());
observable.firstElement().subscribe(new PrintMaybeObserver());
observable.firstOrError().subscribe(new PrintSingleObserver());
``` 

```
On subscribe.
On success: 999
On subscribe.
On completed.
On subscribe.
On error: NoSuchElementException: null
```

### Last
Return Observable with only one item, which was emitted before complete.

*Method variants: last(defaultValue), lastElement, lastOrError*

Beware that each variation of the method returns Observable with the different Observer on subscribing. See the example for more info.

``` java
Observable<Integer> observable = Observable.range(1, 10);
observable.last(999).subscribe(new PrintSingleObserver());
observable.lastElement().subscribe(new PrintMaybeObserver());
observable.lastOrError().subscribe(new PrintSingleObserver());
``` 

```
On subscribe.
On success: 10
On subscribe.
On success: 10
On subscribe.
On success: 10
```

An example in case, where Observable is empty. Where we can see differences between each method variant.

``` java
observable = Observable.empty();
observable.last(999).subscribe(new PrintSingleObserver());
observable.lastElement().subscribe(new PrintMaybeObserver());
observable.lastOrError().subscribe(new PrintSingleObserver());
``` 

```
On subscribe.
On success: 999
On subscribe.
On completed.
On subscribe.
On error: NoSuchElementException: null
```

### ElementAt
Return Observable with only one item, which was emitted as *n*-th value. If given index is out of bounds, IndexOutOfBoundsException is thrown.

*Method variantions: elementAt(index), elementAt(index, defaultValue), elementOrError(index)*

``` java
Observable<Integer> observable = Observable.range(1, 10);
observable.elementAt(2).subscribe(new PrintMaybeObserver());
observable.elementAt(2, 999).subscribe(new PrintSingleObserver());
observable.elementAtOrError(11).subscribe(new PrintSingleObserver());
```

```
On subscribe.
On success: 3
On subscribe.
On success: 3
On subscribe.
On error: NoSuchElementException: null
```

An example in case, where Observable is empty. Where we can see differences between each method variant.

``` java
observable = Observable.empty();
observable.elementAt(2).subscribe(new PrintMaybeObserver());
observable.elementAt(2, 999).subscribe(new PrintSingleObserver());
observable.elementAtOrError(11).subscribe(new PrintSingleObserver());
``` 

```
On subscribe.
On completed.
On subscribe.
On success: 999
On subscribe.
On error: NoSuchElementException: null
```

## Filtering to *n* items
Functions in this section filter Observable and result Observable have always *n* items.

### Take
Return Observable with first *n* emitted items of another Observable.

``` java
Observable<Integer> observable = Observable.range(1, 10);
observable.take(2).subscribe(new PrintObserver());
```

```
On subscribe.
On next: 1
On next: 2
On completed.
```

Example for empty Observable.

``` java
observable = Observable.empty();
observable.take(2).subscribe(new PrintObserver());
```

```
On subscribe.
On completed.
```

Example for Observable with error.

``` java
observable = Observable.error(new IllegalAccessException());
observable.take(2).subscribe(new PrintObserver());
``` 

```
On subscribe.
On error: IllegalAccessException: null
```

### TakeLast
Return Observable with last *n* emitted items before complete is emitted.

``` java
Observable<Integer> observable = Observable.range(1, 10);
observable.takeLast(2).subscribe(new PrintObserver());
``` 

```
On subscribe.
On next: 9
On next: 10
On completed.
```

Example for empty Observable.

``` java
observable = Observable.empty();
observable.takeLast(2).subscribe(new PrintObserver());
``` 

```
On subscribe.
On completed.
```

Example for Observable with error.

``` java
observable = Observable.error(new IllegalAccessException());
observable.takeLast(2).subscribe(new PrintObserver());
``` 

```
On subscribe.
On error: IllegalAccessException: null
```

## Filtering
Filtering functions in this section filter Observable and return Observable with the same or less count of source Observable.

### Debounce
Emit value only if no value after time span is not emitted.

``` java
Subject<Integer> observable = PublishSubject.create();
observable.onNext(1);
observable.onNext(2);
observable.debounce(100, TimeUnit.MILLISECONDS).subscribe(new PrintObserver());
observable.onNext(3);
observable.onNext(4);
Thread.sleep(110);
observable.onNext(5);
Thread.sleep(110);
observable.onNext(6);
observable.onNext(7);
observable.onComplete();
``` 

```
On subscribe.
On next: 4
On next: 5
On next: 7
On completed.
```

This example shows how *debounce* works. Returned Observable from filter function will emit only last value, because before the end of the time period is emitted some next value.

``` java
Subject<Integer> observable = PublishSubject.create();
observable.onNext(1);
observable.onNext(2);
observable.debounce(100, TimeUnit.MILLISECONDS).subscribe(new PrintObserver());
observable.onNext(3);
Thread.sleep(90);
observable.onNext(4);
observable.onNext(5);
Thread.sleep(90);
observable.onNext(6);
observable.onNext(7);
observable.onComplete();
```

```
On subscribe.
On next: 7
On completed.
```

### Distinct
Creating Observable, which will emit only items, which were not already emitted.

``` java
Observable<Integer> observable = Observable.just(1, 2, 3, 1, 4, 2, 5, 6);
observable.distinct().subscribe(new PrintObserver());
observable.distinct(integer -> integer % 3).subscribe(new PrintObserver());
``` 

```
On subscribe.
On next: 1
On next: 2
On next: 3
On next: 4
On next: 5
On next: 6
On completed.
On subscribe.
On next: 1
On next: 2
On next: 3
On completed.
```

### Filter
Filter items by predicate. If the predicate is true, it will emit the item.

``` java
Observable<Integer> observable = Observable.range(1, 10);
observable.filter(integer -> integer % 2 == 0).subscribe(new PrintObserver());
``` 

```
On subscribe.
On next: 2
On next: 4
On next: 6
On next: 8
On next: 10
On completed.
```

### IgnoreElements
Ignore all emitted items, so only complete or error will be emitted.

``` java
Observable<Integer> observable = Observable.range(1, 10);
observable.ignoreElements().subscribe(new PrintCompleteObserver());
``` 

```
On subscribe.
On completed.
```

### Sample
Filter items by time Interval, where only last item in sample period is emitted. If no value was emitted by a source Observable in sample, also no item will be emitted.

``` java
Subject<Integer> observable = PublishSubject.create();
observable.onNext(1);
observable.onNext(2);
observable.sample(100, TimeUnit.MILLISECONDS).subscribe(new PrintObserver());
observable.onNext(3);
observable.onNext(4);
Thread.sleep(110);
observable.onNext(5);
Thread.sleep(110);
observable.onNext(6);
observable.onNext(7);
observable.onComplete();
``` 

```
On subscribe.
On next: 4
On next: 5
On completed.
```

### Skip
Skip first *n* items of Observable.

``` java
Observable<Integer> observable = Observable.range(1, 10);
observable.skip(3).subscribe(new PrintObserver());
``` 

```
On subscribe.
On next: 4
On next: 5
On next: 6
On next: 7
On next: 8
On next: 9
On next: 10
On completed.
```

### SkipLast
Skip *n* emitted items before complete emit.

``` java
Observable<Integer> observable = Observable.range(1, 10);
observable.skipLast(3).subscribe(new PrintObserver());
``` 

```
On subscribe.
On next: 1
On next: 2
On next: 3
On next: 4
On next: 5
On next: 6
On next: 7
On completed.
```
