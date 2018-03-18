---
layout: post
title: Observable - transformation functions
date: 2018-03-18 23:30:00
comments: true
excerpt: Transformation functions change emitted items of Observable into another type, which is emitted.
---

Transformation functions change emitted items of Observable into another type, which is emitted. In other words, transformation creates Observable wrapper around Observable. The wrapper is subscribed to that observable to catch all emitted items, and then the wrapper transform items into specified object and emit them to wrapper subscribers.

## Grouping functions
Functions in this section are grouping emitted items and then they will emit them as one item.

### Buffer
Buffer function will gather *n* items, group them and then emit group instead of emitting each item. When an error occurs during function, it will immediately stop function and emit error. Emitted group is usually an array or a collection of items.

#### Example
``` java
final Observable<Integer> observable = Observable.range(0, 10);
observable.buffer(2, 4).subscribe(new PrintObserver());
```
```
On subscribe.
On next: [0, 1]
On next: [4, 5]
On next: [8, 9]
On completed.
```

### Window
Window function is similar to buffer function, but instead of emitting groupie items, it will emit new Observable and emit *n* items into that observable and then emit a complete event. When an error occurs during emitting, it will be propagated to emitted Observable and stops observable to emit another items.

#### Example
``` java
final Observable<Integer> observable = Observable.range(0, 10);
final Observer<Object> observer = new PrintObserver();
observable
	.window(2, 4)
	.subscribe(integerObservable -> integerObservable.subscribe(observer));
```
```
On subscribe.
On next: 0
On next: 1
On completed.
On subscribe.
On next: 4
On next: 5
On completed.
On subscribe.
On next: 8
On next: 9
On completed.
```

### GroupBy
This function will group items into groups by key returned from discriminating function. For each group defined by the same key, it will emit new Observable. Emitted observable ends, when the source Observable ends.

#### Example
``` java
final Observable<Integer> observable = Observable.range(0, 10);
observable
	.groupBy(integer -> integer % 3)
	.subscribe(grouped -> {
		System.out.println("Accepting group: " + grouped.getKey());
		grouped.subscribe(new PrintObserver(grouped.getKey().toString()));
	});
```
```
Accepting group: 0
[0] On subscribe.
[0] On next: 0
Accepting group: 1
[1] On subscribe.
[1] On next: 1
Accepting group: 2
[2] On subscribe.
[2] On next: 2
[0] On next: 3
[1] On next: 4
[2] On next: 5
[0] On next: 6
[1] On next: 7
[2] On next: 8
[0] On next: 9
[0] On completed.
[1] On completed.
[2] On completed.
```

## Mapping functions
Mapping functions transform emitted item into another item, which is emitted.

### Map
This function map emitted item into another item, by applying function, where argument is emitted item and returned value is an item, which will be emitted.

#### Example
``` java
final Observable<Integer> observable = Observable.range(0, 5);
observable
	.map(integer -> integer.doubleValue() / 100)
	.subscribe(new PrintObserver());
```
```
On subscribe.
On next: 0.0
On next: 0.01
On next: 0.02
On next: 0.03
On next: 0.04
On completed.
```

### FlatMap
FlatMap is similar to map function, but as a result of applied function is here Observable, which is flattened and merged with other created Observable by applying the function. Order of emitted items can be interleaved, but there exist variations of flatMap, where the result is ordered by emitted items of source Observable.

#### Example
``` java
final Observable<Integer> observable = Observable.range(0, 5);
observable
	.flatMap((Function<Integer, ObservableSource<?>>) integer ->
		Observable.range(0, integer))
	.subscribe(new PrintObserver());
```
```
On subscribe.
On next: 0
On next: 0
On next: 1
On next: 0
On next: 1
On next: 2
On next: 0
On next: 1
On next: 2
On next: 3
On completed.
```

### Scan
Scan function is almost same as map function. The difference between them is that: scan function applies function sequentially and use a function with two parameters, when first is last returned value (on the first run is null or specified item) and second emitted value from Observable.

#### Example
``` java
final Observable<Integer> observable = Observable.range(0, 5);
observable.scan(10, (integer, integer2) -> integer + integer2)
	.subscribe(new PrintObserver());
```
```
On subscribe.
On next: 10
On next: 10
On next: 11
On next: 13
On next: 16
On next: 20
On completed.
```
