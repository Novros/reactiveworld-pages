---
layout: post
title: RxSmalltalk - implementing reactive library part 2
date: 2018-04-08 23:30:00
comments: true
excerpt: After implementing base Observable, we can implement other basic Observable types like Empty, Throw, Never and so on.
---
After implementing base Observable, we can implement other basic Observable types like Empty, Throw, Never and so on.

We can create Observable from array through creation static function. And now we want to implement other static creation methods like from Range, empty, never, throw. For more information about these functions look at this [post](https://www.reactiveworld.net/2018/03/04/Observable-creation.html) about creation methods of Observable.

## Tests
Again we will start with implementing test, which are defined by definition of creation method. Some of the tests are intuitive and straightforward.

When we start creating many tests for different types of Observables, Is good to create some method, which will test Observable in a positive way with an expected collection of values.

```Smalltalk
assertObservable: observable contains: data
	| testObserver | 

	testObserver := TestObserver new. 

	observable subscribe: testObserver. 

	self assert: testObserver isCompleted equals: true.
	self assert: testObserver isError equals: false.
	self assert: testObserver data size equals: data size.
	self assertIterables: testObserver data equals: data
```

Now we can simply test Observable by calling:

```Smalltalk
self assertObservable: observable contains: { item item2 }.
```

All tests which we will implement has the same form:
1. Create Observable.
2. Subscribe to Observable.
3. Assert TestObserver with expected values.

### Just
Just method only creates Observable from *n* items.

```Smalltalk
testJust
	| observable item|
	item := 1. 

	observable := Observable just: item.
	
	self assertObservable: observable contains: { item }.
```

### Range
Creates Observable from interval defined by two numbers.

```Smalltalk
testRange
	| observable | 
	observable := Observable range: 1 to: 5.
	
	self assertObservable: observable contains: (1 to: 5).
```

### Empty
Creates Observable with zero data.

```Smalltalk
testEmpty
	| observable |
	observable := Observable empty.

	observable subscribe: observer.
	
	self assert: (observer isCompleted) equals: true.
	self assert: (observer isError) equals: false. 
	self assert: (observer data size) equals: 0.
```

### Never
Creates Observable, which will never stop emitting zero items.

```Smalltalk
testNever
	| observable |
	observable := Observable never.

	observable subscribe: observer.
	
	self assert: (observer isCompleted) equals: false.
	self assert: (observer isError) equals: false.
	self assert: (observer data size) equals: 0.
```

### Raise
Creates Observable which only emits exception.

```Smalltalk
testRaise
	| observable exception | 

	exception := TestError new.
	observable := Observable raise: exception.
	observable subscribe: observer. 

	self assert: observer isCompleted equals: false.
	self assert: observer isError equals: true.
	self assert: observer data size equals: 0.
	self assert: observer error equals: exception
```

## Implementation
In [first part](https://www.reactiveworld.net/2018/03/11/RxSmalltalk-part01.html) we defined the basic structure of Observable and in this post we will continue implementing from this structure.

Adding another type of Observable is simple. It can described with these steps:

1. Define creation function with an appropriate name, in which will be created type of Observable.
2. Create a new subclass of Observable for this creation method, which will contain only create subscription for this type of Observable.
3. Create a new subscription for this type of Observable, which will call ```observer.onNext: item```  and ```observe.onComplete``` when is needed.

### Range
We will begin with the simpliest method. Range method is nothing else than *fromArray* with another arguments.

```Smalltalk
range: from to: to
	^self array:(from to: to).
```

### Just
Creation method *just* creates Observable from given items. We will implement only creation from one item.

#### Creation method
```Smalltalk
just: item
	^ JustObservable newItem: item.
```

#### Observable subclass
```Smalltalk
Observable subclass: #JustObservable
	instanceVariableNames: 'item'
	classVariableNames: ''
	package: 'Rx'
```

```Smalltalk
subscribe: observer
	observer onSubscribe: (JustSubscription newObserver: observer item: item)
```

#### Subscription subclass
Subscription only holds item as data and then it will emit it in request method for any count.

```Smalltalk
Subscription subclass: #JustSubscription
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Rx'
```

```Smalltalk
newObserver: aObserver item: aItem
	|subscription|
	subscription := self new.
	subscription observer: aObserver.
	subscription data: aItem.
	subscription initialize.
	^subscription .
```

```Smalltalk
request: count
	((count > 0 and: completed not) and: cancelled not)
		ifTrue: [ observer onNext: data.
			self complete ]
```

### Empty
Empty Observable will emit no items and only emit complete after subscription.

#### Creation method
```Smalltalk
empty
	^ EmptyObservable new.
```

#### Observable subclass
```Smalltalk
Observable subclass: #EmptyObservable
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Rx'
```

```Smalltalk
subscribe: observer
	observer onSubscribe: (EmptySubscription newObserver: observer)
```

#### Subscription subclass
Implementing of empty subscription is straightforward, call ```observer.onComplete``` when is observer call request.

```Smalltalk
newObserver: aObserver
	|subscription|
	subscription := self new.
	subscription observer: aObserver.
	subscription initialize.
	^subscription .
```

```Smalltalk
request: count
	"Request count items from publisher, but emit zero items."

	((count >= 0 and: completed not) and: cancelled not)
		ifTrue: [ self complete ]
```

### Never
Never Observable will emit no item or onComplete or onError ever.

#### Creation method
```Smalltalk
never
	^ NeverObservable new.
```

#### Observable subclass
```Smalltalk
Observable subclass: #NeverObservable
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Rx'
```

```Smalltalk
subscribe: observer
	observer onSubscribe: (NeverSubscription newObserver: observer)
```

#### Subscription subclass
Never subscription will do nothing in method ```request```, so we do not need to override it.

```Smalltalk
Subscription subclass: #NeverSubscription
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Rx'
```

```Smalltalk
newObserver: aObserver
	"Creates never subscription."
	
	|subscription|
	subscription := self new.
	subscription observer: aObserver.
	subscription initialize.
	^subscription.

```

### Raise
Raise Observable will holds error and emit it when observer subscribe to it.

#### Creation method
```Smalltalk
raise: error
	^ RaiseObservable newError: error
```

#### Observable subclass
```Smalltalk
Observable subclass: #RaiseObservable
	instanceVariableNames: 'error'
	classVariableNames: ''
	package: 'Rx'
```

```Smalltalk
subscribe: observer
	observer onSubscribe: (RaiseSubscription newObserver: observer error: error)
```

#### Subscription subclass
Implementation is again simple, call only ```observer.onError``` with saved error.
```Smalltalk
newObserver: aObserver error: error
	|subscription|
	subscription := self new.
	subscription observer: aObserver.
	subscription data: error.
	subscription initialize.
	^subscription .
```

```Smalltalk
accessing
request: count
	cancelled not
		ifTrue: [ observer onError: data ]
```
