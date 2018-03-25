---
layout: post
title: RxSmalltalk - implementation - part 1
date: 2018-03-11 23:30:00
comments: true
excerpt: Let's try to implement reactive extension in Smalltalk. For inspiration, we will use ReactiveX. In first part of this series, we will implement basic reactive object called Observable with one creation method.
---

Let's try to implement reactive extension in Smalltalk. For inspiration, we will use ReactiveX. In first part of this series, we will implement basic reactive object called Observable with one creation method.

## Observable
Full description of Observable is in this [post](https://www.reactiveworld.net/2018/02/25/Observable-types.html). In simple words, it is something what emits a sequence of values.

### Basic creation methods
We will start with implementing creation methods, because at the begin we need have the way to create observable. Basic creation methods are: *fromArray, just, range, empty, never*. For their description with examples, see [this post](https://www.reactiveworld.net/2018/03/04/Observable-creation.html) about creation functions of Observables.

## Test method
At the begging we need test method as use cases for creating Observable. So, positive test for array creation method is:

``` smalltalk
testArray
        "Test method to create observable from array."
        | observable observer|
        observable := Observable array: #( 'Nice' 5 'Ok' ).
        observer := TestObserver new.

        observable subscribe: observer.

        self assert: (observer data size) equals: 3.
        self assert: (observer dataAt: 1) equals: 'Nice'.
        self assert: (observer dataAt: 2) equals: 5.
        self assert: (observer dataAt: 3) equals: 'Ok'.
        self assert: (observer isCompleted).
```

### TestObserver
TestObserver is class, which will observe our Observable and saves emitted items, which cane asserted later. From definition of Observer in reactive programming, observer must implement three basic methods: ```onNext: item```, ```onComplete``` and ```onError: error```.

#### Class definition
Emitted items We will be storing in collection and complete and error will be boolean flag. So, the definition of class is:

``` smalltalk
Object subclass: #TestObserver
	instanceVariableNames: 'data isCompleted isError'
	classVariableNames: ''
	package: 'Rx-Tests'
```

#### Methods implementation
Implementing Observer methods are straightforward. Set a boolean flag if completed or error, add items into collection in the onNext method. Of course getters should be mplemented also, but they are not shown.

``` smalltalk
initialize
        data := OrderedCollection new.
        isCompleted  := false.
```
``` smalltalk
onComplete
        isCompleted := true.
```
``` smalltalk
onNext: item
        data add: item
```
``` smalltalk
onError: exception
	isError  := true.
```

## Naive implementation
Now, we have implemented a simple test with TestObserver, so as next step, we can start implementing Observable. In the begging, let's start with some naive implementation based on [Observer](https://www.reactiveworld.net/2018/02/11/Observer-design-pattern.html) design pattern.

### Class definition
We will need only one attribute, in which will be all data stored. That is mandatory to emit items multiple times, because subscribe method can be called many times.

``` smalltalk
Object subclass: #Observable
	instanceVariableNames: 'data'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Rx'
```

### Class methods
Our main method for creating Observable with array as argument, stores array to data attribute as a collection.

``` smalltalk
"Class method"
array: array
        "Create observable from array"
        | observable |
        observable := self new.
       data: OrderedCollection new
        array do: [ :item | observable data add: item ].
        ^ observable.
```

### Instance methods
Now, we need to implement methods to notify observer about a change of our observable state. Again, implementation is straightforward.

``` smalltalk
notify: item to: observer
	"Notify all observer about change in state."
	observer onNext: item
```
``` smalltalk
notifyCompleteTo: observer
	"Notify observer about completed observable."
	observer onComplete
```
``` smalltalk
notifyError: error to: observer
	"Notify observer about error."
	observer onError: error
```

And as last method, which we need to implement is subscribing Observer to Observable.

``` smalltalk
subscribe: observer
	"Subscribe observer to this observable."
	data do: [ :item | self notify: item to: observer ].
	self notifyCompleteTo: observer.
```

Now run the test ... green. Nice, it works. But if we think about implementing some additional features of reactive programming (reactive functions, other Observable types) and when we look at the definition of Observer in Reactive Programming and also on ReactiveX implementation of Observable, we see that Reactive programming uses extended Observer design pattern with subscription (we can unsubscribe) desing pattern. And also implementing of a new type of Observable must costs only implementing one or two classes.

Our actual state of codes can be found in repository of RxSmalltalk under this [commit](https://github.com/Novros/RxSmalltalk/tree/4b07cc973037cd0218e7b140e58661596c80f3f1).

## Robust implementation
After implementing naive solution, we discovered that we need more robust implementation, which will be more in OOP style and give us flexible hierarchy to simply create new type of Observable. So, the hierarchy of classes shoudl be:

1. Class Observable will be an abstract class for all Observables. With "abstract" method  subscribe, which will return the subscription.
2. Class Subscription will holds data and will know how to emit each item of data to Observers.
3. Creating class ArrayObservable, will create and return ArraySubscription in subscribe observer method.
4. Class ArraySubscription will holds data as a collection and will know how to iterate the collection of data and how to emit data.
5. Class Observer will be abstract class and have a default implementation of common methods of Observer.

Sequence of calling methods will be:
1. Observer.fromArray: array -> ArrayObservable
2. ArrayObservable.subscribe: observer -> ArraySubscription
3. ArraySubscription -> Observer.onSubscribe
4. Observer.onSubscribe -> Subscription.request: n.

### Observable
Observable must be changed to more abstract type. It will be our facade over Observable child classes to hide implementation hierarchy. So, it will only create instances of child classes of Observables through class methods (Observable creation methods). Implementation of this is simple, removing all previous code from Observable and implement only one method, which will create our ArrayObservable with passed array as arguments.

#### Class definition
``` smalltalk
Object subclass: #Observable
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Rx'
```

#### Create method for array
``` smalltalk
"Class method"
array: array
	"Create observable from array"
	^ ArrayObservable newFromArray: array
```

### ArrayObservable
After that we need to implement a subclass of Observable for arrays. It will be called ArrayObservable and it will be creating ArraySubscription for Observers. So, we need to store array into some instance attribute in the constructor and implement subscribe method, which will return ArraySubscription with stored array.

#### Class definition
``` smalltalk
Observable subclass: #ArrayObservable
	instanceVariableNames: 'collection'
	classVariableNames: ''
	package: 'Rx'
```

#### Constructor
``` smalltalk
newFromArray: aCollection
	"Create observable from collection."

	| observable |
	observable := self new.
	observable collection: aCollection.
	^ observable
```

#### Subscribe method
Subscribe method will call only observer's onSubscribe with instance of ArraySubsciption.

``` smalltalk
subscribe: observer
	"Subscribe observer to observable by subscription."
	observer onSubscribe: (ArraySubscription newObserver: observer data: collection)
```

### Subscription
Subscription is bridge between Observable and Observer, which is created by Observable and used by the Observer. So, it must hold the data (which will be emitted), observer reference (which is owner of subscription) and also it must provide the option to be cancelled during emitting.

#### Class definition
We will define abstract class for all subscriptions, which will implement common (simple) methods and child classes will now what is stored in data and how it must be emitted.

``` smalltalk
Object subclass: #Subscription
	instanceVariableNames: 'observer data cancelled completed'
	classVariableNames: ''
	package: 'Rx'
```

#### Common methods
Implementing methods ```initialize```, ```cancel``` and ```complete``` methods are straightforward.

``` smalltalk
initialize
	cancelled := false.
	completed := false.
```
``` smalltalk
cancel
	"Request observable to stop sending data."
	cancelled := true.
```
``` smalltalk
complete
	"Sent notification about completed observable."
	completed := true.
	observer onComplete
```

### ArraySubscription
Now we will create a subclass of Subscription for arrays, which will iterate over data and emit each item from the array.

#### Class definition
``` smalltalk
Subscription subclass: #ArraySubscription
	instanceVariableNames: 'actualIndex'
	classVariableNames: ''
	package: 'Rx'
```

#### Constructor
The constructor will only create instance with data and prepare everything for emitting data. Data will be emitted when Observer will request it.

``` smalltalk
newObserver: aObserver data: aData
	"Create subscribtion from observer and data."
	
	|subscription|
	subscription := self new.
	subscription observer: aObserver.
	subscription data: aData.
	subscription initialize.
	^subscription .
```
``` smalltalk
initialize
	super initialize.
	actualIndex := 1
```

#### Request method
The main part of all Subscriptions is method ```request: count```, which will emit *count* items to the observer.

Method request should emit *count* items by calling ```onNext: item``` of Observer, only if the subscription is not completed or cancelled and catch any exception during emitting and emit erro to observer throught ```onError: error``` method. Implementation of the method is below:

``` smalltalk
request: count
	"Request count items from publisher"
	[ self requestIndexes: (actualIndex to: (self maxIndex: count)).
	actualIndex >= data size
		ifTrue: [ self complete ] ]
		on: Exception
		do: [ :exception | observer onError: exception ]
```

*MaxIndex* method return maximal index, which can be requested on data.

``` smalltalk
maxIndex: count
	"Return maximal index for request."
	| indexWithCount |
	indexWithCount := actualIndex + count.
	indexWithCount <= data size
		ifTrue: [ ^ indexWithCount ]
		ifFalse: [ ^ data size ]
```

Method requestIndexes will create a range of indexes and check if the item can be now emitted (if subscription is not cancelled or completed) and if can be, it will call ```onNext: item``` of the Observer.

``` smalltalk
requestIndexes: range
	range
		do: [ :index | 
			(completed or: cancelled)
				ifTrue: [ ^ self ]
				ifFalse: [ observer onNext: (self requestItem: index) ]. ]
```

RequestItem is only wrapping of method collection method ```at: index``` with storing actual index to attribute *actualIndex*.

``` smalltalk
requestItem: index
	"Return item on index."
	actualIndex := index.
	^ data at: index
```

### Observer
And as last class, which we need to implement is a common class for Observers. It will holds achieved subscription, provides default implementation of ```cancel``` and ```onSubscription: subscription``` method. And other methods like ```onNext: item```, ```onError: error``` and ```onComplete``` should be implemented by subclass.

#### Class definition
``` smalltalk
Object subclass: #Observer
	instanceVariableNames: 'subscription'
	classVariableNames: ''
	package: 'Rx'
```

#### Default methods
``` smalltalk
cancel
	"Cancel subscription."
	subscription cancel.
	subscription := nil
```

Method ```onSubscribe``` will store subscription and will request some big number of items on subscription.

``` smalltalk
onSubscribe: aSubscription
	"Invoked after calling subscribe to observable."
	subscription := aSubscription.
	self request: 100000.
```
``` smalltalk
request: count
	"Request count of items from subscription."
	subscription isNotNil ifTrue: [ subscription request: count ]
```

## Conclusion
As you can see by running test, we have implemented working creation method of Observable for arrays, which will be also the base for another Observable types(from range, never, empty and so on). This robust implementation gives us a more flexible implementation and nice OOP hierarchy. In the next part, we will implement another basic Observables of Reactive programming.

Actual state of RxSmalltalk can be found in [RxSmalltalk repository](https://github.com/Novros/RxSmalltalk).`
