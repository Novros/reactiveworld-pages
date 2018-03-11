---
layout: post
title: RxSmalltalk - implementing reactive library - part 1
date: 2018-03-11 23:30:00
comments: true
excerpt: Let's try to implement reactive extension in Smalltalk. As inspiration, we will use ReactiveX. In this part of the series, we will implement basic reactive object called Observable with one creation method.
---

Let's try to implement reactive extension in Smalltalk. As inspiration, we will use ReactiveX. In this part of the series, we will implement basic reactive object called Observable with one creation method.

## Observable
Description of Observable is in this [post](https://www.reactiveworld.net/2018/02/25/Observable-types.html). In simple words, it is something what emits a sequence of values.

### Basic creation methods
We will start with implementing creation methods, because at the begin we need have the way to create observable. Basic creation methods are: *fromArray, just, range, empty, never*. From their description with example, see [this post](https://www.reactiveworld.net/2018/03/04/Observable-creation.html) about static functions.

## Tests
At the begging we need some tests as use cases for Observable. So, positive test for array creation method:

### Array
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
We need a class which will observe our Observable and saves emitted items to assert later. The observer must implement three basic methods: ```onNext: item```, ```onComplete``` and ```onError: error```.

#### Class definition
We will be storing emitted items into collection and complete and error will be boolean flag. So, the definition of class will be:

``` smalltalk
Object subclass: #TestObserver
	instanceVariableNames: 'data isCompleted isError'
	classVariableNames: ''
	package: 'Rx-Tests'
```

#### Methods implementation
Implementing Observer methods are straightforward. Set a boolean flag if completed or error, add items into collection in the onNext method. Of course getters should be also, but they are not shown.

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
Now, when we have a simple test with TestObserver, we can start implementing Observable. So, lets start with some naive implementation based on [Observer](https://www.reactiveworld.net/2018/02/11/Observer-design-pattern.html) design pattern.

### Class definition
We need only attribute when all data be stored, because subscribe method can be called many times.

``` smalltalk
Object subclass: #Observable
	instanceVariableNames: 'data'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Rx'
```

### Class methods
Our main method for creating Observable with array stored in data attribute as collection.

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
Now, we need to implement methods to notify observer about a change of out observable state. Again implementation is straightforward.

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

And last method which we need to implement is subscribing observer to observable.

``` smalltalk
subscribe: observer
	"Subscribe observer to this observable."
	data do: [ :item | self notify: item to: observer ].
	self notifyCompleteTo: observer.
```

Now run the test... green. Nice, it works. But if we think about implementing some additional features (reactive functions, other Observable types) and when we look at the definition of Observer in Reactive Programming and also on ReactiveX implementation, we see that Reactive programming uses extended Observer design pattern with subscription (we can unsubscribe). And also implementing of a new type of Observable must cost only implementing one or two classes.

Our actual state of codes can be found on repository under [commit](https://github.com/Novros/RxSmalltalk/tree/4b07cc973037cd0218e7b140e58661596c80f3f1).

## Robust implementation
We need more robust implementation, which will in more in OOP style and give us flexible hierarchy to simply create next Observable. The hierarchy will be:

1. Class Observable will be an abstract class for all Observables. With "abstract" method  subscribe, which will return the subscription class.
2. Class Subscription class will holds data and will know how to emit them to Observers.
3. Creating class ArrayObservable which will be creating and return ArraySubscription in subscribe: observer method.
4. Class ArraySubscription will hold data as a collection will know how to iterate the collection and how to emit data in it.
5. Class Observer will be abstract class and have a default implementation of common methods.

Sequence of calling methods will be:
1. Observer.fromArray: array -> ArrayObservable
2. ArrayObservable.subscribe: observer -> ArraySubscription
3. ArraySubscription -> Observer.onSubscribe
4. Observer.onSubscribe -> Subscription.request: n.

### Observable
Observable must be changed to more abstract type. It will be our facade over Observable child classes to hide implementation hierarchy. So, it will only create instances of child classes of Observables through class methods (Observable creation methods). Implementation of this is simple, removing all previous code from Observable and implement only one method, which will create our ArrayObservable with given array.

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
And now we will need to implement a subclass of Observable for arrays. It will be called ArrayObservable and it will be creating ArraySubscription for Observers. So, we need to store array in some instance attribute in the constructor and implement subscribe method, which will return ArraySubscription with stored array.

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
Subscription is bridge between Observable and Observer, which is created by Observable and used by the Observer. So, it must hold the data (which will be emitted), observer (which is owner of subscription) and also it must provide the option to be cancelled during emitting.

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
The constructor will create instance with data and prepare everything for emitting data. Data will be emitted when Observer will request it.

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

Method request should emit *count* items by calling ```onNext: item``` of Observer, only if the subscription is not completed or cancelled and catch any exception during emitting and emit to observer to ```onError: error``` method. Implementation of the method is below:

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
And as last class we need to implement a common class for Observers. It will holds achieved subscription, provided default implementation of ```cancel``` and ```onSubscription: subscription``` method. Other methods like ```onNext: item```, ```onError: error``` and ```onComplete``` should be implemented by subclass.

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
As you can see by running test, we have implemented working Observable for arrays, which will be the base for another Observables (from range, never, empty and so on). This robust implementation gives us a more flexible implementation and nice OOP hierarchy. In the next part, we will implement another basic Observables of Reactive programming.

Actual state of RxSmalltalk can be found in [RxSmalltalk repository](https://github.com/Novros/RxSmalltalk).`
