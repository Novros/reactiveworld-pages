---
layout: post
comments: true
date: 2018-04-22 23:30:00
title: Java stream api
excerpt: In Java 8 were introduced stream api, which is similar to Reactive Programming streams. So, let's look on this interface with some examples.
---
In Java 8 were introduced stream api, which is similar to Reactive Programming streams. So, let's look on this interface with some examples.

## What is stream
As we know Observable stream is a sequence of objects with complete and error event. In Java stream API, stream same as Observable, but without events. It also supports some aggregate and filtering operations, too. Source for creating streams are collections, arrays or I/O resources, where stream does the iterations internally over the source's elements.

### Differences against collections
- has no storage - stream is not data structure, it iterates over source elements
- is functional in a nature - stream never ever modify the source
- is lazy - many of stream operations are implemented lazily
- unbounded - has no finite size

### Creation of streams
Java API Stream can be created:
- from Collection - stream() and parallelStream() method
- from array - Arrays.stream(Object[])
- from static factory methods on stream classes - of(Object[]), range(int, int), Random.ints(), ...

## Stream operations
Stream operations are divided into two types: intermediate and terminal operations. Default sequence of operation is: source, *0... n* intermediate operations and terminal operation. This sequence is also called pipeline of stream operations.

### Intermediate operation
These operations return a new stream and are always lazy, so executing intermediate operation does not actually perform any action, but creates new streams that, when is traversed that operation is applied to each element. 

### Terminal operation
Terminal operations traverse the elements of stream and is considered as consumed (stream can no longer be used). If we want the traverse same data source again, we must create a new stream. All terminal operations are eager.

## Parallelism
All of stream operations can be executed in serial or in parallel way, but parallel must be explicitly requested. That is defined at creation of the stream via method parallel. 

Few recommendations for using or writing operations:
- **we must prevent interference with data source** during execution of stream pipeline (data source should not be modified)
- **behavioral parameters (lambda functions) must be stateless** or access to the state must be synchronized
- **no side-effects in behavioral parameters**, because there is a chance that these changes cannot be visible in other threads
- may be **unordered** - streams may or may not have defined order, because the order of iteration over items is defined in source, also some terminal operations may ignore encounter order

## Examples
In this section we look on some simple examples.

### Some creation examples

Creation from array.
``` java
final int[] array = new int[]{1, 2, 3, 4, 5};
Arrays.stream(array)
		.forEach(value -> System.out.println("value = " + value));
```

```
value = 1
value = 2
value = 3
value = 4
value = 5
```

Creation from collection.
``` java
final Collection<Integer> collection = new ArrayList<>();
collection.add(12);
collection.add(23);
collection.add(3);
collection.stream()
		.forEach(value -> System.out.println("value = " + value));
```

```
value = 1
value = 2
value = 3
```

Creation from random ints.
``` java
new Random().ints()
		.limit(5) // Must be limited, or it will be infinite
		.forEach(value -> System.out.println("value = " + value));
```

```
value = -1809420586
value = -1914114084
value = -269904304
value = -135297415
value = 1979327176
```

### Pipeline example
``` java
IntStream.range(0, 5)
		.map(operand -> operand + 3)
		.filter(value -> value % 2 == 0)
		.limit(2)
		.forEachOrdered(value -> System.out.println("value = " + value));
```

```
value = 4
value = 6
```

### Reduction example
``` java
final int value =	IntStream.range(0, 100).sum();
System.out.println("value = " + value);
```

```
value = 4950
```
