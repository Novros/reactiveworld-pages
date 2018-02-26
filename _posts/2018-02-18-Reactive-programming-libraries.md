---
layout: post
title: Reactive programming - libraries
date: 2018-02-18 23:30:00
comments: true
excerpt: When we know what is Reactive programming and how it can be implemented, it is time to look on some done implementations. So, we will look at some of them.
---
When we know what is Reactive programming and how it can be implemented, it is time to look on some done implementations. So, we will look at some of them.

Because I am Java programmer, this list will be oriented on technologies based on Java or javascript. But if you find some implementation (which has a big impact on your language) writes about it in the comments and I will look at it.

## Reactive streams
This is not a library or framework, but also one important thing, which becoming to be standard for reactive extensions and frameworks, which want to support reactive streams. Currently it aims to JVM, JavaScript and network protocols, and it is the cornerstone of every modern Reactive library and must have for interoperability purpose.

The ambition of Reactive streams is to define a minimal set of interfaces, methods and protocols that will describe necessary operations and entities for reactive streams. Also create a bridge between various implementations of Reactive streams, but should not used to provide end-user api.

The state of this is specification is:
- defined JVM reactive streams, which Are included in Java 9 in thé java.util.concurrent.Flow container class - [reactive-streams-jvm repository](https://github.com/reactive-streams/reactive-streams-jvm/)
- defined minimal set of object properties for observing stream of elements in JavaScript - [reactive-streams-js repository](https://github.com/reactive-streams/reactive-streams-js/)
- defined network protocols for passing reactive streams over various transport  media (TCP, UDP, HTTP, ...) - [reactive-streams-io repository](https://github.com/reactive-streams/reactive-streams-io/)

More information about Reactive Streams on their [website](http://www.reactive-streams.org/)

## ReactiveX
![ReactiveX logo]({{ site.url }}/assets/images/2018-02-18-Reactive_programming-libraries/reactivex.png){:style="float: left; margin-right: 12px; margin-top: 7px; height: 128px"}
ReactiveX is the main library for those, who want to start with reactive programming. It has many implementations across many languages. And the most benefit is that, they are trying to keep same api in all languages. The definition of what ReactiveX is from their website:

*"ReactiveX is a library for composing asynchronous and event-based programs by using observable sequences."*

Also, they have very good ipdocumentation and introduction to Reactive Programming on their [website](http://reactivex.io/intro.html).

## Vert.x
![Vert.x logo]({{ site.url }}/assets/images/2018-02-18-Reactive_programming-libraries/vert.x.png){:style="float: left; margin-right: 12px; margin-top: 7px; height: 64px"}
Small, fast, event driven and non blocking toolkit for building reactive systems. The toolkit can be used in multiple languages for example Java, Javascript, Ruby, Scala. It provides a couple components for reactive programming. Also, it supports Reactive Streams so it can interoperate with other reactive implementations. Vert. x provides its own mechanism for handling streams of data and pumping them with backpressure. And provides Rx-ified version of the api, so it can be easily used with ReactiveX together.

Information about toolkitVert.x is [here](http://vertx.io/) and [part](http://vertx.io/docs/#reactive) about reactive support.

## Bacon.js
Small reactive programming library for Javascript. [Website](https://baconjs.github.io)

## Kefir.js
![Kefir.js logo]({{ site.url }}/assets/images/2018-02-18-Reactive_programming-libraries/kefir.js.png){:style="float: left; margin-right: 12px; margin-top: 7px; height: 64px"}
A reactive programming library for JavaScript, which is inspired by Bacon.js and ReactiveX with focus on low memory usage and high performance. The library supports streaming and properties, where streams are events made available over time and properties are value change over time.

Website for library is [here](http://kefirjs.github.io/kefir/).

## Spring Reactive streams
![Spring logo]({{ site.url }}/assets/images/2018-02-18-Reactive_programming-libraries/spring.png){:style="float: left; margin-right: 12px; margin-top: 7px; height: 128px"}
Spring framework today also support reactive. It uses a project reactor to achieve this, also has another terminology for Reactive programming then ReactiveX, for example Flux is Observable and Mono is Single or Maybe. But Spring also fully supports use of RxJava. If you want to you use Spring reactive streams it is in spring-webflux module, which support reactive HTTP and WebSocket clients. Where request expose the body as Flux or Mono and the response expose as any Reactive Streams publisher as body

Documentation for spring reactive atreams is [here](https://docs.spring.io/spring/docs/5.0.0.BUILD-SNAPSHOT/spring-framework-reference/html/web-reactive.html)

## Project Reactor
![Project reactor logo]({{ site.url }}/assets/images/2018-02-18-Reactive_programming-libraries/reactor.png){:style="float: left; margin-right: 12px; margin-top: 7px; height: 128px"}
*"Non-Blocking Reactive Foundation for the JVM"*

Another library builds on the Reactive Streams specification and it is organized into multiple projects: reactor-core, spring and RxJava (ReactiveX). It is well suited for creating Microservices Architecture and also offers backpressure-ready network engines.

More information is available on [website](https://projectreactor.io/).

## Akka streams
![Akka logo]({{ site.url }}/assets/images/2018-02-18-Reactive_programming-libraries/akka.png){:style="float: left; margin-right: 12px; margin-top: 7px; height: 128px"}
Akka is toolkit for building message-driven applications for Java and Scala. The main object in Akka is an Actor, which is something, what receives and sends messages. And  can be seen as something what is dealing with a stream of messages. So, the creators of Akka decided to create Akka Streams API with support of Reactive Streams, which Akka is also founding member. But one important thing, Akka Streams API is completely decoupled from the Reactive Streams interfaces, because Reactive Streams interfaces should not be geared towards ent-users as API. Also Akka Streams have own terminology on reactive programming, for example chain of reactive functions is called graphs.

Documentation of Akka stream is [here](http://doc.akka.io/docs/akka/current/stream/stream-introduction.html) (introduction), their [stream design](http://doc.akka.io/docs/akka/current/general/stream/stream-design.html) and [some basic](http://doc.akka.io/docs/akka/current/stream/stream-flows-and-basics.html)

## Conclusion
As we can see, today, many toolkits and frameworks has included support for Reactive Streams. So, they can be used in reactive programming, with other terminology, but same meaning.
