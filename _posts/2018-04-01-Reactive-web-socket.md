---
layout: post
comments: true
date: 2018-04-01 23:30:00
title: Reactive WebSocket
excerpt: Few days back, I needed to use WebSockets on the client to get almost real-time communication with the backend. And I was thinking if is possible to simply connect RxJava to Java web sockets.
---

Few days back, I needed to use WebSockets on the client to get almost real-time communication with the backend. And I was thinking if is possible to simply connect RxJava to Java web sockets.

So, lets go to discover how we can achieve it. At the beginning, we will look at what is WebSocket (practise makes perfect), then implement a simple example of WebSocket and at last, connect RxJava to it and create reactive web socket.

## What is WebSocket
WebSocket is communication protocol, which is full-duplex and use one TCP connection. The url of WebSocket use prefix ```ws://``` or ```wss://``` for secured connection. Properties of WebSocket are:
- **Bi-Directional** - HTTP normally uses uni-directional style, where the request is initiated by the client and then server processes request and returns a response. On the other hand, in WebSocket there is no request/response and server or client can send messages to the other group.
- **Full-Duplex** - server and client can talk in the same time, so they are independent of each other
- **One TCP connection** - Typically, HTTP use request at initiation connection and then is terminated with the response. Websocket use HTTP upgrade mechanism.
- **Stateful protocol** - WebSocket remember server or client session. On the other hand, HTTP is a stateless protocol.
- **Is fast** - There is no need for HTTP header overhead or TCP handshake.
- **Low-level protocol**
- Security with WebSocket is all-or-nothing approach
- not all proxies, DNS, firewalls are fully aware of WebSocket traffic

So, WebSocket is very useful for very frequent sending of small messages. The best use case is a simple chat application, where all clients connect to server (chat room) and all messages received from client is broadcasted by the server to all connected clients.

## WebSocket example
In this section we will look how to simply implement example of websocket implementation. Firstly, we need dependencies which contains needed annotations.

``` xml
<dependency>
	<groupId>javax.websocket</groupId>
	<artifactId>javax.websocket-api</artifactId>
	<version>LATEST</version>
</dependency>
```

### Websocket client
Then we will implement client web socket, which only implement method for receiving data from server. The class must be annotated with ```@ClientEndpoint``` and method for receiving data must be annotated with ```@OnMessage``` annotation. 

``` java
@ClientEndpoint
public class SimpleClientSocket {
	@OnMessage
	public void onMessage(final String message) {
		System.out.println("On message: " + message);
	}
}
```

### Websocket server
In Implementing server web socket, we must be again annotated the class with ```@ServerEndpoint``` with a defined endpoint path. Then you can/must define methods with annotations:
- ```@OnOpen``` - For reacting event, when client connects to the server.
- ```@OnMessage``` - For reacting, when a client sends message to server.
- ```@OnError``` - For reacting, when during communication with the client occurs error.
- ```@OnClose``` - For reacting when client disconnects from server.

``` java
@ServerEndpoint(value = "/simple")
public class SimpleServerSocket {
	@OnOpen
	public void onOpen(final Session session) {
		printMessage(session, "On open...");
	}

	@OnMessage
	public void onMessage(final Session session, final String message) throws IOException {
		printMessage(session, "On message: " + message);
		session.getBasicRemote().sendText("Thanks for message...");
	}

	@OnError
	public void onError(final Session session, final Throwable t) {
		printMessage(session, "On error: " + t.getMessage());
	}

	@OnClose
	public void onClose(final Session session) {
		printMessage(session, "On close...");
	}

	private static void printMessage(final Session session, final String message) {
		System.out.println("[" + new Date() + "][" + session.getId() + "][" + SimpleServerSocket.class.getCanonicalName() + "] " + message);
	}
}
```

## ReactiveStream network protocol
If you look on specification about [Reactive Streams network protocols](https://github.com/reactive-streams/reactive-streams-io/), you see there is no actual specification, but only some discussion on draft specifications. So, we will not bind with any specification.

## Reactive WebSockets
Our goal will be creates web socket client and web socket server, which will provide an interface of Observable for observing received messages and also be Observed for sending messages to client or server.

In case of server, we will simplify it by sending messages to all connected clients (broadcast).

### Observable WebSocket
For implementing an Observable interface (support for reactive functions, subscribing) we need only simply extends Observable class and implement abstract method ```subscribeActual (Observer) ```, which must creates disposal and pass it to Observer throughout unsubscribe (Disposable) ``` method.

``` java
@Override
protected void subscribeActual(final Observer<? super T> observer) {
	observers.add(observer);
	observer.onSubscribe(new WebSocketDisposable(observer));
}
```

An implementation of Disposable interface:

``` java
private final class WebSocketDisposable implements Disposable {

	private final Observer observer;

	private WebSocketDisposable(final Observer observer) {
		this.observer = observer;
	}

	@Override
	public void dispose() {
		observers.remove(observer);
	}

	@Override
	public boolean isDisposed() {
		return !observers.contains(observer);
	}
}
```

This extension we will add to both client and server web socket. But with only one different thing about the client, where we add trying to connect server web socket if not connected.

``` java
private void connect() {
	try {
		if (session.get() == null) {
			session.set(webSocketContainer.connectToServer(clientSocket, uri));
		}
	} catch (DeploymentException | IOException ignored) {
	}
}
```

 The last thing what we must add is calling the method ```observer.onNext (Object)``` in message method.

Client part:
``` java
@OnMessage
public void onMessage(final String message) {
	final T entity = gson.fromJson(message, tClass);
	observers.forEach(observer -> observer.onNext(entity));
}
```
And server part:

``` java
public void onMessage(final Session session, final String message) {
	final T entity = gson.fromJson(message, tClass);
	observers.forEach(observer -> observer.onNext(entity));
	onMessage(session, entity);
}
```

### Observer WebSocket
An implementing Observer ability we need only implement interface ```Observer<T>``` with methods ```onNext(Object)```, ```onSubscribe(Disposable)```, ```onError(Throwable)``` and ```onComplete()```. Methods onError and onComplete will be empty.

Method to Subscribe only store disposable into collection.

``` java
@Override
public void onSubscribe(final Disposable disposable) {
	disposables.add(disposable);
}
```

And method onNext sends data to server or to clients.

``` java
public void onNext(final T t) {
	try {
		if (session.get() != null) {
			session.get().getBasicRemote().sendText(gson.toJson(t));
		}
	} catch (final IOException ignored) {
	}
}
```

Server implementation:

``` java
public void onNext(final T entity) {
	broadcast(entity);
}

public void broadcast(final T entity) {
	clientSessions.forEach(session -> sentEntity(entity, session));
}
```

``` java
protected void sentEntity(final T entity, final Session session) {
	try {
		session.getBasicRemote().sendText(gson.toJson(entity));
	} catch (final IOException e) {
	}
}
```

## Usage of reactive WebSockets
Usage of our implementation is simple. For using client, we only create instances  with the URL and class. And for the server we create a subclass, where we define class and we can override methods.

### Client Usage

``` java
// Create observable for sending data to server socket.
final Subject<Entity> data = PublishSubject.create();

// Set up client socket to connect
final String uri = "ws://127.0.0.1:58080/reactive";
final ReactiveClientWebSocket<Entity> webSocket = new ReactiveClientWebSocket<Entity>(uri, Entity.class);

// Subscribe to send data to server socket
data.subscribe(webSocket);

// Subscribe to print data from server socket.
webSocket
    .map(Entity::getNumber)
    .subscribe(new PrintConsumer());

// Send some data
data.onNext(new Entity("Client 4", 4));
data.onNext(new Entity("Client 3", 3));

// Wait for user to end.
System.in.read();

// Cleanup
webSocket.close();
```

### Server usage
Only extend ```ReactiveServerWebSocket``` and annotate it with ```@ServerEndpoint```.

``` java
// Create server web socket endpoint
final ReactiveServerWebSocket serverSocket = ...;

// Create observable for sending data to client sockets.
final Subject<Entity> data = PublishSubject.create();

// Subscribe to send data to client socket. It will broadcast recieved emitted entity to all connected clients.
// To change this behaviour, override onNext method.
data.subscribe(serverSocket);

// Subscribe to print data from client sockets.
serverSocket
    .map(Entity::getMsg)
    .subscribe(new PrintConsumer());
```

## Source codes
- actual state of reactive web sockets are in this [repository](https://github.com/Novros/SimpleRxWebSocket)
- example of usage of web sockets are in this [repository](https://github.com/Novros/websockets-example)
