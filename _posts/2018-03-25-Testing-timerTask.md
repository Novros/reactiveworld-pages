---
layout: post
date: 2018-03-25 23:30:00
title: How to test TimerTask
comments: true
except: Image situation, that you have some task, which must be run after specified time. In our case we have invalidated login token after some time, after successful login. So, lets go to implement it.
---
Image situation, that you have some task, which must be run after specified time. In our case we have invalidated login token after some time, after successful login. So, lets go to implement it.

## Implementation
Login tokens are stored in ConcurrentHashMap and invalidating will be after some time defined in class constant. For implementation timed task we will use Java Timer. Clearing procedure will be stored in TimerTask and then scheduled by Timer with delay time. The source code is shown below:

```java
public String refreshLoginOfClient(@Nonnull final String refreshToken) throws LoginFailedException {
	... // Checking of refresh token and so on.
	... // Generate new access token

	// Invalidate access token after delay
	timer.schedule(new TimerTask() {
		@Override
		public void run() {
			clientsRefreshTokens.remove(accessToken);
		}
	}, ApiConstants.ACCESS_TOKEN_INVALIDATION_DELAY);

	return accessToken;
}
```

Then I figured out, how it can be tested. We can not use sleep in tests, because using sleep in tests is antipattern and also it will prolong testing time. So, we need some other robust solution for this.

## Better solution
If you think about the test, one idea becomes: It would be nice, if we can start TimerTask when want to assert and also it should be blocked. So, we will create interface for TimedTask to add more abstraction. This interface should provide a start method and holds procedures, which should run after some delay. In this case, we will use listener design pattern, where listeners will be our procedure. Then interface should look like this:

``` java
public interface Clock {

	void register(@Nonnull final ClockListener listener);

	void start();

	void stop();

	interface ClockListener {
		void timeElapsed();
	}
}
```
Great, now we will edit our example of invalidating access token.

``` java
public String refreshLoginOfClient(@Nonnull final String refreshToken) throws LoginFailedException {
	... // Checking of refresh token and so on.

	// Generate new access token
	final String accessToken = securityTokenSupplier.generateAccessToken(refreshToken);
	clientsRefreshTokens.put(accessToken, refreshToken);

	// Invalidate access token after delay
	final Clock clock = ... 
	clock.register(() -> clientsRefreshTokens.remove(accessToken));
	clock.start();

	return accessToken;
}
```

After this edit, we have an abstraction, which we can work with. But one thing is missing. We need something, what will create instances of our interface for clock and also it must create various implementations. (In tests we want to another instance of the clock then in the normal run of code). So, that leads us to another design pattern and that is a factor. So lets define an interface for factory:

``` java
public interface ClockFactory {
	Clock createClock(final long millis);
}
```

Add this factory to our method:

``` java
public String refreshLoginOfClient(@Nonnull final String refreshToken) throws LoginFailedException {
	... // Checking of refresh token and so on.

	// Generate new access token
	final String accessToken = securityTokenSupplier.generateAccessToken(refreshToken);
	clientsRefreshTokens.put(accessToken, refreshToken);

	// Invalidate access token after delay
	final Clock clock = clockFactory.createClock(ApiConstants.ACCESS_TOKEN_INVALIDATION_DELAY);
	clock.register(() -> clientsRefreshTokens.remove(accessToken));
	clock.start();

	return accessToken;
}
```

At least we have robust implementation, which depends on implementation of ClockFactory. So, let's implement factory and clock for our method.

``` java
public static final class SimpleClockFactory implements ClockFactory {

	private final long delayMillis;

	@Override
	public Clock createClock() {
		return new SimpleClock(delayMillis);
	}
}
```

An implementation of SimpleClock, which is only wrapper for TimerTask and holds Timer for executing it.

``` java
public class SimpleClock extends TimerTask implements Clock {

	private final Collection<ClockListener> listeners = new HashSet<>();
	private final Timer timer = new Timer(true);
	private final long delayMillis;

	public SimpleClock(final long delay) {
		this.delayMillis = delay;
	}

	@Override
	public void register(@Nonnull final ClockListener listener) {
		listeners.add(listener);
	}

	@Override
	public void start() {
		timer.schedule(this, delayMillis);
	}

	@Override
	public void stop() {
		timer.cancel();
		timer.purge();
	}

	@Override
	public void run() {
		listeners.forEach(ClockListener::timeElapsed);
	}
}
```

Now, we are done with the implementation of our example method with appropriate Clock implementation. So, lets start with implementing test method.

## Test method
Implementation of the test method will be simple. Passed test clock factory for service, login to the service and then manually invalidate tokens by clock method. So, the test method will look like this:

``` java
@Test
public void testInvalidateAccessToken() throws LoginFailedException {
	final ClientTokens tokens = securityService.loginClient(PASSWORD);
	Assert.assertTrue(securityService.isClientLogged(tokens.getAccessToken()));

	clockFactory.timeElapsed();
	Assert.assertFalse(securityService.isClientLogged(tokens.getAccessToken()));
}
```

Let's implement appropriate factory, which will create our manual clock for tests. Also, we must add ability to run listeners when we need them.

``` java
private class ManualClockFactory implements ClockFactory {

	private final List<ManualClock> clocks = new ArrayList<>();

	@Override
	public Clock createClock(final long millis) {
		final ManualClock clock = new ManualClock();
		clocks.add(clock);
		return clock;
	}

	public void timeElapsed() {
		clocks.forEach(ManualClock::timeElapsed);
	}
}
```

An implementation of manual clock is straightforward, only storing listeners.

``` java
public class ManualClock implements Clock {

	private final Collection<ClockListener> listeners = new HashSet<>();

	@Override
	public void register(@Nonnull final ClockListener listener) {
		listeners.add(listener);
	}

	@Override
	public void start() {
	}

	@Override
	public void stop() {
	}

	public void timeElapsed() {
		listeners.forEach(ClockListener::timeElapsed);
	}
}
```

## Reactive programming solution
Reactive programming has also an own variation of this. The solution is a switch Clock Factory with Scheduler, create Observable with delay and subscribe consumer by calling method to remove accessToken.

So, lets change method, where ```timeScheduler = Schedulers.computation()```.

``` java
public String refreshLoginOfClient(@Nonnull final String refreshToken) throws LoginFailedException {
	... // Checking of refresh token and so on.

	// Generate new access token
	final String accessToken = securityTokenSupplier.generateAccessToken(refreshToken);
	clientsRefreshTokens.put(accessToken, refreshToken);

	// Invalidate access token after delay
	Observable
		.timer(ApiConstants.ACCESS_TOKEN_INVALIDATION_DELAY, TimeUnit.MILISECONDS, timerScheduler)
		.subsribe(aLong -> clientsRefreshTokens.remove(accessToken));

	return accessToken;
}
```

And change test method, with passing ```TestScheduler``` to service.

``` java
@Test
public void testInvalidateAccessToken() throws LoginFailedException {
	final ClientTokens tokens = securityService.loginClient(PASSWORD);
	Assert.assertTrue(securityService.isClientLogged(tokens.getAccessToken()));

	testScheduler.advanceTimeBy(ApiConstants.ACCESS_TOKEN_INVALIDATION_DELAY, TimeUnit.MILISECONDS);
	Assert.assertFalse(securityService.isClientLogged(tokens.getAccessToken()));
}
```
