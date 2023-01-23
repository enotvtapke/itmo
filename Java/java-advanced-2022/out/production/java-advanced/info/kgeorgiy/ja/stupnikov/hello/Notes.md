WARNING: A command line option has enabled the Security Manager
WARNING: The Security Manager is deprecated and will be removed in a future release
Running class info.kgeorgiy.java.advanced.hello.HelloClientTest for info.kgeorgiy.ja.stupnikov.hello.HelloUDPNonblockingClient
=== Running test01_singleRequest
Socket closed
Test finished in 0.227s
=== Running test02_sequence
Test finished in 0.061s
Socket closed
=== Running test03_singleWithFailures
Test finished in 0.220s
Socket closed
=== Running test04_sequenceWithFailures
Exception in thread "Thread-3" java.lang.AssertionError: Invalid or unexpected request info.kgeorgiy.java.advanced.hello.HelloClientTest0_3 expected:<2> but was:<3>
at junit@4.11/org.junit.Assert.fail(Assert.java:88)
at junit@4.11/org.junit.Assert.failNotEquals(Assert.java:743)
at junit@4.11/org.junit.Assert.assertEquals(Assert.java:118)
at junit@4.11/org.junit.Assert.assertEquals(Assert.java:555)
at info.kgeorgiy.java.advanced.hello/info.kgeorgiy.java.advanced.hello.Util.lambda$server$1(Util.java:159)
at java.base/java.lang.Thread.run(Thread.java:833)
