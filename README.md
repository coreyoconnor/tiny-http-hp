tiny-http-hp
============

An attempt to make a small HTTP server using only Haskell Platform components

known issues:

* always requests the connection to be closed
* JMeter reports the connection is closed *before* the response is sent. On ocassion. Laziness
  issue?

