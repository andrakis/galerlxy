Galerlxy
--------

A massive-multiclient galaxy simulator, designed for load balancing and fault tolerance.


Load balancing
==============

Galerlxy is designed to run across many nodes across many machines. If a particular machine is getting overloaded, it will migrate to another machine, hopefully transparently to the user.


Fault tolerance
===============

Galerlxy aims to provide quick resolution in the event of a network partition or node failure, restarting the affected zones as quickly as possible.
