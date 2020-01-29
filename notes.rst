Protocol quirks / documentation errors
======================================

- Port Mode Information Requestion [0x22] with Information Type
  "CAPABILITY BITS" [0x08] always results in parameter error (for
  every type of hardware I tried).


Unused(?) protocol capabilities
===============================

Hub chaining:
https://lego.github.io/lego-ble-wireless-protocol-docs/index.html#common-message-header


Operational quirks
==================

Motors with tacho
-----------------

- position "0" is whatever rotation is in place when the motor is
  initialised.  Therefore for a steering system, it may not be
  "straight ahead".  Calibration is needed.

Colour/distance sensor
----------------------

See ``SensedColour`` haddock.


Alternatives
============

https://github.com/imurvai/brickcontroller2
