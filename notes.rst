Protocol quirks / documentation errors
======================================

- Port Mode Information Requestion [0x22] with Information Type
  "CAPABILITY BITS" [0x08] always results in parameter error (for
  every type of hardware I tried).


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
