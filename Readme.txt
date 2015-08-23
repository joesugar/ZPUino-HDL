ZPUino HDL (Source Code) - Current Version 1.0

ZPUino Home Page:
http://www.alvie.com/zpuino/

ZPUino on Papilio User Guide:
http://www.papilio.cc/index.php?n=Papilio.ZPUinoUserGuide


Note from joesugar:

I use this fork of Alvie's ZPUino code to test various blocks
and projects I'm working on.  For the most part the master is
the same as the original except for the blocks I add to the
contrib directory.  Test builds that can serve as examples are 
contained in their corresponding branches.  Some of them include:

WM8731 - blocks and example for interfacing with the the Wolfson
         WM8731 audio codec as implemented on the Arduino Audio
         Codec shield.  Only supports 44.1 kHz sampling rate and
         requires the included I2C blocks.
PSK    - blocks and example for a PSK31 transmit block.  The top
         leve block includes a simple sigma-delta D2A used to test
         the block.  It could easily be removed and replaced with
         another.  An Arudino sketch showing how to make use of 
         the PSK block is included.

