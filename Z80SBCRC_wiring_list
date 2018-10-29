# Wiring list of Z80SBCRC
These are list of connections over the existing rev1 ProtoRC. 

CPLD is already prewired with address, data, and control signals.
VCC to Z80 (pin 11) <- note RAM is battery-backed via DS1210
GND to RAM (pin 16) and Z80 (pin 29)
Data D[7:0] from RAM to Z80 to RC2014 connector
Address A[13:0] from RAM to Z80 to RC2014 connector
nM1 from Z80 to RC2014 connector
nRESET from Z80 to RC2014 connector
Clock from Z80 to RC2014 connector to T37
nINT from Z80 to RC2014 connector and pulled up via 4.7K resistor
nMREQ from Z80 to RC2014 connector to pin 5 of DS1210 to T10 
nWR from Z80 to RC2014 connector to nWE of RAM (pin 29)
nRD from Z80 to RC2014 connector to nOE of RAM (pin 24)
nIORQ from Z80 to RC2014 connector to 4.7K pullup
T17 to 4.7K pull up <-- this is the bootstrap mode select
T1 to pin 2 of 6-pin serial port header
T2 to pin 3 of 6-pin serial port header
GND to pin 5 of 6-pin serial port header
RAM A14 (pin 3) to T21
RAM A15 (pin 31) to T20
RAM A16 (pin 2) to T19
RAM A17 (pin 30) to T16 
RAM A18 (pin 1) to T18
***
**CF interface:**
IDE pin 35 (A0) to A0
IDE pin 33 (A1) to A1
IDE pin 36 (A2) to A2
IDE pin 17 (D0) to D0
IDE pin 15 (D1) to D1
IDE pin 13 (D2) to D2
IDE pin 11 (D3) to D3
IDE pin 9 (D4) to D4
IDE pin 7 (D5) to D5
IDE pin 5 (D6) to D6
IDE pin 3 (D7) to D7
IDE pin 38 (nCS3FX) to VCC
IDE pin 31 (nINTR) to 4.7K pullup
IDE pin 23 (nIOWR) to T31
IDE pin 25 (nIORD) to T30
IDE pin 37 (nCS1FX) to T29
***
**DS1210 Battery backup** 
pins 3, 4, 7 to GND
pin 2 to battery (CR1210)
pin 8 to VCC
pin 1 to RAM VCC (pin 32)
pin 5 to nMREQ
pin 6 to RAM nCS (pin 22)
