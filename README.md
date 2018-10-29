# Z80SBCRC Single board 20MHz Z80 for RC2014
## Introduction
The original intent of Z80SBCRC is to design a very simple single-board Z80 computer to check out the handful of 20MHz Z80 chips I purchased off eBay.  The design built on a Prototype for RC2014 board (https://github.com/Plasmode/ProtoRC) and added Z80 and SRAM and later a compact flash drive.  All the logic are inside the existing EPM7128 CPLD on the prototype board.

![](DSC_40221029.jpg)

[Solder side](DSC_40231029.jpg) picture of Z80SBCRC prototype.

## Features
* 20MHz Z80 running at 22MHz
* 512Kbyte SRAM
* Compact flash interface
* CP/M ready
* CPLD glue logic
* 16K banked memory
* Serial bootstrap
* Battery-backed memory
* RC2014 bus compatible
## Design Data
* [ProtoRC rev1 schematic](https://github.com/Plasmode/ProtoRC/blob/master/protoRC_r1_scm.pdf).  This is the base on which Z80SBCRC is built on.
  - [Wiring list](Z80SBCRC_wiring_list.md) of Z80SBCRC
* CPLD equations of Altera EPM7128SQC100
### Software for Z80SBCRC
* Serial bootstrap, this 255-byte program is serially loaded into lowest part of RAM.  Execution starts from 0x0 after the 255th byte is loaded.
* ZMon, simple monitor for Z80SBCRC
* SCMonitor, a sophiscated monitor by [Steve Cousins](http://scc.me.uk/) ported to Z80SBCRC
* cpm22all, CP/M22 for Z80SBCRC
* xmodem, this is first loaded into memory as Intel HEX file, then boot up CP/M and type 'SAVE 17 XMODEM.COM' to create the first file in CP/M22 disk
* CPM22DISTRO, this is the distribution files for CP/M2.2.  It is packaged as .pkg file.  It needs depkg.com (below) to unpackage into CP/M2.2 system files
* depkg.com, this file unpackage the CPM22DISTRO above.
## Construction log
Photos of Z80SBCRC being constructed.

