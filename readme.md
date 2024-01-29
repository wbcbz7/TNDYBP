# TNDYBP - Debug Extensions-powered Tandy Sound redirector

This utility allows to run applications using Tandy Sound device at port C0 with sound devices that have the SN76489 chip mapped at other port, like 1E0 or 2C0, or a TNDLPT device connector to parallel port.

Unlike other redirection TSRs like TNDY or TNDLPT, it does not use Virtual 8086 mode, instead using the CPU hardware breakpoints to intercept and redirect port I/O access, while running in real mode - this potentially resolves incompatibilities between some software.

### Hardware/software requirements

* an x86 CPU with Debug Extensions support, min. Intel Pentium/AMD K5 or later. 386/486 CPUs do not support hardware breakpoints on I/O port access, hence TNDYBP cannot work on these CPUs.
* a Tandy Sound device connected to the ISA bus (like TNDY board or PicoGUS in Tandy emulation mode), or TNDLPT device connected to the parallel port.
* no V86 monitors/memory managers and 386+ debugging utilities active. 
* tested with MS-DOS 7.1, should work on 3.3+ and FreeDOS.

### Usage

If your Tandy Sound card is mapped to the port 2C0, simply running `TNDYBP` should be enough. Available command line options:

* `/P[hex]` - redirect to Tandy Sound at port [hex] (default 0x2C0)

* `/L[hex]` - redirect to TNDLPT at LPT1/2/3 or port [hex] (default LPT1)

* `/U, /R`  - release and unload from memory

After loading, TNDYBP uses around 600 bytes of memory.

### Known issues

* TNDYLPT redirection not tested
* INT11/15 patches for Tandy detection not implemented
* since it hooks INT1, it can trip into anti-debugger checks on some applications.
* debugging utilities and DOS extenders may clobber debug registers, making TNDYBP not working - in this case, unload it and reinstall.

### Building notes

use [NASM](https://nasm.us) for building TNDYBP - run `nasm tndybp.asm -f bin -o tndybp.com ` and you're done :)



--wbcbz7 29.o1.2o24