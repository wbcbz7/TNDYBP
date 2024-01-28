strings:
.start      db  "TNDYBP - Debug Extensions-powered Tandy Sound redirector - wbcbz7 28.o1.2o24"
.crlf       db  13,10, '$'
.error      db  "error: ", '$'
.no_de      db  "CPU has no Debug Extensions support!", 13,10,'$'
.v86        db  "CPU is in V86 mode!", 13,10,'$'
.installed  db  "installed",'$'
.released   db  "released",13,10,'$'
.reset      db  "reset",'$'
.not_last   db  "not last in interrupt chain!",13,10,'$'
.already    db  "already installed",'$'
.not_inst   db  "not installed!",13,10,'$'
.redirect   db  " - redirect to ",'$'
.tandysnd   db  "Tandy Sound at 0x",'$'
.tndlpt     db  "TNDLPT at 0x",'$'
.no_port    db  "incorrect port!",13,10,'$'
.help       db  "help:",13,10
            db  "  /P[hex] - redirect to Tandy Sound at port [hex]        (default 0x2C0)",13,10
            db  "  /L[hex] - redirect to TNDLPT at LPT1/2/3 or port [hex] (default LPT1)",13,10,
            db  "  /U, /R  - release and unload from memory",13,10,'$'