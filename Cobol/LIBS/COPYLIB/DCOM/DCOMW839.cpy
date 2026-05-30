      ******************************************************************
      * ARQUIVO COM INFORMACOES PARA MONTAGEM DO DOC3040               *
      * NOME DO BOOK : DCOMW839 - TAMANHO DO REGISTRO: 400 BYTES       *
      * AUTOR        : EDSON MATSUMOTO 7COMM                           *
      * FORMATO DATAS: AAAA-MM-DD                                      *
      ******************************************************************
       01  REG-DCOMW839.
           03  DCOMW839-TPREG                 PIC  9(002).
      *----------------------------------------------------------------*
      * TPREG = 00 - HEADER                                            *
      *----------------------------------------------------------------*
           03  DCOMW839-TIPREG00.
               05  DCOMW839-TR00-ROTINA       PIC  X(004).
               05  DCOMW839-TR00-DTBASE       PIC  X(010).
               05  DCOMW839-TR00-DTGER        PIC  X(010).
               05  DCOMW839-TR00-TPMOV        PIC  X(001).
               05  FILLER                     PIC  X(373).
      *----------------------------------------------------------------*
      * TPREG = 04 - CARACTERISTICA ESPECIAL                           *
      *----------------------------------------------------------------*
           03  DCOMW839-TIPREG04        REDEFINES DCOMW839-TIPREG00.
               05  DCOMW839-TR04-MOTSUB       PIC  X(001).
               05  DCOMW839-TR04-NROCPFCNPJ   PIC  9(009)       COMP-3.
               05  DCOMW839-TR04-FLIALCPFCNPJ PIC  9(004)       COMP-3.
               05  DCOMW839-TR04-CONTRCPFCNPJ PIC  9(002)       COMP-3.
               05  DCOMW839-TR04-TPCLI        PIC  9(001)       COMP-3.
               05  DCOMW839-TR04-CCUSTO       PIC  X(004).
               05  DCOMW839-TR04-AGENCIA      PIC  9(005)       COMP-3.
               05  DCOMW839-TR04-CONTA        PIC  9(007)       COMP-3.
               05  DCOMW839-TR04-CARTEIRA     PIC  X(003).
               05  DCOMW839-TR04-NCONTR       PIC  9(017)       COMP-3.
               05  DCOMW839-TR04-EMPRESA      PIC  9(005)       COMP-3.
               05  DCOMW839-TR04-CARACESPEC01 PIC  9(003)       COMP-3.
               05  DCOMW839-TR04-CARACESPEC02 PIC  9(003)       COMP-3.
               05  DCOMW839-TR04-CARACESPEC03 PIC  9(003)       COMP-3.
               05  DCOMW839-TR04-CARACESPEC04 PIC  9(003)       COMP-3.
               05  DCOMW839-TR04-CARACESPEC05 PIC  9(003)       COMP-3.
               05  DCOMW839-TR04-CARACESPEC06 PIC  9(003)       COMP-3.
               05  DCOMW839-TR04-CARACESPEC07 PIC  9(003)       COMP-3.
               05  DCOMW839-TR04-CARACESPEC08 PIC  9(003)       COMP-3.
               05  DCOMW839-TR04-CARACESPEC09 PIC  9(003)       COMP-3.
               05  DCOMW839-TR04-CARACESPEC10 PIC  9(003)       COMP-3.
               05  FILLER                     PIC  X(340).
