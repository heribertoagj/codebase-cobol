      ******************************************************************
      * NOME BOOK : DCOMW25M - BOOK DE INTERFACE COM SERVICO DCOM425M  *
      * DATA      : 18/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 171 BYTES                                          *
      ******************************************************************
           05 DCOMW25M-HEADER.
             10 DCOMW25M-COD-LAYOUT      PIC X(08) VALUE 'DCOMW25M'.
             10 DCOMW25M-TAM-LAYOUT      PIC 9(05) VALUE 171.
           05 DCOMW25M-REGISTRO.
             10 DCOMW25M-AREA-ENTRADA.
                15 DCOMW25M-E-CPRODT               PIC 9(003).
             10 DCOMW25M-AREA-SAIDA.
                15 DCOMW25M-S-CPRODT               PIC 9(003).
                15 DCOMW25M-S-DINIC-PARM-CONFG     PIC X(010).
                15 DCOMW25M-S-CEMPR-GSTOR-OPER     PIC 9(005).
                15 DCOMW25M-S-CDEPDC-GSTOR-OPER    PIC 9(005).
                15 DCOMW25M-S-CEMPR-GSTOR-NEGOC    PIC 9(005).
                15 DCOMW25M-S-CDEPDC-GSTOR-NEGOC   PIC 9(005).
                15 DCOMW25M-S-CMOEDA-OFIC-DESC     PIC 9(005).
                15 DCOMW25M-S-HINIC-TRANS-ONLINE   PIC X(008).
                15 DCOMW25M-S-HFIM-TRANS-ONLINE    PIC X(008).
                15 DCOMW25M-S-HLIM-OPER-NORML      PIC X(008).
                15 DCOMW25M-S-HLIM-OPER-RETRO      PIC X(008).
                15 DCOMW25M-S-TMAX-OPER-ATRSO      PIC 9(003).
                15 DCOMW25M-S-CINDCD-PRZ-OPER      PIC X(002).
                15 DCOMW25M-S-DFCHTO-MES           PIC X(010).
                15 DCOMW25M-S-CSIT-ACSSO-ONLINE    PIC X(001).
                15 DCOMW25M-S-HSIT-ACSSO-ONLINE    PIC X(026).
                15 DCOMW25M-S-HULT-ATULZ           PIC X(026).
                15 DCOMW25M-S-CFUNC-BDSCO          PIC 9(009).
                15 DCOMW25M-S-CTERM                PIC X(008).
