      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPI                                      *
      * TAMANHO : 0059 BYTES                                           *
      * ARQUIVO : TRANSITORIA CONTROLE DIGITACAO TITULO                *
      * INCLUDE DA TABELA DCOMB0F9 - DB2PRD.TTRASI_CTRL_DIGTC          *
      ******************************************************************
       01  PI-F9-REGISTRO.
01 03      05 PI-F9-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04 05      05 PI-F9-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09 05      05 PI-F9-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
14 02      05 PI-F9-QTITLO-DIGTC-AG      PIC S9(3)V USAGE COMP-3.
16 09      05 PI-F9-VTOT-TITLO-DIGTC     PIC S9(15)V9(2) USAGE COMP-3.
25 01      05 PI-F9-CINDCD-CONCL-DIGTC   PIC X(1).
26 26      05 PI-F9-HULT-ATULZ           PIC X(26).
52 08      05 PI-F9-CTERM                PIC X(8).
