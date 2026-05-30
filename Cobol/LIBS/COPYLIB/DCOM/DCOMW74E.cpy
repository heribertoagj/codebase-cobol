      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB074: DB2PRD.TTAC_CONTR_LIM                          *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM474S                                       *
      *    -> FETCH   - DCOM474F                                       *
      *    -> INSERT  - DCOM474I                                       *
      *    -> UPDATE  - DCOM474U                                       *
      *    -> DELETE  - DCOM474D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW74E-HEADER.
          10 DCOMW74E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW74E'.
          10 DCOMW74E-TAM-LAYOUT       PIC  9(005) VALUE 00582.
      *
       05 DCOMW74E-BLOCO-ENTRADA.
          10 DCOMW74E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW74E-ENTRADA.
             15 DCOMW74E-CPRODT                    PIC  S9(003) COMP-3.
             15 DCOMW74E-DINIC-PARMZ-CONTR         PIC  X(010).
             15 DCOMW74E-CTPO-COBR-TAC             PIC  S9(003) COMP-3.
             15 DCOMW74E-CTAC-CONTR-LIM            PIC  S9(005) COMP-3.
             15 DCOMW74E-CTAC-PERC-CONTR           PIC  S9(005) COMP-3.
             15 DCOMW74E-CTAC-MIN-CONTR            PIC  S9(005) COMP-3.
             15 DCOMW74E-CTAC-MAX-CONTR            PIC  S9(005) COMP-3.
             15 DCOMW74E-DFIM-PARMZ-CONTR          PIC  X(010).
             15 DCOMW74E-HULT-ATULZ                PIC  X(026).
             15 DCOMW74E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMW74E-CTERM                     PIC  X(008).
             15 DCOMW74E-FILLER                    PIC  X(041).
      *
          10 DCOMW74E-BLOCO-VARIAVEL.
             15 DCOMW74E-V-FILLER                  PIC  X(300).
      *
          10 DCOMW74E-BLOCO-PAGINACAO.
             15 DCOMW74E-MAX-OCORR                 PIC  9(003).
             15 DCOMW74E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW74E-P-INICIAL              VALUE 'I'.
                88 DCOMW74E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW74E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW74E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW74E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW74E-BLOCO-RESTART.
             15 DCOMW74E-CHAVE-INI.
                20 DCOMW74E-I-CPRODT               PIC  S9(003) COMP-3.
                20 DCOMW74E-I-DINIC-PARMZ-CONTR    PIC  X(010).
                20 DCOMW74E-I-CTPO-COBR-TAC        PIC  S9(003) COMP-3.
                20 DCOMW74E-I-FILLER               PIC  X(014).
             15 DCOMW74E-CHAVE-FIM.
                20 DCOMW74E-F-CPRODT               PIC  S9(003) COMP-3.
                20 DCOMW74E-F-DINIC-PARMZ-CONTR    PIC  X(010).
                20 DCOMW74E-F-CTPO-COBR-TAC        PIC  S9(003) COMP-3.
                20 DCOMW74E-F-FILLER               PIC  X(014).
      *
          10 DCOMW74E-FILLER                       PIC  X(083).
      *
