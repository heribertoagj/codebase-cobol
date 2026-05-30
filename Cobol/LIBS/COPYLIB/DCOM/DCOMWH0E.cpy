      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0H0: DB2PRD.TCONTR_CONS_CREDT                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4H0S                                       *
      *    -> FETCH   - DCOM4H0F                                       *
      *    -> INSERT  - DCOM4H0I                                       *
      *    -> UPDATE  - DCOM4H0U                                       *
      *    -> DELETE  - DCOM4H0D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWH0E-HEADER.
          10 DCOMWH0E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWH0E'.
          10 DCOMWH0E-TAM-LAYOUT       PIC  9(005) VALUE 00613.
      *
       05 DCOMWH0E-BLOCO-ENTRADA.
          10 DCOMWH0E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWH0E-ENTRADA.
             15 DCOMWH0E-CCONTR-LIM-DESC           PIC  S9(009) COMP-3.
             15 DCOMWH0E-CVRSAO-CONTR-LIM          PIC  S9(003) COMP-3.
             15 DCOMWH0E-NSEQ-CONS-CREDT           PIC  S9(003) COMP-3.
             15 DCOMWH0E-DANO-BASE                 PIC  S9(004) COMP-3.
             15 DCOMWH0E-CDEPDC                    PIC  S9(005) COMP-3.
             15 DCOMWH0E-CSEQ-STUDO                PIC  S9(009) COMP-3.
             15 DCOMWH0E-VCONS-NEGOC               PIC S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMWH0E-CCART-LIM-CREDT           PIC  X(005).
             15 DCOMWH0E-HULT-ATULZ                PIC  X(026).
             15 DCOMWH0E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMWH0E-CTERM                     PIC  X(008).
             15 DCOMWH0E-FILLER                    PIC  X(052).
      *
          10 DCOMWH0E-BLOCO-VARIAVEL.
             15 DCOMWH0E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWH0E-BLOCO-PAGINACAO.
             15 DCOMWH0E-MAX-OCORR                 PIC  9(003).
             15 DCOMWH0E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWH0E-P-INICIAL              VALUE 'I'.
                88 DCOMWH0E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWH0E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWH0E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWH0E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWH0E-BLOCO-RESTART.
             15 DCOMWH0E-CHAVE-INI.
                20 DCOMWH0E-I-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMWH0E-I-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMWH0E-I-NSEQ-CONS-CREDT      PIC  S9(003) COMP-3.
                20 DCOMWH0E-I-FILLER               PIC  X(015).
             15 DCOMWH0E-CHAVE-FIM.
                20 DCOMWH0E-F-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMWH0E-F-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMWH0E-F-NSEQ-CONS-CREDT      PIC  S9(003) COMP-3.
                20 DCOMWH0E-F-FILLER               PIC  X(015).
      *
          10 DCOMWH0E-FILLER                       PIC  X(113).
      *
