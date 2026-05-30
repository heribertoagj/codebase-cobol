      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB087: DB2PRD.TWORKF_CONTR_LIM                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM487S                                       *
      *    -> FETCH   - DCOM487F                                       *
      *    -> INSERT  - DCOM487I                                       *
      *    -> UPDATE  - DCOM487U                                       *
      *    -> DELETE  - DCOM487D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW87E-HEADER.
          10 DCOMW87E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW87E'.
          10 DCOMW87E-TAM-LAYOUT       PIC  9(005) VALUE 00976.
      *
       05 DCOMW87E-BLOCO-ENTRADA.
          10 DCOMW87E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW87E-ENTRADA.
             15 DCOMW87E-CCONTR-LIM-DESC           PIC  S9(009) COMP-3.
             15 DCOMW87E-CVRSAO-CONTR-LIM          PIC  S9(003) COMP-3.
             15 DCOMW87E-CEVNTO-DESC-COML          PIC  S9(003) COMP-3.
             15 DCOMW87E-HWORKF-CONTR-LIM          PIC  X(026).
             15 DCOMW87E-CSIT-INIC-DESC            PIC  S9(003) COMP-3.
             15 DCOMW87E-HSIT-INIC-DESC            PIC  X(026).
             15 DCOMW87E-CSIT-FNAL-DESC            PIC  S9(003) COMP-3.
             15 DCOMW87E-HSIT-FNAL-DESC            PIC  X(026).
             15 DCOMW87E-RWORKF-CONTR-LIM.
                49 DCOMW87E-RWORKF-CONTR-LIM-LEN   PIC S9(004) COMP.
                49 DCOMW87E-RWORKF-CONTR-LIM-TEXT  PIC  X(240).
             15 DCOMW87E-HULT-ATULZ                PIC  X(026).
             15 DCOMW87E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMW87E-CTERM                     PIC  X(008).
             15 DCOMW87E-FILLER                    PIC  X(066).
      *
          10 DCOMW87E-BLOCO-VARIAVEL.
             15 DCOMW87E-V-VARIAVEL                PIC  X(300).
      *
          10 DCOMW87E-BLOCO-PAGINACAO.
             15 DCOMW87E-MAX-OCORR                 PIC  9(003).
             15 DCOMW87E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW87E-P-INICIAL              VALUE 'I'.
                88 DCOMW87E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW87E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW87E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW87E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW87E-BLOCO-RESTART.
             15 DCOMW87E-CHAVE-INI.
                20 DCOMW87E-I-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMW87E-I-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMW87E-I-CEVNTO-DESC-COML     PIC  S9(003) COMP-3.
                20 DCOMW87E-I-HWORKF-CONTR-LIM     PIC  X(026).
                20 DCOMW87E-I-FILLER               PIC  X(019).
             15 DCOMW87E-CHAVE-FIM.
                20 DCOMW87E-F-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMW87E-F-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMW87E-F-CEVNTO-DESC-COML     PIC  S9(003) COMP-3.
                20 DCOMW87E-F-HWORKF-CONTR-LIM     PIC  X(026).
                20 DCOMW87E-F-FILLER               PIC  X(019).
      *
          10 DCOMW87E-FILLER                       PIC  X(103).
      *
