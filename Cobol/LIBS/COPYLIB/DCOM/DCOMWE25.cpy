      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB025: DB2PRD.TEVNTO_ELMTO_DESC             *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM425S                                       *
      *    -> FETCH   - DCOM425F                                       *
      *    -> INSERT  - DCOM425I                                       *
      *    -> UPDATE  - DCOM425U                                       *
      *    -> DELETE  - DCOM425D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW25E-HEADER.
          10 DCOMW25E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW25E'.
          10 DCOMW25E-TAM-LAYOUT       PIC  9(005) VALUE 00295.
      *
       05 DCOMW25E-BLOCO-ENTRADA.
          10 DCOMW25E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW25E-ENTRADA.
             15 DCOMW25E-CELMTO-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMW25E-CEVNTO-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMW25E-CINDCD-TPO-EVNTO          PIC  X(001).
             15 DCOMW25E-CINDCD-REG-LOG            PIC  X(001).
             15 DCOMW25E-CTBELA-LOG-DESC           PIC  X(008).
             15 DCOMW25E-CCATLG-COMPS-REG          PIC  X(008).
             15 DCOMW25E-NSEQ-PROCM-EVNTO          PIC S9(003) COMP-3.
             15 DCOMW25E-CTRANS-PROG-ATULZ         PIC  X(008).
             15 DCOMW25E-HULT-ATULZ                PIC  X(026).

          10 DCOMW25E-BLOCO-VARIAVEL.
             15 DCOMW25E-V-FILLER                  PIC  X(100).

          10 DCOMW25E-BLOCO-PAGINACAO.
             15 DCOMW25E-MAX-OCORR                 PIC  9(003).
             15 DCOMW25E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW25E-P-INICIAL              VALUE 'I'.
                88 DCOMW25E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW25E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW25E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW25E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW25E-BLOCO-RESTART.
             15 DCOMW25E-CHAVE-INI.
                20 DCOMW25E-I-CELMTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW25E-I-CEVNTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW25E-I-CINDCD-TPO-EVNTO     PIC  X(001).
             15 DCOMW25E-CHAVE-FIM.
                20 DCOMW25E-F-CELMTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW25E-F-CEVNTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW25E-F-CINDCD-TPO-EVNTO     PIC  X(001).
      *
          10 DCOMW25E-FILLER                       PIC  X(100).
      *
