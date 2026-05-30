      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB052: DB2PRD.TREGRA_MORA_LEGAL                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM452S                                       *
      *    -> FETCH   - DCOM452F                                       *
      *    -> INSERT  - DCOM452I                                       *
      *    -> UPDATE  - DCOM452U                                       *
      *    -> DELETE  - DCOM452D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW52E-HEADER.
          10 DCOMW52E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW52E'.
          10 DCOMW52E-TAM-LAYOUT       PIC  9(005) VALUE 00589.
      *
       05 DCOMW52E-BLOCO-ENTRADA.
          10 DCOMW52E-INSTRUCAO                    PIC  X(010).
          10 DCOMW52E-ENTRADA.
             15 DCOMW52E-CREGRA-MORA-LEGAL         PIC S9(003) COMP-3.
             15 DCOMW52E-IREGRA-MORA-LEGAL         PIC  X(040).
             15 DCOMW52E-CIDTFD-CORRC-DIVDA        PIC S9(005) COMP-3.
             15 DCOMW52E-CINDCD-CALC-MORA          PIC S9(001) COMP-3.
             15 DCOMW52E-CIDTFD-JURO-MORA          PIC S9(005) COMP-3.
             15 DCOMW52E-HULT-ATULZ                PIC  X(026).
             15 DCOMW52E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMW52E-CTERM                     PIC  X(008).
          10 DCOMW52E-BLOCO-VARIAVEL.
             15 DCOMW52E-V-FILLER                  PIC  X(300).
          10 DCOMW52E-BLOCO-PAGINACAO.
             15 DCOMW52E-MAX-OCORR                 PIC  9(003).
             15 DCOMW52E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW52E-P-INICIAL              VALUE 'I'.
                88 DCOMW52E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW52E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW52E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW52E-P-ULTIMA               VALUE 'U'.
          10 DCOMW52E-BLOCO-RESTART.
             15 DCOMW52E-CHAVE-INI.
                20 DCOMW52E-I-CREGRA-MORA-LEGAL    PIC S9(003) COMP-3.
             15 DCOMW52E-CHAVE-FIM.
                20 DCOMW52E-F-CREGRA-MORA-LEGAL    PIC S9(003) COMP-3.
          10 DCOMW52E-FILLER                       PIC  X(170).
      *                                                                *
