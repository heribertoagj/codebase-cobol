      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMW52S-HEADER.
          10 DCOMW52S-COD-LAYOUT   PIC  X(008) VALUE 'DCOMW52S'.
          10 DCOMW52S-TAM-LAYOUT   PIC  9(005) VALUE 04550.
      *
          10 DCOMW52S-BLOCO-SAIDA.
             15 DCOMW52S-QTDE-TOTAL            PIC  9(010).
             15 DCOMW52S-QTDE-RETORNADA        PIC  9(003).
             15 DCOMW52S-OCORRENCIA            OCCURS 050 TIMES.
                20 DCOMW52S-CREGRA-MORA-LEGAL  PIC S9(003) COMP-3.
                20 DCOMW52S-IREGRA-MORA-LEGAL  PIC  X(040).
                20 DCOMW52S-CIDTFD-CORRC-DIVDA PIC S9(005) COMP-3.
                20 DCOMW52S-CINDCD-CALC-MORA   PIC S9(001) COMP-3.
                20 DCOMW52S-CIDTFD-JURO-MORA   PIC S9(005) COMP-3.
                20 DCOMW52S-HULT-ATULZ         PIC  X(026).
                20 DCOMW52S-CFUNC-BDSCO        PIC S9(009) COMP-3.
                20 DCOMW52S-CTERM              PIC  X(008).
             15 DCOMW52S-FILLER                PIC  X(124).
      *
