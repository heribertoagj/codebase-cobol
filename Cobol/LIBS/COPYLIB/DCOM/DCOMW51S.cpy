      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB051: DB2PRD.TREGRA_MORA_GERC                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM451S                                       *
      *    -> FETCH   - DCOM451F                                       *
      *    -> INSERT  - DCOM451I                                       *
      *    -> UPDATE  - DCOM451U                                       *
      *    -> DELETE  - DCOM451D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW51S-HEADER.
          10 DCOMW51S-COD-LAYOUT   PIC  X(008) VALUE 'DCOMW51S'.
          10 DCOMW51S-TAM-LAYOUT   PIC  9(005) VALUE 05700.
      *
          10 DCOMW51S-BLOCO-SAIDA.
             15 DCOMW51S-QTDE-TOTAL            PIC  9(010).
             15 DCOMW51S-QTDE-RETORNADA        PIC  9(003).
             15 DCOMW51S-OCORRENCIA            OCCURS 050 TIMES.
                20 DCOMW51S-CREGRA-MORA-GERC   PIC S9(003) COMP-3.
                20 DCOMW51S-IREGRA-MORA-GERC   PIC  X(040).
                20 DCOMW51S-TINIC-VCTO-PCELA   PIC S9(005) COMP-3.
                20 DCOMW51S-TFNAL-VCTO-PCELA   PIC S9(005) COMP-3.
                20 DCOMW51S-CINDCD-CALC-MORA   PIC S9(001) COMP-3.
                20 DCOMW51S-CIDTFD-CORRC-DIVDA PIC S9(005) COMP-3.
                20 DCOMW51S-CIDTFD-JURO-MORA   PIC S9(005) COMP-3.
                20 DCOMW51S-CINDCD-RECAL-VENCD PIC  X(001).
                20 DCOMW51S-HULT-ATULZ         PIC  X(026).
                20 DCOMW51S-CFUNC-BDSCO        PIC S9(009) COMP-3.
                20 DCOMW51S-CTERM              PIC  X(008).
                20 DCOMW51S-CIDTFD-JURO-REMUN  PIC S9(005) COMP-3.
                20 DCOMW51S-PTX-JURO-REMUN     PIC S9(003) COMP-3.
                20 DCOMW51S-CIDTFD-MORA-VENCD  PIC S9(005) COMP-3.
                20 DCOMW51S-PTX-MORA-VENCD     PIC S9(003) COMP-3.
                20 DCOMW51S-CIDTFD-MULTA-VENCD PIC S9(005) COMP-3.
                20 DCOMW51S-PTX-MULTA-VENCD    PIC S9(003) COMP-3.
             15 DCOMW51S-FILLER                PIC  X(174).
      *
