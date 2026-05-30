      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB066: DB2PRD.TSPROD_MEIO_PGTO                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM466S                                       *
      *    -> FETCH   - DCOM466F                                       *
      *    -> INSERT  - DCOM466I                                       *
      *    -> UPDATE  - DCOM466U                                       *
      *    -> DELETE  - DCOM466D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW66S-HEADER.
          10 DCOMW66S-COD-LAYOUT   PIC  X(008) VALUE 'DCOMW66S'.
          10 DCOMW66S-TAM-LAYOUT   PIC  9(005) VALUE 05500.
      *
          10 DCOMW66S-BLOCO-SAIDA.
             15 DCOMW66S-QTDE-TOTAL            PIC  9(010).
             15 DCOMW66S-QTDE-RETORNADA        PIC  9(003).
             15 DCOMW66S-OCORRENCIA            OCCURS 050 TIMES.
                20 DCOMW66S-CPRODT             PIC  S9(003) COMP-3.
                20 DCOMW66S-CSPROD-DESC-COML   PIC  S9(003) COMP-3.
                20 DCOMW66S-CINDCD-VCTO-PCELA  PIC  X(001).
                20 DCOMW66S-CMEIO-PGTO-DESC    PIC  S9(003) COMP-3.
                20 DCOMW66S-HINIC-MEIO-PGTO    PIC  X(026).
                20 DCOMW66S-CINDCD-RESP-PGTO   PIC  X(001).
                20 DCOMW66S-TDEB-PCELA-VENCD   PIC  S9(003) COMP-3.
                20 DCOMW66S-CINDCD-PRZ-PCELA   PIC  X(002).
                20 DCOMW66S-HFIM-MEIO-PGTO     PIC  X(026).
                20 DCOMW66S-HULT-ATULZ         PIC  X(026).
                20 DCOMW66S-CFUNC-BDSCO        PIC  S9(009) COMP-3.
                20 DCOMW66S-CTERM              PIC  X(008).
                20 DCOMW66S-CINDCD-PGTO-OPCAO  PIC  X(001).
                20 DCOMW66S-CINDCD-OPCAO-BLETO PIC  X(001).
             15 DCOMW66S-FILLER                PIC  X(237).
      *
