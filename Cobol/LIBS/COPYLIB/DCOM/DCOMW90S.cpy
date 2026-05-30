      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB090: DB2PRD.TMEIO_PGTO_DESC                         *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM490S                                       *
      *    -> FETCH   - DCOM490F                                       *
      *    -> INSERT  - DCOM490I                                       *
      *    -> UPDATE  - DCOM490U                                       *
      *    -> DELETE  - DCOM490D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW90S-HEADER.
          10 DCOMW90S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW90S'.
          10 DCOMW90S-TAM-LAYOUT       PIC  9(005) VALUE  4926.
      *
          10 DCOMW90S-BLOCO-SAIDA.
             15 DCOMW90S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW90S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW90S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW90S-CMEIO-PGTO-DESC        PIC S9(003) COMP-3.
                20 DCOMW90S-IMEIO-PGTO-DESC        PIC  X(040).
                20 DCOMW90S-IRSUMO-MEIO-PGTO       PIC  X(015).
                20 DCOMW90S-CSGL-MEIO-PGTO         PIC  X(006).
                20 DCOMW90S-CINDCD-MEIO-CNDDO      PIC  X(001).
                20 DCOMW90S-HULT-ATULZ             PIC  X(026).
                20 DCOMW90S-CTRANS-PROG-ATULZ      PIC  X(008).
      *                                                                *
