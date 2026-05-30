      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB033: DB2PRD.TMEIO_ENTRD_DESC                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM433S                                       *
      *    -> FETCH   - DCOM433F                                       *
      *    -> INSERT  - DCOM433I                                       *
      *    -> UPDATE  - DCOM433U                                       *
      *    -> DELETE  - DCOM433D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW33S-HEADER.
          10 DCOMW33S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW33S'.
          10 DCOMW33S-TAM-LAYOUT       PIC  9(005) VALUE 04126.
      *
          10 DCOMW33S-BLOCO-SAIDA.
             15 DCOMW33S-QTDE-TOTAL              PIC  9(010).
             15 DCOMW33S-QTDE-RETORNADA          PIC  9(003).
             15 DCOMW33S-OCORRENCIA              OCCURS 050 TIMES.
                20 DCOMW33S-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
                20 DCOMW33S-IMEIO-ENTRD-DESC     PIC  X(025).
                20 DCOMW33S-IRSUMO-MEIO-ENTRD    PIC  X(015).
                20 DCOMW33S-CSGL-MEIO-ENTRD      PIC  X(006).
                20 DCOMW33S-HULT-ATULZ           PIC  X(026).
                20 DCOMW33S-CTRANS-PROG-ATULZ    PIC  X(008).
      *                                                                *
