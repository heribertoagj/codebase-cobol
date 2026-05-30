      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB026: DB2PRD.TEVNTO_DESC_COML                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM426S                                       *
      *    -> FETCH   - DCOM426F                                       *
      *    -> INSERT  - DCOM426I                                       *
      *    -> UPDATE  - DCOM426U                                       *
      *    -> DELETE  - DCOM426D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW26S-HEADER.
          10 DCOMW26S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWS26'.
          10 DCOMW26S-TAM-LAYOUT       PIC  9(005) VALUE 04826.
      *
          10 DCOMW26S-BLOCO-SAIDA.
             15 DCOMW26S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW26S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW26S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW26S-CEVNTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW26S-IEVNTO-DESC-COML       PIC  X(040).
                20 DCOMW26S-IRSUMO-EVNTO-DESC      PIC  X(020).
                20 DCOMW26S-CTRANS-PROG-ATULZ      PIC  X(008).
                20 DCOMW26S-HULT-ATULZ             PIC  X(026).
      *                                                                *
      *================================================================*
