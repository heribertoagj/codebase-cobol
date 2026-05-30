      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB023: DB2PRD.TELMTO_DESC_COML                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM423S                                       *
      *    -> FETCH   - DCOM423F                                       *
      *    -> INSERT  - DCOM423I                                       *
      *    -> UPDATE  - DCOM423U                                       *
      *    -> DELETE  - DCOM423D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW23S-HEADER.
          10 DCOMW23S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW23S'.
          10 DCOMW23S-TAM-LAYOUT       PIC  9(005) VALUE 03450.
      *
          10 DCOMW23S-BLOCO-SAIDA.
             15 DCOMW23S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW23S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW23S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW23S-CELMTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW23S-IELMTO-DESC-COML       PIC  X(030).
                20 DCOMW23S-HULT-ATULZ             PIC  X(026).
                20 DCOMW23S-CTRANS-PROG-ATULZ      PIC  X(008).

             15 DCOMW23S-FILLER                    PIC  X(124).
      *
