      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB085: DB2PRD.TTPO_ISENC_DESC                         *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM485S                                       *
      *    -> FETCH   - DCOM485F                                       *
      *    -> INSERT  - DCOM485I                                       *
      *    -> UPDATE  - DCOM485U                                       *
      *    -> DELETE  - DCOM485D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW85S-HEADER.
          10 DCOMW85S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW85S'.
          10 DCOMW85S-TAM-LAYOUT       PIC  9(005) VALUE  4076.
      *
          10 DCOMW85S-BLOCO-SAIDA.
             15 DCOMW85S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW85S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW85S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW85S-CTPO-ISENC-DESC        PIC S9(003) COMP-3.
                20 DCOMW85S-ITPO-ISENC-DESC        PIC  X(030).
                20 DCOMW85S-IRSUMO-TPO-ISENC       PIC  X(015).
                20 DCOMW85S-HULT-ATULZ             PIC  X(026).
                20 DCOMW85S-CTRANS-PROG-ATULZ      PIC  X(008).
      *                                                                *
