      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB057: DB2PRD.TCONTR_LIM_ROTRO                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM457S                                       *
      *    -> FETCH   - DCOM457F                                       *
      *    -> INSERT  - DCOM457I                                       *
      *    -> UPDATE  - DCOM457U                                       *
      *    -> DELETE  - DCOM457D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW57S-HEADER.
          10 DCOMW57S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW57S'.
          10 DCOMW57S-TAM-LAYOUT       PIC  9(005) VALUE 04376.
      *
          10 DCOMW57S-BLOCO-SAIDA.
             15 DCOMW57S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW57S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW57S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW57S-CSIT-DESC-COML         PIC S9(003) COMP-3.
                20 DCOMW57S-ISIT-DESC-COML         PIC  X(030).
                20 DCOMW57S-IRSUMO-SIT-DESC        PIC  X(015).
                20 DCOMW57S-CSGL-SIT-DESC          PIC  X(006).
                20 DCOMW57S-HULT-ATULZ             PIC  X(026).
                20 DCOMW57S-CTRANS-PROG-ATULZ      PIC  X(008).
      *                                                                *
      *================================================================*
