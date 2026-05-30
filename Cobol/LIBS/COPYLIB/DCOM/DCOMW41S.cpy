      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB041: DB2PRD.TPERDC_DESC_COML                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM441S                                       *
      *    -> FETCH   - DCOM441F                                       *
      *    -> INSERT  - DCOM441I                                       *
      *    -> UPDATE  - DCOM441U                                       *
      *    -> DELETE  - DCOM441D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW41S-HEADER.
          10 DCOMW41S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW41S'.
          10 DCOMW41S-TAM-LAYOUT       PIC  9(005) VALUE 12626.
      *
          10 DCOMW41S-BLOCO-SAIDA.
             15 DCOMW41S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW41S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW41S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW41S-CPERDC-DESC-COML       PIC  X(003).
                20 DCOMW41S-IPERDC-DESC-COML       PIC  X(015).
                20 DCOMW41S-HULT-ATULZ             PIC  X(026).
                20 DCOMW41S-CTRANS-PROG-ATULZ      PIC  X(008).
                20 DCOMW41S-FILLER                 PIC  X(200).
      *                                                                *
