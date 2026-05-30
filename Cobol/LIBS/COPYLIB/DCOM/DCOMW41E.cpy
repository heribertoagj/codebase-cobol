      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
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
       05 DCOMW41E-READER.
          10 DCOMW41E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW41E'.
          10 DCOMW41E-TAM-LAYOUT       PIC  9(005) VALUE 00658.
      *
       05 DCOMW41E-BLOCO-ENTRADA.
          10 DCOMW41E-INSTRUCAO               PIC  X(010).
      *
          10 DCOMW41E-ENTRADA.
             15 DCOMW41E-CPERDC-DESC-COML     PIC X(3).
             15 DCOMW41E-IPERDC-DESC-COML     PIC X(15).
             15 DCOMW41E-HULT-ATULZ           PIC X(26).
             15 DCOMW41E-CTRANS-PROG-ATULZ    PIC X(8).
      *
          10 DCOMW41E-BLOCO-PAGINACAO.
             15 DCOMW41E-MAX-OCORR                 PIC  9(003).
             15 DCOMW41E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW41E-P-INICIAL              VALUE 'I'.
                88 DCOMW41E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW41E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW41E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW41E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW41E-BLOCO-RESTART.
             15 DCOMW41E-CHAVE-INI.
                20 DCOMW41E-I-CPERDC-DESC-COML     PIC  X(003).
                20 FILLER                          PIC  X(200).
             15 DCOMW41E-CHAVE-FIM.
                20 DCOMW41E-F-CPERDC-DESC-COML     PIC  X(003).
                20 FILLER                          PIC  X(200).
      *
          10 DCOMW41E-FILLER                       PIC  X(173).
      *
