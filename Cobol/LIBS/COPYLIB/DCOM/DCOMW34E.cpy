      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB034: DB2PRD.TMEIO_LIBRC_DESC                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM434S                                       *
      *    -> FETCH   - DCOM434F                                       *
      *    -> INSERT  - DCOM434I                                       *
      *    -> UPDATE  - DCOM434U                                       *
      *    -> DELETE  - DCOM434D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW34E-READER.
          10 DCOMW34E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW34E'.
          10 DCOMW34E-TAM-LAYOUT       PIC  9(005) VALUE 00783.
      *
       05 DCOMW34E-BLOCO-ENTRADA.
          10 DCOMW34E-INSTRUCAO               PIC  X(010).
      *
          10 DCOMW34E-ENTRADA.
             15 DCOMW34E-CMEIO-LIBRC-DESC     PIC S9(003) COMP-3.
             15 FILLER                        PIC X(100).
      *
          10 DCOMW34E-BLOCO-PAGINACAO.
             15 DCOMW34E-MAX-OCORR                 PIC  9(003).
             15 DCOMW34E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW34E-P-INICIAL              VALUE 'I'.
                88 DCOMW34E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW34E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW34E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW34E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW34E-BLOCO-RESTART.
             15 DCOMW34E-CHAVE-INI.
                20 DCOMW34E-I-CMEIO-LIBRC-DESC     PIC  S9(003) COMP-3.
                20 FILLER                          PIC  X(200).
             15 DCOMW34E-CHAVE-FIM.
                20 DCOMW34E-F-CMEIO-LIBRC-DESC     PIC  S9(003) COMP-3.
                20 FILLER                          PIC  X(200).
      *
          10 DCOMW34E-FILLER                       PIC  X(250).
      *
