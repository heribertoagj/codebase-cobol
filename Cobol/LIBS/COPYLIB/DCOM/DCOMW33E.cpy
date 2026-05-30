      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
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
       05 DCOMW33E-READER.
          10 DCOMW33E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW33E'.
          10 DCOMW33E-TAM-LAYOUT       PIC  9(005) VALUE 00783.
      *
       05 DCOMW33E-BLOCO-ENTRADA.
          10 DCOMW33E-INSTRUCAO               PIC  X(010).
      *
          10 DCOMW33E-ENTRADA.
             15 DCOMW33E-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
             15 FILLER                        PIC X(100).
      *
          10 DCOMW33E-BLOCO-PAGINACAO.
             15 DCOMW33E-MAX-OCORR                 PIC  9(003).
             15 DCOMW33E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW33E-P-INICIAL              VALUE 'I'.
                88 DCOMW33E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW33E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW33E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW33E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW33E-BLOCO-RESTART.
             15 DCOMW33E-CHAVE-INI.
                20 DCOMW33E-I-CMEIO-ENTRD-DESC     PIC  S9(003) COMP-3.
                20 FILLER                          PIC  X(200).
             15 DCOMW33E-CHAVE-FIM.
                20 DCOMW33E-F-CMEIO-ENTRD-DESC     PIC  S9(003) COMP-3.
                20 FILLER                          PIC  X(200).
      *
          10 DCOMW33E-FILLER                       PIC  X(250).
      *
