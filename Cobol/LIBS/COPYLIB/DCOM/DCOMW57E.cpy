      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB057: DB2PRD.TSIT_DESC_COML                          *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM457S                                       *
      *    -> FETCH   - DCOM457F                                       *
      *    -> INSERT  - DCOM457I                                       *
      *    -> UPDATE  - DCOM457U                                       *
      *    -> DELETE  - DCOM457D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW57E-HEADER.
          10 DCOMW57E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW57E'.
          10 DCOMW57E-TAM-LAYOUT       PIC  9(005) VALUE 00497.
      *
       05 DCOMW57E-BLOCO-ENTRADA.
          10 DCOMW57E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW57E-ENTRADA.
             15 DCOMW57E-CSIT-DESC-COML            PIC  S9(003) COMP-3.
             15 DCOMW57E-ISIT-DESC-COML            PIC  X(030).
             15 DCOMW57E-IRSUMO-SIT-DESC           PIC  X(015).
             15 DCOMW57E-CSGL-SIT-DESC             PIC  X(006).
             15 DCOMW57E-HULT-ATULZ                PIC  X(026).
             15 DCOMW57E-CTRANS-PROG-ATULZ         PIC  X(008).
             15 DCOMW57E-FILLER                    PIC  X(029).
      *
          10 DCOMW57E-BLOCO-VARIAVEL.
             15 DCOMW57E-V-FILLER                  PIC  X(300).
      *
          10 DCOMW57E-BLOCO-PAGINACAO.
             15 DCOMW57E-MAX-OCORR                 PIC  9(003).
             15 DCOMW57E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW57E-P-INICIAL              VALUE 'I'.
                88 DCOMW57E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW57E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW57E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW57E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW57E-BLOCO-RESTART.
             15 DCOMW57E-CHAVE-INI.
                20 DCOMW57E-I-CSIT-DESC-COML       PIC  S9(003) COMP-3.
             15 DCOMW57E-CHAVE-FIM.
                20 DCOMW57E-F-CSIT-DESC-COML       PIC  S9(003) COMP-3.
      *
          10 DCOMW57E-FILLER                       PIC  X(050).
