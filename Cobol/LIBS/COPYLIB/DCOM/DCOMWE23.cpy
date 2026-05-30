      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB023: DB2PRD.TELMTO_DESC_COML                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM232S                                       *
      *    -> FETCH   - DCOM232F                                       *
      *    -> INSERT  - DCOM232I                                       *
      *    -> UPDATE  - DCOM232U                                       *
      *    -> DELETE  - DCOM232D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW23E-HEADER.
          10 DCOMW23E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW23E'.
          10 DCOMW23E-TAM-LAYOUT       PIC  9(005) VALUE 00497.
      *
       05 DCOMW23E-BLOCO-ENTRADA.
          10 DCOMW23E-INSTRUCAO                    PIC  X(010).
          10 DCOMW23E-ENTRADA.
             15 DCOMW23E-CELMTO-DESC-COML          PIC  S9(003) COMP-3.
             15 DCOMW23E-IELMTO-DESC-COML          PIC  X(030).
             15 DCOMW23E-HULT-ATULZ                PIC  X(026).
             15 DCOMW23E-CTRANS-PROG-ATULZ         PIC  X(008).
          10 DCOMW23E-BLOCO-VARIAVEL.
             15 DCOMW23E-V-FILLER                  PIC  X(300).
          10 DCOMW23E-BLOCO-PAGINACAO.
             15 DCOMW23E-MAX-OCORR                 PIC  9(003).
             15 DCOMW23E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW23E-P-INICIAL              VALUE 'I'.
                88 DCOMW23E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW23E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW23E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW23E-P-ULTIMA               VALUE 'U'.
          10 DCOMW23E-BLOCO-RESTART.
             15 DCOMW23E-CHAVE-INI.
                20 DCOMW23E-I-CELMTO-DESC-COML     PIC  S9(003) COMP-3.
             15 DCOMW23E-CHAVE-FIM.
                20 DCOMW23E-F-CELMTO-DESC-COML     PIC  S9(003) COMP-3.
          10 DCOMW23E-FILLER                       PIC  X(100).
      *                                                                *
