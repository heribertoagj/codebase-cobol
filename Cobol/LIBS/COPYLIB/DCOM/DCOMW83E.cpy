      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB083: DB2PRD.TTPO_DESC_COML                          *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM483S                                       *
      *    -> FETCH   - DCOM483F                                       *
      *    -> INSERT  - DCOM483I                                       *
      *    -> UPDATE  - DCOM483U                                       *
      *    -> DELETE  - DCOM483D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW83E-HEADER.
          10 DCOMW83E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW83E'.
          10 DCOMW83E-TAM-LAYOUT       PIC  9(005) VALUE 00547.
      *
       05 DCOMW83E-BLOCO-ENTRADA.
          10 DCOMW83E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW83E-ENTRADA.
             15 DCOMW83E-CTPO-DESC-COML            PIC S9(003) COMP-3.
             15 DCOMW83E-ITPO-DESC-COML            PIC  X(030).
             15 DCOMW83E-IRSUMO-TPO-DESC           PIC  X(010).
             15 DCOMW83E-CSGL-TPO-DESC-COML        PIC  X(006).
             15 DCOMW83E-HULT-ATULZ                PIC  X(026).
             15 DCOMW83E-CTRANS-PROG-ATULZ         PIC  X(008).
      *
          10 DCOMW83E-BLOCO-VARIAVEL.
             15 DCOMW83E-V-FILLER                  PIC  X(300).
      *
          10 DCOMW83E-BLOCO-PAGINACAO.
             15 DCOMW83E-MAX-OCORR                 PIC  9(003).
             15 DCOMW83E-TOTALIZAR                 PIC  X(001).
             15 DCOMW83E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW83E-P-INICIAL              VALUE 'I'.
                88 DCOMW83E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW83E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW83E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW83E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW83E-BLOCO-RESTART.
             15 DCOMW83E-CHAVE-INI.
                20 DCOMW83E-I-CTPO-DESC-COML       PIC S9(003) COMP-3.
             15 DCOMW83E-CHAVE-FIM.
                20 DCOMW83E-F-CTPO-DESC-COML       PIC S9(003) COMP-3.
      *                                                                *
          10 DCOMW83E-FILLER                       PIC  X(133).
