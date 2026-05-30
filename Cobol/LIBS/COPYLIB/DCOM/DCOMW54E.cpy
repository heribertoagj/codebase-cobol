      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB054: DB2PRD.TROTRO_ELMTO_DESC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM454S                                       *
      *    -> FETCH   - DCOM454F                                       *
      *    -> INSERT  - DCOM454I                                       *
      *    -> UPDATE  - DCOM454U                                       *
      *    -> DELETE  - DCOM454D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW54E-HEADER.
          10 DCOMW54E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW54E'.
          10 DCOMW54E-TAM-LAYOUT       PIC  9(005) VALUE 00534.
      *
       05 DCOMW54E-BLOCO-ENTRADA.
          10 DCOMW54E-INSTRUCAO                    PIC  X(010).
          10 DCOMW54E-ENTRADA.
             15 DCOMW54E-CELMTO-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMW54E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMW54E-CTPO-DESC-COML            PIC S9(003) COMP-3.
             15 DCOMW54E-CMEIO-ENTRD-DESC          PIC S9(003) COMP-3.
             15 DCOMW54E-CTPO-AGPTO-ROTRO          PIC S9(003) COMP-3.
             15 DCOMW54E-CINDCD-OBRIG-AGPTO        PIC  X(001).
             15 DCOMW54E-CTRANS-PROG-ATULZ         PIC  X(008).
             15 DCOMW54E-HULT-ATULZ                PIC  X(026).
          10 DCOMW54E-BLOCO-VARIAVEL.
             15 DCOMW54E-V-CSPROD-DESC-COML        PIC S9(003) COMP-3.
             15 DCOMW54E-V-FILLER                  PIC  X(297).
          10 DCOMW54E-BLOCO-PAGINACAO.
             15 DCOMW54E-MAX-OCORR                 PIC  9(003).
             15 DCOMW54E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW54E-P-INICIAL              VALUE 'I'.
                88 DCOMW54E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW54E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW54E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW54E-P-ULTIMA               VALUE 'U'.
          10 DCOMW54E-BLOCO-RESTART.
             15 DCOMW54E-CHAVE-INI.
                20 DCOMW54E-I-CELMTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW54E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW54E-I-CTPO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW54E-I-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
                20 DCOMW54E-I-CTPO-AGPTO-ROTRO     PIC S9(003) COMP-3.
             15 DCOMW54E-CHAVE-FIM.
                20 DCOMW54E-F-CELMTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW54E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW54E-F-CTPO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW54E-F-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
                20 DCOMW54E-F-CTPO-AGPTO-ROTRO     PIC S9(003) COMP-3.
          10 DCOMW54E-FILLER                       PIC  X(143).
      *                                                                *
