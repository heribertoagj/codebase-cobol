      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB066: DB2PRD.TSPROD_MEIO_PGTO                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM466S                                       *
      *    -> FETCH   - DCOM466F                                       *
      *    -> INSERT  - DCOM466I                                       *
      *    -> UPDATE  - DCOM466U                                       *
      *    -> DELETE  - DCOM466D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW66E-HEADER.
          10 DCOMW66E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW66E'.
          10 DCOMW66E-TAM-LAYOUT       PIC  9(005) VALUE 00870.
      *
       05 DCOMW66E-BLOCO-ENTRADA.
          10 DCOMW66E-INSTRUCAO                    PIC  X(010).
          10 DCOMW66E-ENTRADA.
             15 DCOMW66E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMW66E-CSPROD-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMW66E-CINDCD-VCTO-PCELA         PIC  X(001).
             15 DCOMW66E-CMEIO-PGTO-DESC           PIC S9(003) COMP-3.
             15 DCOMW66E-HINIC-MEIO-PGTO           PIC  X(026).
             15 DCOMW66E-CINDCD-RESP-PGTO          PIC  X(001).
             15 DCOMW66E-TDEB-PCELA-VENCD          PIC S9(003) COMP-3.
             15 DCOMW66E-CINDCD-PRZ-PCELA          PIC  X(002).
             15 DCOMW66E-HFIM-MEIO-PGTO            PIC  X(026).
             15 DCOMW66E-HULT-ATULZ                PIC  X(026).
             15 DCOMW66E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMW66E-CTERM                     PIC  X(008).
             15 DCOMW66E-CINDCD-PGTO-OPCAO         PIC  X(001).
             15 DCOMW66E-CINDCD-OPCAO-BLETO        PIC  X(001).
          10 DCOMW66E-BLOCO-VARIAVEL.
             15 DCOMW66E-V-FILLER                  PIC  X(300).
          10 DCOMW66E-BLOCO-PAGINACAO.
             15 DCOMW66E-MAX-OCORR                 PIC  9(003).
             15 DCOMW66E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW66E-P-INICIAL              VALUE 'I'.
                88 DCOMW66E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW66E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW66E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW66E-P-ULTIMA               VALUE 'U'.
          10 DCOMW66E-BLOCO-RESTART.
             15 DCOMW66E-CHAVE-INI.
                20 DCOMW66E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW66E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW66E-I-CINDCD-VCTO-PCELA    PIC  X(001).
                20 DCOMW66E-I-CMEIO-PGTO-DESC      PIC S9(003) COMP-3.
                20 DCOMW66E-I-HINIC-MEIO-PGTO      PIC  X(026).
             15 DCOMW66E-CHAVE-FIM.
                20 DCOMW66E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW66E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW66E-F-CINDCD-VCTO-PCELA    PIC  X(001).
                20 DCOMW66E-F-CMEIO-PGTO-DESC      PIC S9(003) COMP-3.
                20 DCOMW66E-F-HINIC-MEIO-PGTO      PIC  X(026).
          10 DCOMW66E-FILLER                       PIC  X(138).
      *                                                                *
