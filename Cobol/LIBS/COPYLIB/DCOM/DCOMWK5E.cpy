      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB091: DB2PRD.TSPROD_DESC_COML                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4K5S                                       *
      *    -> FETCH   - DCOM4K5F                                       *
      *    -> INSERT  - DCOM4K5I                                       *
      *    -> UPDATE  - DCOM4K5U                                       *
      *    -> DELETE  - DCOM4K5D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWK5E-HEADER.
          10 DCOMWK5E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWK5E'.
          10 DCOMWK5E-TAM-LAYOUT       PIC  9(005) VALUE 01000.
      *
       05 DCOMWK5E-BLOCO-ENTRADA.
          10 DCOMWK5E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWK5E-ENTRADA.
             15 DCOMWK5E-CCNPJ-CPF                 PIC S9(009) COMP-3.
             15 DCOMWK5E-CFLIAL-CNPJ               PIC S9(005) COMP-3.
             15 DCOMWK5E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMWK5E-CSPROD-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWK5E-CCONTR-CONVE-DESC         PIC S9(009) COMP-3.
             15 DCOMWK5E-CELMTO-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWK5E-CCTRL-CNPJ-CPF            PIC S9(002) COMP-3.
             15 DCOMWK5E-QOPER-ATIVO-ESTOQ         PIC S9(009) COMP-3.
             15 DCOMWK5E-VOPER-ATIVO-ESTOQ         PIC S9(013)V9(002)
                                                               COMP-3.
             15 DCOMWK5E-DINCL-REG                 PIC  X(010).
             15 DCOMWK5E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWK5E-CTERM                     PIC  X(008).
      *
          10 DCOMWK5E-BLOCO-VARIAVEL.
             15 DCOMWK5E-V-FILLER                  PIC  X(300).

          10 DCOMWK5E-BLOCO-PAGINACAO.
             15 DCOMWK5E-MAX-OCORR                 PIC  9(003).
             15 DCOMWK5E-TOTALIZAR                 PIC  X(001).
             15 DCOMWK5E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWK5E-P-INICIAL              VALUE 'I'.
                88 DCOMWK5E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWK5E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWK5E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWK5E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWK5E-BLOCO-RESTART.
             15 DCOMWK5E-CHAVE-INI.
                20 DCOMWK5E-I-CCNPJ-CPF            PIC S9(009) COMP-3.
                20 DCOMWK5E-I-CFLIAL-CNPJ          PIC S9(005) COMP-3.
                20 DCOMWK5E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWK5E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 FILLER                          PIC  X(200).
             15 DCOMWK5E-CHAVE-FIM.
                20 DCOMWK5E-F-CCNPJ-CPF            PIC S9(009) COMP-3.
                20 DCOMWK5E-F-CFLIAL-CNPJ          PIC S9(005) COMP-3.
                20 DCOMWK5E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWK5E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 FILLER                          PIC  X(200).
      *
          10 DCOMWK5E-FILLER                       PIC  X(191).
      *
