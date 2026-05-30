      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0K9: DB2PRD.TTESTM_AG_OPER                          *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4K9S                                       *
      *    -> FETCH   - DCOM4K9F                                       *
      *    -> INSERT  - DCOM4K9I                                       *
      *    -> UPDATE  - DCOM4K9U                                       *
      *    -> DELETE  - DCOM4K9D
      *================================================================*
      *                                                                *
       05 DCOMWK9E-HEADER.
          10 DCOMWK9E-COD-LAYOUT           PIC  X(008) VALUE 'DCOMWK9E'.
          10 DCOMWK9E-TAM-LAYOUT           PIC  9(005) VALUE 01005.
      *
       05 DCOMWK9E-BLOCO-ENTRADA.
          10 DCOMWK9E-INSTRUCAO            PIC  X(010).
      *
          10 DCOMWK9E-ENTRADA.
             15 DCOMWK9E-CBCO                     PIC S9(003) COMP-3.
             15 DCOMWK9E-CAG-BCRIA                PIC S9(005) COMP-3.
             15 DCOMWK9E-NPSSOA-DESC-COML         PIC S9(009) COMP-3.
             15 DCOMWK9E-DULT-UTILZ               PIC  X(010).
             15 DCOMWK9E-HULT-ATULZ               PIC  X(026).
             15 DCOMWK9E-DINCL-REG                PIC  X(010).
             15 DCOMWK9E-CFUNC-BDSCO              PIC S9(009) COMP-3.
             15 DCOMWK9E-CTERM                    PIC  X(008).
      *
          10 DCOMWK9E-BLOCO-VARIAVEL.
             15 DCOMWK9E-V-CCNPJ-CPF              PIC  9(009).
             15 DCOMWK9E-V-CCTRL-CNPJ-CPF         PIC  9(002).
             15 DCOMWK9E-V-FILLER                 PIC  X(289).

          10 DCOMWK9E-BLOCO-PAGINACAO.
             15 DCOMWK9E-MAX-OCORR                PIC  9(003).
             15 DCOMWK9E-INDICADOR-PAGINACAO      PIC  X(001).
                88 DCOMWK9E-P-INICIAL             VALUE 'I'.
                88 DCOMWK9E-P-PRIMEIRA            VALUE 'P'.
                88 DCOMWK9E-P-SEGUINTE            VALUE 'S'.
                88 DCOMWK9E-P-ANTERIOR            VALUE 'A'.
                88 DCOMWK9E-P-ULTIMA              VALUE 'U'.
      *
          10 DCOMWK9E-BLOCO-RESTART.
             15 DCOMWK9E-CHAVE-INI.
                20 DCOMWK9E-I-CBCO                PIC S9(003) COMP-3.
                20 DCOMWK9E-I-CAG-BCRIA           PIC S9(005) COMP-3.
                20 DCOMWK9E-I-NPSSOA-DESC-COML    PIC S9(009) COMP-3.
                20 DCOMWK9E-I-DULT-UTILZ          PIC  X(010).
                20 FILLER                         PIC  X(190).
             15 DCOMWK9E-CHAVE-FIM.
                20 DCOMWK9E-F-CBCO                PIC S9(003) COMP-3.
                20 DCOMWK9E-F-CAG-BCRIA           PIC S9(005) COMP-3.
                20 DCOMWK9E-F-NPSSOA-DESC-COML    PIC S9(009) COMP-3.
                20 DCOMWK9E-F-DULT-UTILZ          PIC  X(010).
                20 FILLER                         PIC  X(190).
      *
          10 DCOMWK9E-FILLER                      PIC  X(189).
      *
