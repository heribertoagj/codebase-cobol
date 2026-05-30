      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0L3: DB2PRD.TPARMZ_CUSTO_EFETV                      *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4L3S                                       *
      *    -> FETCH   - DCOM4L3F                                       *
      *    -> INSERT  - DCOM4L3I                                       *
      *    -> UPDATE  - DCOM4L3U                                       *
      *    -> DELETE  - DCOM4L3D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWL3E-HEADER.
          10 DCOMWL3E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWL3E'.
          10 DCOMWL3E-TAM-LAYOUT       PIC  9(005) VALUE 00490.
      *
       05 DCOMWL3E-BLOCO-ENTRADA.
          10 DCOMWL3E-INSTRUCAO                    PIC  X(010).
          10 DCOMWL3E-ENTRADA.
             15 DCOMWL3E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMWL3E-CSPROD-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWL3E-CINDCD-PSSOA-TOMAD        PIC  X(001).
             15 DCOMWL3E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWL3E-HATULZ                    PIC  X(026).
          10 DCOMWL3E-BLOCO-VARIAVEL.
             15 DCOMWL3E-V-FILLER                  PIC  X(300).
          10 DCOMWL3E-BLOCO-PAGINACAO.
             15 DCOMWL3E-MAX-OCORR                 PIC  9(003).
             15 DCOMWL3E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWL3E-P-INICIAL              VALUE 'I'.
                88 DCOMWL3E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWL3E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWL3E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWL3E-P-ULTIMA               VALUE 'U'.
          10 DCOMWL3E-BLOCO-RESTART.
             15 DCOMWL3E-CHAVE-INI.
                20 DCOMWL3E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWL3E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
             15 DCOMWL3E-CHAVE-FIM.
                20 DCOMWL3E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWL3E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
          10 DCOMWL3E-FILLER                       PIC  X(119).
      *                                                                *
