      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0I6: DB2PRD.TSPROD_PARMZ_CUSTO                    *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4I6S                                       *
      *    -> FETCH   - DCOM4I6F                                       *
      *    -> INSERT  - DCOM4I6I                                       *
      *    -> UPDATE  - DCOM4I6U                                       *
      *    -> DELETE  - DCOM4I6D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWI6E-HEADER.
          10 DCOMWI6E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWI6E'.
          10 DCOMWI6E-TAM-LAYOUT       PIC  9(005) VALUE 00610.
      *
       05 DCOMWI6E-BLOCO-ENTRADA.
          10 DCOMWI6E-INSTRUCAO                  PIC  X(010).
      *
          10 DCOMWI6E-ENTRADA.
             15 DCOMWI6E-CPRODT                  PIC S9(003) COMP-3.
             15 DCOMWI6E-CSPROD-DESC-COML        PIC S9(003) COMP-3.
             15 DCOMWI6E-DINCL-REG               PIC  X(010).
             15 DCOMWI6E-CTPO-PSSOA              PIC  X(001).
             15 DCOMWI6E-CSIT-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWI6E-DINIC-VGCIA-CUSTO       PIC  X(010).
             15 DCOMWI6E-DFIM-VGCIA-CUSTO        PIC  X(010).
             15 DCOMWI6E-CIDTFD-CUSTO-INTRN      PIC S9(005).
             15 DCOMWI6E-HULT-ATULZ              PIC  X(026).
             15 DCOMWI6E-CFUNC-BDSCO             PIC S9(008) COMP-3.
             15 DCOMWI6E-CTERM                   PIC  X(008).
             15 DCOMWI6E-FILLER                  PIC  X(042).
      *
          10 DCOMWI6E-BLOCO-VARIAVEL.
             15 DCOMWI6E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWI6E-BLOCO-PAGINACAO.
             15 DCOMWI6E-MAX-OCORR                 PIC  9(003).
             15 DCOMWI6E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWI6E-P-INICIAL              VALUE 'I'.
                88 DCOMWI6E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWI6E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWI6E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWI6E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWI6E-BLOCO-RESTART.
             15 DCOMWI6E-CHAVE-INI.
                20 DCOMWI6E-I-CPRODT             PIC S9(003) COMP-3.
                20 DCOMWI6E-I-CSPROD-DESC-COML   PIC S9(005) COMP-3.
                20 DCOMWI6E-I-DINCL-REG          PIC  X(010).
                20 DCOMWI6E-I-CTPO-PSSOA         PIC  X(001).
                20 DCOMWI6E-I-CSIT-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWI6E-I-FILLER               PIC  X(006).
             15 DCOMWI6E-CHAVE-FIM.
                20 DCOMWI6E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWI6E-F-CSPROD-DESC-COML     PIC S9(005) COMP-3.
                20 DCOMWI6E-F-DINCL-REG            PIC  X(010).
                20 DCOMWI6E-F-CTPO-PSSOA           PIC  X(001).
                20 DCOMWI6E-F-CSIT-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWI6E-F-FILLER               PIC  X(006).
      *
          10 DCOMWI6E-FILLER                       PIC  X(112).
      *
