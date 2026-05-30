      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0I6: DB2PRD.TTX_OPER_DESC_CLI                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4I6S                                       *
      *    -> FETCH   - DCOM4I6F                                       *
      *    -> INSERT  - DCOM4I6I                                       *
      *    -> UPDATE  - DCOM4I6U                                       *
      *    -> DELETE  - DCOM4I6D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWI6S-HEADER.
          10 DCOMWI6S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWI6S'.
          10 DCOMWI6S-TAM-LAYOUT       PIC  9(005) VALUE 04100.
      *
          10 DCOMWI6S-BLOCO-SAIDA.
             15 DCOMWI6S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWI6S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWI6S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWI6S-CPRODT                  PIC S9(003) COMP-3.
                20 DCOMWI6S-CSPROD-DESC-COML        PIC S9(003) COMP-3.
                20 DCOMWI6S-DINCL-REG               PIC  X(010).
                20 DCOMWI6S-CTPO-PSSOA              PIC  X(001).
                20 DCOMWI6S-CSIT-DESC-COML          PIC S9(003) COMP-3.
                20 DCOMWI6S-DINIC-VGCIA-CUSTO       PIC  X(010).
                20 DCOMWI6S-DFIM-VGCIA-CUSTO        PIC  X(010).
                20 DCOMWI6S-CIDTFD-CUSTO-INTRN      PIC S9(005).
                20 DCOMWI6S-HULT-ATULZ              PIC  X(026).
                20 DCOMWI6S-CFUNC-BDSCO             PIC S9(008) COMP-3.
                20 DCOMWI6S-CTERM                   PIC  X(008).
      *                                                                *
          10 DCOMWI6S-FILLER                       PIC  X(024).
      *                                                                *
      *================================================================*
