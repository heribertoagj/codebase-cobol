      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0K5: DB2PRD.TESTOQ_OPER_ATIVO                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4K5S                                       *
      *    -> FETCH   - DCOM4K5F                                       *
      *    -> INSERT  - DCOM4K5I                                       *
      *    -> UPDATE  - DCOM4K5U                                       *
      *    -> DELETE  - DCOM4K5D                                       *
      *================================================================*
      *
       05 DCOMWK5S-HEADER.
          10 DCOMWK5S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWK5S'.
          10 DCOMWK5S-TAM-LAYOUT       PIC  9(005) VALUE 03600.
      *
          10 DCOMWK5S-BLOCO-SAIDA.
             15 DCOMWK5S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWK5S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWK5S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWK5S-CCNPJ-CPF              PIC S9(009) COMP-3.
                20 DCOMWK5S-CFLIAL-CNPJ            PIC S9(005) COMP-3.
                20 DCOMWK5S-CPRODT                 PIC S9(003) COMP-3.
                20 DCOMWK5S-CSPROD-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWK5S-CCONTR-CONVE-DESC      PIC S9(009) COMP-3.
                20 DCOMWK5S-CELMTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWK5S-CCTRL-CNPJ-CPF         PIC S9(002) COMP-3.
                20 DCOMWK5S-QOPER-ATIVO-ESTOQ      PIC S9(009) COMP-3.
                20 DCOMWK5S-VOPER-ATIVO-ESTOQ      PIC S9(013)V9(002)
                                                               COMP-3.
                20 DCOMWK5S-DINCL-REG              PIC  X(010).
                20 DCOMWK5S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWK5S-CTERM                  PIC  X(008).
             15 DCOMWK5S-FILLER                    PIC  X(724).
      *                                                                *
      *================================================================*
