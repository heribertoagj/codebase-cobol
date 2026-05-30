      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0O2 - DB2PRD.TTAC_OPER_CLI          *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************

       01  O2-ARGUMENTOS-ENTRADA.

       05  O2-COLUNAS-DA-TABELA.
           10  O2-CBCO                 PIC S9(03)      COMP-3.
           10  O2-CAG-BCRIA            PIC S9(05)      COMP-3.
           10  O2-CCTA-BCRIA-CLI       PIC S9(13)      COMP-3.
           10  O2-CPRODT               PIC S9(03)      COMP-3.
           10  O2-CSPROD-DESC-COML     PIC S9(03)      COMP-3.
           10  O2-CMEIO-ENTRD-DESC     PIC S9(03)      COMP-3.
           10  O2-HINIC-VGCIA          PIC  X(26).
           10  O2-CTPO-COBR-TAC        PIC S9(03)      COMP-3.
           10  O2-CIDTFD-FIXO-TAC      PIC S9(05)      COMP-3.
           10  O2-PTAC-VARVL           PIC S9(03)V9(2) COMP-3.
           10  O2-CIDTFD-MIN-TAC       PIC S9(05)      COMP-3.
           10  O2-CIDTFD-MAX-TAC       PIC S9(05)      COMP-3.
           10  O2-HFIM-VGCIA           PIC  X(26).
           10  O2-HULT-ATULZ           PIC  X(26).
           10  O2-CFUNC-BDSCO          PIC S9(09)      COMP-3.
           10  O2-CTERM                PIC  X(08).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  O2-QTDE-A-DESPREZAR         PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  O2-INSTRUCAO                PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  O2-RETORNO.
       05  O2-COD-RETORNO              PIC 9(04).
       05  O2-MENSAGEM                 PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  O2-PROXIMO-RESTART          PIC 9(05).
       05  O2-QTDE-RETORNADA           PIC 9(03).
       05  O2-QTDE-TOTAL               PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  O2-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TO2.
           10  O2RT-CBCO               PIC S9(03)      COMP-3.
           10  O2RT-CAG-BCRIA          PIC S9(05)      COMP-3.
           10  O2RT-CCTA-BCRIA-CLI     PIC S9(13)      COMP-3.
           10  O2RT-CPRODT             PIC S9(03)      COMP-3.
           10  O2RT-CSPROD-DESC-COML   PIC S9(03)      COMP-3.
           10  O2RT-CMEIO-ENTRD-DESC   PIC S9(03)      COMP-3.
           10  O2RT-HINIC-VGCIA        PIC  X(26).
           10  O2RT-CTPO-COBR-TAC      PIC S9(03)      COMP-3.
           10  O2RT-CIDTFD-FIXO-TAC    PIC S9(05)      COMP-3.
           10  O2RT-PTAC-VARVL         PIC S9(03)V9(2) COMP-3.
           10  O2RT-CIDTFD-MIN-TAC     PIC S9(05)      COMP-3.
           10  O2RT-CIDTFD-MAX-TAC     PIC S9(05)      COMP-3.
           10  O2RT-HFIM-VGCIA         PIC  X(26).
           10  O2RT-HULT-ATULZ         PIC  X(26).
           10  O2RT-CFUNC-BDSCO        PIC S9(09)      COMP-3.
           10  O2RT-CTERM              PIC  X(08).
