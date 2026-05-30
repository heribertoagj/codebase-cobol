      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0N4 - DB2PRD.TOPER_DESC_RCUSA       *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7650 - SELECT         - DCOM7651 - FETCH             *
      *    - DCOM7652 - INSERT         - DCOM7653 - UPDATE            *
      *    - DCOM7654 - DELETE                                        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  N4-ARGUMENTOS-ENTRADA.

       05  N4-COLUNAS-DA-TABELA.
           10  N4-DANO-OPER-DESC       PIC S9(004) COMP-3.
           10  N4-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
           10  N4-CSIT-DESC-COML       PIC S9(003) COMP-3.
           10  N4-HSIT-DESC-COML       PIC  X(026).
           10  N4-CBCO                 PIC S9(003) COMP-3.
           10  N4-CAG-BCRIA            PIC S9(005) COMP-3.
           10  N4-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  N4-CBCO-OPER-OPER       PIC S9(003) COMP-3.
           10  N4-CAG-OPER-OPER        PIC S9(005) COMP-3.
           10  N4-CPRODT               PIC S9(003) COMP-3.
           10  N4-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N4-CTPO-DESC-COML       PIC S9(003) COMP-3.
           10  N4-CORIGE-OPER-DESC     PIC S9(001) COMP-3.
           10  N4-CELMTO-DESC-COML     PIC S9(003) COMP-3.
           10  N4-NSMULA-OPER-DESC     PIC S9(009) COMP-3.
           10  N4-VOPER-DESC-COML      PIC S9(015)V9(2) COMP-3.
           10  N4-DINIC-OPER-DESC      PIC  X(010).
           10  N4-DVCTO-FNAL-OPER      PIC  X(010).
           10  N4-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
           10  N4-QPCELA-OPER          PIC S9(005) COMP-3.
           10  N4-QTITLO-CHEQ-PCELA    PIC S9(009) COMP-3.
           10  N4-VJURO-OPER-DESC      PIC S9(015)V9(2) COMP-3.
           10  N4-VJURO-CUSTO-OPER     PIC S9(015)V9(2) COMP-3.
           10  N4-VTARIF-REG-TITLO     PIC S9(015)V9(2) COMP-3.
           10  N4-VTAC-OPER-DESC       PIC S9(015)V9(2) COMP-3.
           10  N4-VPSENT-OPER-DESC     PIC S9(015)V9(2) COMP-3.
           10  N4-VIOF-OPER-DESC       PIC S9(015)V9(2) COMP-3.
           10  N4-VLIQ-OPER-DESC       PIC S9(015)V9(2) COMP-3.
           10  N4-HULT-ATULZ           PIC  X(026).
           10  N4-CFUNC-BDSCO          PIC S9(009) COMP-3.
           10  N4-CTERM                PIC  X(008).
           10  N4-CUSUAR-MANUT         PIC  X(009).
           10  N4-CTERM-ULT-ATULZ      PIC  X(008).
           10  N4-CCONTR-CORP-OPER     PIC S9(007) COMP-3.
           10  N4-DINCL-REG            PIC  X(010).
           10  N4-CCART-LIM-CREDT      PIC  X(005).
           10  N4-CCHAVE-ELMTO-DESC    PIC  X(045).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  N4-QTDE-A-DESPREZAR         PIC  9(005).
       05  N4-START-COUNT              PIC  X(001).

      * PREENCHER PARA PARCIAL-03 ----------*
       05  N4-DINCL-REG-INI            PIC  X(010).
       05  N4-DINCL-REG-FIM            PIC  X(010).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  N4-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  N4-RETORNO.
       05  N4-COD-RETORNO              PIC  9(04).
       05  N4-MENSAGEM                 PIC  X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  N4-PROXIMO-RESTART          PIC  9(05).
       05  N4-QTDE-RETORNADA           PIC  9(03).
       05  N4-QTDE-TOTAL               PIC  9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  N4-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TN4.
           10  N4RT-DANO-OPER-DESC     PIC S9(004) COMP-3.
           10  N4RT-NSEQ-OPER-DESC     PIC S9(009) COMP-3.
           10  N4RT-CSIT-DESC-COML     PIC S9(003) COMP-3.
           10  N4RT-HSIT-DESC-COML     PIC  X(026).
           10  N4RT-CBCO               PIC S9(003) COMP-3.
           10  N4RT-CAG-BCRIA          PIC S9(005) COMP-3.
           10  N4RT-CCTA-BCRIA-CLI     PIC S9(013) COMP-3.
           10  N4RT-CBCO-OPER-OPER     PIC S9(003) COMP-3.
           10  N4RT-CAG-OPER-OPER      PIC S9(005) COMP-3.
           10  N4RT-CPRODT             PIC S9(003) COMP-3.
           10  N4RT-CSPROD-DESC-COML   PIC S9(003) COMP-3.
           10  N4RT-CTPO-DESC-COML     PIC S9(003) COMP-3.
           10  N4RT-CORIGE-OPER-DESC   PIC S9(001) COMP-3.
           10  N4RT-CELMTO-DESC-COML   PIC S9(003) COMP-3.
           10  N4RT-NSMULA-OPER-DESC   PIC S9(009) COMP-3.
           10  N4RT-VOPER-DESC-COML    PIC S9(015)V9(2) COMP-3.
           10  N4RT-DINIC-OPER-DESC    PIC  X(010).
           10  N4RT-DVCTO-FNAL-OPER    PIC  X(010).
           10  N4RT-CMEIO-ENTRD-DESC   PIC S9(003) COMP-3.
           10  N4RT-QPCELA-OPER        PIC S9(005) COMP-3.
           10  N4RT-QTITLO-CHEQ-PCELA  PIC S9(009) COMP-3.
           10  N4RT-VJURO-OPER-DESC    PIC S9(015)V9(2) COMP-3.
           10  N4RT-VJURO-CUSTO-OPER   PIC S9(015)V9(2) COMP-3.
           10  N4RT-VTARIF-REG-TITLO   PIC S9(015)V9(2) COMP-3.
           10  N4RT-VTAC-OPER-DESC     PIC S9(015)V9(2) COMP-3.
           10  N4RT-VPSENT-OPER-DESC   PIC S9(015)V9(2) COMP-3.
           10  N4RT-VIOF-OPER-DESC     PIC S9(015)V9(2) COMP-3.
           10  N4RT-VLIQ-OPER-DESC     PIC S9(015)V9(2) COMP-3.
           10  N4RT-HULT-ATULZ         PIC  X(026).
           10  N4RT-CFUNC-BDSCO        PIC S9(009) COMP-3.
           10  N4RT-CTERM              PIC  X(008).
           10  N4RT-CUSUAR-MANUT       PIC  X(009).
           10  N4RT-CTERM-ULT-ATULZ    PIC  X(008).
           10  N4RT-CCONTR-CORP-OPER   PIC S9(007) COMP-3.
           10  N4RT-DINCL-REG          PIC  X(010).
           10  N4RT-CCART-LIM-CREDT    PIC  X(005).
           10  N4RT-CCHAVE-ELMTO-DESC  PIC  X(045).

