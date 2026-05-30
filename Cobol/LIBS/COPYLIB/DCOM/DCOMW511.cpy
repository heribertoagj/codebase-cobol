      *================================================================*
      * NOME BOOK : DCOMW511                                           *
      * DESCRICAO : COMUNICACAO CICS X IMS (DCOM3RTC X DCOM3511)       *
      * DATA      : 07/05/2024                                         *
      * AUTOR     : LUCIANDRA SILVEIRA                                 *
      * EMPRESA   : WIPRO DO BRASIL TECNOLOGIA                         *
      ******************************************************************
      * OBJETIVO  : AREA DE COMUNICACAO DO MODULO DCOM3511 (IMS)       *
      *             ACIONAR MODULOS IMS CONFORME LAYOUT CORRESPONDENTE *
      *             DCOMW511-FUNCAO:                                   *
      *             001 - ACESSAR NTAX3300                             *
      *             002 - ACESSAR NTAX5030                             *
      *             003 - ACESSAR CTAS9011                             *
      *             004 - ACESSAR DCOM5110 - CREDITO LIBERAC.(COSA2020)*
      *================================================================*
      *
       01  DCOMW511-AREA.
           05 DCOMW511-FUNCAO                 PIC  9(003).
           05 DCOMW511-DADOS                  PIC  X(3900).
      *
      *--> AREA DE ACESSO AO MODULO NTAX5030
      *--> =================================
           05 DCOMW511-DADOS-NTAX             REDEFINES DCOMW511-DADOS.
              10 DCOMW511-NTAX-ENTRADA.
                 15 DCOMW511-E1-CIDTFD-INDCD  PIC  9(005).
                 15 DCOMW511-E1-DATA-CONS     PIC  X(010).
      *
              10 DCOMW511-NTAX-SAIDA.
                 15 DCOMW511-S1-CIDTFD-INDCD  PIC  9(005).
                 15 DCOMW511-S1-CTPO-INDECONM PIC  X(001).
                 15 DCOMW511-S1-ITPO-INDECONM PIC  X(030).
                 15 DCOMW511-S1-CTPO-PERDCIND PIC  X(001).
                 15 DCOMW511-S1-DINIC-CONS    PIC  X(010).
                 15 DCOMW511-S1-DFIM-CONS     PIC  X(010).
                 15 DCOMW511-S1-PTX           PIC  9(003)V9(007).
                 15 DCOMW511-S1-VTAXA-ATUAL   PIC  9(015).
                 15 DCOMW511-S1-QCASAS-DEC    PIC  9(003).
              10 DCOMW511-NTAX-FILLER         PIC  X(3800).
      *
      *--> AREA DE ACESSO AO MODULO CTES9011
      *--> =================================
           05 DCOMW511-DADOS-CTES             REDEFINES DCOMW511-DADOS.
              10 DCOMW511-CTES-ENTRADA.
                 15 DCOMW511-E2-FUNCAO-CTES   PIC  9(001).
      *-->          1 -> VALIDAR
      *-->          2 -> VINCULAR    AO CONTRATO
      *-->          3 -> DESVINCULAR AO CONTRATO
      *-->          4 -> CONSULTAR COTACAO VINCULADA AO CONTRATO
                 15 DCOMW511-E2-CFUNC-BDSCO   PIC  9(009).
                 15 DCOMW511-E2-DEPTO-BDSCO   PIC  9(005).
                 15 DCOMW511-E2-CBCO          PIC  9(003).
                 15 DCOMW511-E2-CAG-BCRIA     PIC  9(005).
                 15 DCOMW511-E2-CCTA-CORR     PIC  9(013).
                 15 DCOMW511-E2-DCOTAC-TX-ESPCL
                                              PIC  X(010).
                 15 DCOMW511-E2-CCOTAC-TX-ESPCL
                                              PIC  9(005).
                 15 DCOMW511-E2-DCELEB-CONTR  PIC  X(010).
                 15 DCOMW511-E2-CCONTR-EMPTM  PIC  9(009).
                 15 DCOMW511-E2-QDIA-UTIL     PIC  9(003).
                 15 DCOMW511-E2-CPERDC-TX     PIC  9(001).
                 15 DCOMW511-E2-PTX-COTAC     PIC  9(006)V9(007).
      *
              10 DCOMW511-CTES-SAIDA.
      *-->     CODIGO DA COTACAO VINCULADA AO CONTRATO
                 15 DCOMW511-S2-CCOTAC-TX-ESPCL
                                              PIC  9(005).
      *-->     TAXA DE JUROS COTADA NA TESOURARIA
                 15 DCOMW511-S2-PTX-COTAC     PIC  9(006)V9(007).
      *-->     SPREAD CDI AO ANO COTADA NA TESOURARIA
                 15 DCOMW511-S2-PSPREAD-CDI   PIC  9(006)V9(007).
      *-->     PERCENTUAL DO CDI COTADA NA TESOURARIA
                 15 DCOMW511-S2-PCDI-ENCAR    PIC  9(003)V9(001).
              10 DCOMW511-CTES-FILLER         PIC  X(3778).
      *
      *--> AREA DE ACESSO AO MODULO NTAX3300
      *--> =================================
           05 DCOMW511-DADOS-NTAX3300         REDEFINES DCOMW511-DADOS.
              10 DCOMW511-NTAX3300-ENTRADA.
                 15 DCOMW511-E3-CIDTFD-INDCD  PIC  9(005).
                 15 DCOMW511-E3-DATA-APL      PIC  X(010).
                 15 DCOMW511-E3-DATA-RES      PIC  X(010).
      *
              10 DCOMW511-NTAX3300-SAIDA.
                 15 DCOMW511-S3-TAXA-ANUAL    PIC  9(008)V9(008).
              10 DCOMW511-NTAX3300-FILLER     PIC  X(3859).
      *
      *--> AREA DE ACESSO AO MODULO DCOM5110
      *--> =================================
           05 DCOMW511-DADOS-DCOM5110         REDEFINES DCOMW511-DADOS.
              10 DCOMW511-5110-ENTRADA.
                 15 DCOMW511-E4-COD-RETORNO   PIC  X(004).
                 15 DCOMW511-E4-MSG-RETORNO   PIC  X(079).
                 15 DCOMW511-E4-RESTART       PIC  9(005).
                 15 DCOMW511-E4-FLAG          PIC  X(001).
                 15 DCOMW511-E4-CFUNC-BDSCO   PIC  9(009).
                 15 DCOMW511-E4-CTERM         PIC  X(008).
                 15 DCOMW511-E4-DANO-OPER     PIC  9(004).
                 15 DCOMW511-E4-NSEQ-OPER     PIC  9(009).
                 15 DCOMW511-E4-TPO-LIBRC     PIC  X(001).
                 15 DCOMW511-E4-VLIBRC        PIC S9(015)V9(002).
              10 DCOMW511-5110-SAIDA.
                 15 DCOMW511-S4-COD-RETORNO   PIC  9(004).
                 15 DCOMW511-S4-MSG-RETORNO   PIC  X(079).
                 15 DCOMW511-S4-RESTART       PIC  9(005).
                 15 DCOMW511-S4-FLAG          PIC  X(001).
                 15 DCOMW511-S4-CONSISTENCIA  PIC  9(005).
                 15 DCOMW511-S4-ERRO-AREA     PIC  X(106).
                 15 DCOMW511-S4-SQLCA         PIC  X(136).
              10 DCOMW511-DCOM5110-FILLER     PIC  X(3427).
      *
