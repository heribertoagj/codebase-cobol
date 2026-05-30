      ******************************************************************
      * NOME BOOK : I#DCOMZI                                           *
      * DESCRICAO : LAYOUT DA TABELA TPROT_SOLTC_AGNDA (DCOMB0P5)      *
      *             LAYOUT DA TABELA TPROT_SOLTC_CANAL (DCOMB0P6)      *
      * DATA      : 25/08/2020                                         *
      * AUTOR     : AMANDA BELTOSO DOS SANTOS                          *
      * EMPRESA   : CAPGEMINI BRASIL                                   *
      * TAMANHO   : 081 BYTES                                          *
      ******************************************************************
       05  DCOMZI-REGISTRO.
      *    DADOS DO PROTOCOLO
           10 DCOMZI-CBCO                 PIC S9(003)V     USAGE COMP-3.
           10 DCOMZI-CAG-BCRIA            PIC S9(005)V     USAGE COMP-3.
           10 DCOMZI-CCTA-BCRIA-CLI       PIC S9(013)V     USAGE COMP-3.
           10 DCOMZI-CPROT-SOLTC-AGNDA    PIC X(050).
           10 DCOMZI-CTPO-PROT-AGNDA      PIC X(001).
           10 DCOMZI-NSMULA-OPER-DESC     PIC S9(009)V     USAGE COMP-3.
           10 DCOMZI-NSMULA-OPER-DESC-N   PIC X(001).
           10 DCOMZI-DANO-OPER-DESC       PIC S9(004)V     USAGE COMP-3.
           10 DCOMZI-DANO-OPER-DESC-N     PIC X(001).
           10 DCOMZI-NSEQ-OPER-DESC       PIC S9(009)V     USAGE COMP-3.
           10 DCOMZI-NSEQ-OPER-DESC-N     PIC X(001).
      *    DADOS DO CANAL
           10 DCOMZI-CTPO-CANAL           PIC S9(003)V     USAGE COMP-3.
