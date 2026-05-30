      ******************************************************************
      * NOME BOOK : I#DCOMZM                                           *
      * DESCRICAO : LAYOUT DO RELATORIO D EPROTOCOLOS DO DIA           *
      * DATA      : 25/08/2020                                         *
      * AUTOR     : AMANDA BELTOSO DOS SANTOS                          *
      * EMPRESA   : CAPGEMINI BRASIL                                   *
      * TAMANHO   : 082 BYTES                                          *
      ******************************************************************
       05  DCOMZM-REGISTRO.
      *    DADOS DO PROTOCOLO
           10 DCOMZM-CBCO                 PIC S9(003)V     USAGE COMP-3.
           10 DCOMZM-CAG-BCRIA            PIC S9(005)V     USAGE COMP-3.
           10 DCOMZM-CCTA-BCRIA-CLI       PIC S9(013)V     USAGE COMP-3.
           10 DCOMZM-CPROT-SOLTC-AGNDA    PIC X(050).
           10 DCOMZM-CTPO-PROT-AGNDA      PIC X(001).
           10 DCOMZM-CSIT-PROT-AGNDA      PIC S9(001)V     USAGE COMP-3.
           10 DCOMZM-NSMULA-OPER-DESC     PIC S9(009)V     USAGE COMP-3.
           10 DCOMZM-NSMULA-OPER-DESC-N   PIC X(001).
           10 DCOMZM-DANO-OPER-DESC       PIC S9(004)V     USAGE COMP-3.
           10 DCOMZM-DANO-OPER-DESC-N     PIC X(001).
           10 DCOMZM-NSEQ-OPER-DESC       PIC S9(009)V     USAGE COMP-3.
           10 DCOMZM-NSEQ-OPER-DESC-N     PIC X(001).
      *    DADOS DO CANAL
           10 DCOMZM-CTPO-CANAL           PIC S9(003)V     USAGE COMP-3.
