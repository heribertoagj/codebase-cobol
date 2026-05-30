      ******************************************************************
      * NOME BOOK : I#DCOMZB                                           *
      * DESCRICAO : LAYOUT DA TABELA TPROT_SOLTC_AGNDA (DCOMB0P5)      *
      * DATA      : 09/2020                                            *
      * AUTOR     : MATHEUS SANTOS                                     *
      * EMPRESA   : CAPGEMINI NEARSHORE                                *
      * TAMANHO   : 0108 BYTES                                         *
      ******************************************************************
       05  DCOMZB-REGISTRO.
      *    TABELA TPROT_SOLTC_AGNDA
           10 DCOMZB-CBCO                 PIC S9(3)V USAGE COMP-3.
           10 DCOMZB-CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
           10 DCOMZB-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
           10 DCOMZB-CPROT-SOLTC-AGNDA    PIC X(50).
           10 DCOMZB-CTPO-PROT-AGNDA      PIC X(1).
           10 DCOMZB-QTEMPO-VALDD-PROCM   PIC S9(3)V USAGE COMP-3.
           10 DCOMZB-CSIT-PROT-AGNDA      PIC S9(1)V USAGE COMP-3.
           10 DCOMZB-HINCL-REG            PIC X(26).
           10 DCOMZB-NSMULA-OPER-DESC     PIC 9(09)V USAGE COMP-3.
           10 DCOMZB-NSMULA-OPER-DESC-N   PIC X(01)  VALUE SPACES.
           10 DCOMZB-DANO-OPER-DESC       PIC 9(04)V USAGE COMP-3.
           10 DCOMZB-DANO-OPER-DESC-N     PIC X(01)  VALUE SPACES.
           10 DCOMZB-NSEQ-OPER-DESC       PIC 9(09)V USAGE COMP-3.
           10 DCOMZB-NSEQ-OPER-DESC-N     PIC X(01)  VALUE SPACES.
