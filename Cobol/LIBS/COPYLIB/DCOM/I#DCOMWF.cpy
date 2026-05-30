      ******************************************************************
      * SISTEMA : DCOM - I#DCOMWF                                      *
      * TAMANHO : 135 BYTES                                            *
      * ARQUIVO : RESP                                                 *
      *                                                                *
      ******************************************************************
       01  WF-MA-REGISTRO.
           05  WF-MA-TIPO-REG             PIC  X(01).
           05  WF-MA-CPRODT               PIC  9(03)       COMP-3.
           05  WF-MA-CSPROD-DESC-COML     PIC  9(03)       COMP-3.
           05  WF-MA-CBCO                 PIC  9(03)       COMP-3.
           05  WF-MA-CAG-BCRIA            PIC  9(05)       COMP-3.
           05  WF-MA-CCTA-BCRIA-CLI       PIC  9(13)       COMP-3.
           05  WF-MA-CCNPJ-CPF            PIC  9(09)        COMP-3.
           05  WF-MA-CFLIAL-CNPJ          PIC  9(05)        COMP-3.
           05  WF-MA-CCTRL-CNPJ-CPF       PIC  9(02)        COMP-3.
           05  WF-MA-DANO-OPER-DESC       PIC  9(04)       COMP-3.
           05  WF-MA-NSEQ-OPER-DESC       PIC  9(09)       COMP-3.
           05  WF-MA-CCONVE-CLI-DESC      PIC  9(09)       COMP-3.
           05  WF-MA-CSIT-DESC-COML       PIC  9(03)       COMP-3.
           05  WF-MA-DINIC-OPER-DESC      PIC  X(10).
           05  WF-MA-DVCTO-FNAL-OPER      PIC  X(10).
           05  WF-MA-PTX-JURO-MES         PIC  9(03)V9(05) COMP-3.
           05  WF-MA-VLIM-CONVE-CLI       PIC  9(15)V9(02) COMP-3.
           05  WF-MA-VUTLZD-CONVE-CLI     PIC  9(15)V9(02) COMP-3.
           05  WF-MA-CCART-LIM-CREDT      PIC  X(05).
           05  WF-MA-QTITLO-CHEQ-PCELA    PIC  9(09)        COMP-3.
           05  WF-MA-NOME                 PIC  X(40).
