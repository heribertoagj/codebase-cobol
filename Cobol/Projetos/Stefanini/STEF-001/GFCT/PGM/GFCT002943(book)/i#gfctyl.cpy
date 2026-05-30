      *****************************************************************
      * NOME DA INC - I#GFCTYL                                        *
      * DESCRICAO   - COBRANCA LIBERACAO SUSPENSAO TARIFAS            *
      * REG TAB DE EVENTOS - GFCTB092 COM AGRUPAMENTOS - ROTINA BATCH *
ST2507* TAMANHO     - 098                                             *
      * DATA        - 24.01.2007                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTYL-GFCTB092.
           05 GFCTYL-DENVIO-MOVTO-TARIF      PIC  X(10).
           05 GFCTYL-CROTNA-ORIGE-MOVTO      PIC  X(04).
           05 GFCTYL-CNRO-ARQ-MOVTO          PIC S9(02)        COMP-3.
           05 GFCTYL-CSEQ-MOVTO              PIC S9(11)        COMP-3.
           05 GFCTYL-CSERVC-TARIF            PIC S9(05)        COMP-3.
           05 GFCTYL-CTPO-CTA-MOVTO          PIC S9(01)        COMP-3.
           05 GFCTYL-CBCO-DSTNO-MOVTO        PIC S9(03)        COMP-3.
           05 GFCTYL-CAG-DSTNO-MOVTO         PIC S9(05)        COMP-3.
           05 GFCTYL-CCTA-DSTNO-MOVTO        PIC S9(13)        COMP-3.
           05 GFCTYL-CPAB-DSTNO-MOVTO        PIC S9(05)        COMP-3.
           05 GFCTYL-DAGNDA-DEB-MOVTO        PIC  X(10).
           05 GFCTYL-DEFETV-DEB-MOVTO        PIC  X(10).
           05 GFCTYL-CSIT-EVNTO-RECBD        PIC S9(02)        COMP-3.
           05 GFCTYL-DOCOR-EVNTO             PIC  X(10).
           05 GFCTYL-AGRUPAMENTOS.
               10 GFCTYL-CMUN-RURAL          PIC S9(009)      COMP-3.
               10 GFCTYL-CSGL-UF             PIC  X(002).
               10 GFCTYL-CPOSTAL             PIC S9(005)      COMP-3.
ST2507*        10 GFCTYL-CPF-CNPJ            PIC S9(009)      COMP-3.
ST2507*        10 GFCTYL-FILIAL              PIC S9(005)      COMP-3.
ST2507         10 GFCTYL-CPF-CNPJ            PIC  X(009).
ST2507         10 GFCTYL-FILIAL              PIC  X(004).
               10 GFCTYL-CONTROLE            PIC  X(002).
