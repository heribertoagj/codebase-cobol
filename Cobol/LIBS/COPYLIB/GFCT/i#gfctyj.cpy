      *****************************************************************
      * NOME DA INC - I#GFCTYJ                                        *
      * DESCRICAO   - COBRANCA LIBERACAO SUSPENSAO TARIFAS            *
      * REGISTRO TABELA DE EVENTOS - GFCTB092 -          ROTINA BATCH *
      * TAMANHO     - 073                                             *
      * DATA        - 24.01.2007                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTYJ-GFCTB092.
           05 GFCTYJ-DENVIO-MOVTO-TARIF      PIC  X(10).
           05 GFCTYJ-CROTNA-ORIGE-MOVTO      PIC  X(04).
           05 GFCTYJ-CNRO-ARQ-MOVTO          PIC S9(02)        COMP-3.
           05 GFCTYJ-CSEQ-MOVTO              PIC S9(11)        COMP-3.
           05 GFCTYJ-CSERVC-TARIF            PIC S9(05)        COMP-3.
           05 GFCTYJ-CTPO-CTA-MOVTO          PIC S9(01)        COMP-3.
           05 GFCTYJ-CBCO-DSTNO-MOVTO        PIC S9(03)        COMP-3.
           05 GFCTYJ-CAG-DSTNO-MOVTO         PIC S9(05)        COMP-3.
           05 GFCTYJ-CCTA-DSTNO-MOVTO        PIC S9(13)        COMP-3.
           05 GFCTYJ-CPAB-DSTNO-MOVTO        PIC S9(05)        COMP-3.
           05 GFCTYJ-DAGNDA-DEB-MOVTO        PIC  X(10).
           05 GFCTYJ-DEFETV-DEB-MOVTO        PIC  X(10).
           05 GFCTYJ-CSIT-EVNTO-RECBD        PIC S9(02)        COMP-3.
           05 GFCTYJ-DOCOR-EVNTO             PIC  X(10).
