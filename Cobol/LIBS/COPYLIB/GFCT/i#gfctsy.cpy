      *****************************************************************
      * NOME DA INC - I#GFCTSY                                        *
      * DESCRICAO   - ESPELHO DA TABELA GFCTB093 (MOVTO_EVNTO_NAO)    *
      * TAMANHO     - 128                                             *
      * DATA        - 06.2006                                         *
      * RESPONSAVEL - VANI NUNES   - PROCWORK                         *
      *****************************************************************
      *
       01  GFCTSY-REG.
           10 GFCTSY-DENVIO-MOVTO-TARIF   PIC X(10).
           10 GFCTSY-CROTNA-ORIGE-MOVTO   PIC X(4).
           10 GFCTSY-CNRO-ARQ-MOVTO       PIC S9(2)    COMP-3.
           10 GFCTSY-CSEQ-MOVTO           PIC S9(11)   COMP-3.
           10 GFCTSY-CSERVC-TARIF         PIC S9(5)    COMP-3.
           10 GFCTSY-CMIDIA-SERVC-MOVTO   PIC S9(5)    COMP-3.
           10 GFCTSY-CPERIF-SERVC-MOVTO   PIC S9(5)    COMP-3.
           10 GFCTSY-QEVNTO-AGRUP         PIC S9(6)    COMP-3.
           10 GFCTSY-CIDTFD-NAO-CRRTT     PIC X(30).
           10 GFCTSY-VEVNTO-CORR-MOVTO    PIC S9(9)V99 COMP-3.
           10 GFCTSY-VBRUTO-TARIF-MOVTO   PIC S9(9)V99 COMP-3.
           10 GFCTSY-VTARIF-LIQ-MOVTO     PIC S9(9)V99 COMP-3.
           10 GFCTSY-VTARIF-DEB-MOVTO     PIC S9(9)V99 COMP-3.
           10 GFCTSY-DEFETV-DEB-MOVTO     PIC X(10).
           10 GFCTSY-CSIT-EVNTO-RECBD     PIC S9(2)    COMP-3.
           10 GFCTSY-DOCOR-EVNTO          PIC X(10).
           10 GFCTSY-VTARIF-CADTR-SIST    PIC S9(9)V99 COMP-3.
           10 GFCTSY-DAGNDA-DEB-MOVTO     PIC X(10).
           10 GFCTSY-CINDCD-TARIF-PARCEL  PIC X.
