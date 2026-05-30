      *****************************************************************
      * NOME DA INC - I#GFCTST                                        *
      * DESCRICAO   - ESPELHO DA TABELA GFCTB092 (MOVTO_EVNTO_CRRTT)  *
      * TAMANHO     - 132                                             *
      * DATA        - 05.2006                                         *
      * RESPONSAVEL - VANI NUNES   - PROCWORK                         *
      *****************************************************************
      *
       01  GFCTST-REG.
           10 GFCTST-DENVIO-MOVTO-TARIF   PIC X(10).
           10 GFCTST-CROTNA-ORIGE-MOVTO   PIC X(4).
           10 GFCTST-CNRO-ARQ-MOVTO       PIC S9(2)       COMP-3.
           10 GFCTST-CSEQ-MOVTO           PIC S9(11)      COMP-3.
           10 GFCTST-CSERVC-TARIF         PIC S9(5)       COMP-3.
           10 GFCTST-CMIDIA-SERVC-MOVTO   PIC S9(5)       COMP-3.
           10 GFCTST-CPERIF-SERVC-MOVTO   PIC S9(5)       COMP-3.
           10 GFCTST-QEVNTO-AGRUP         PIC S9(6)       COMP-3.
           10 GFCTST-HPREST-SERVC-MOVTO   PIC X(8).
           10 GFCTST-CTPO-CTA-MOVTO       PIC S9(1)       COMP-3.
           10 GFCTST-CBCO-DSTNO-MOVTO     PIC S9(3)       COMP-3.
           10 GFCTST-CAG-DSTNO-MOVTO      PIC S9(5)       COMP-3.
           10 GFCTST-CCTA-DSTNO-MOVTO     PIC S9(13)      COMP-3.
           10 GFCTST-CPAB-DSTNO-MOVTO     PIC S9(5)       COMP-3.
           10 GFCTST-CAG-CTLZA-DEB        PIC S9(5)       COMP-3.
           10 GFCTST-CCTA-CTLZA-DEB       PIC S9(13)      COMP-3.
           10 GFCTST-VTARIF-BRUTO-MOVTO   PIC S9(9)V99    COMP-3.
           10 GFCTST-VTARIF-LIQ-MOVTO     PIC S9(9)V99    COMP-3.
           10 GFCTST-VTARIF-DEB-MOVTO     PIC S9(9)V99    COMP-3.
           10 GFCTST-DAGNDA-DEB-MOVTO     PIC X(10).
           10 GFCTST-DEFETV-DEB-MOVTO     PIC X(10).
           10 GFCTST-QMAX-TENTV-DEB       PIC S9(3)       COMP-3.
           10 GFCTST-QTENTV-DEB-MOVTO     PIC S9(3)       COMP-3.
           10 GFCTST-CSIT-EVNTO-RECBD     PIC S9(2)       COMP-3.
           10 GFCTST-CIDTFD-PGMC-COBR     PIC S9(2)       COMP-3.
           10 GFCTST-DOCOR-EVNTO          PIC X(10).
           10 GFCTST-VTARIF-CADTR-SIST    PIC S9(9)V99    COMP-3.
           10 GFCTST-CINDCD-TARIF-PARCEL  PIC X.
