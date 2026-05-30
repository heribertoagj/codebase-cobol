      ************************** PROCWORK ******************************
      *   I#GFCTVG - DADOS P/ COBRANCA - APURACAO DE FRANQUIAS         *
      *              - TABELAS: GFCTB092, 41                           *
      *                                                                *
      *   LERCL: 090                                       SET/2006    *
      ************************** PROCWORK ******************************
      *
       01  REG-GFCTVG.
           05 VG-GFCTB041.
              10 VG-DENVIO-MOVTO-TARIF   PIC X(10).
              10 VG-CROTNA-ORIGE-MOVTO   PIC X(4).
              10 VG-CNRO-ARQ-MOVTO       PIC S9(2)V USAGE COMP-3.
              10 VG-CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
              10 VG-CIDTFD-TPO-DESC      PIC S9(1)V USAGE COMP-3.
              10 VG-CIDTFD-MOTVO-DESC    PIC S9(5)V USAGE COMP-3.
              10 VG-CIDTFD-DESC          PIC S9(5)V USAGE COMP-3.
              10 VG-CPARM-DESC           PIC S9(1)V USAGE COMP-3.
              10 VG-QEVNTO-DESC          PIC S9(6)V USAGE COMP-3.
           05 VG-GFCTB092.
              10 VG-CBCO-DSTNO-MOVTO     PIC S9(3)V USAGE COMP-3.
              10 VG-CAG-DSTNO-MOVTO      PIC S9(5)V USAGE COMP-3.
              10 VG-CCTA-DSTNO-MOVTO     PIC S9(13)V USAGE COMP-3.
              10 VG-CPAB-DSTNO-MOVTO     PIC S9(5)V USAGE COMP-3.
              10 VG-CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
              10 VG-DOCOR-EVNTO          PIC X(10).
              10 VG-HPREST-SERVC-MOVTO   PIC X(8).
           05 FILLER                     PIC X(20).
