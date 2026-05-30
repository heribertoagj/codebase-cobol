      *****************************************************************
      * NOME DA INC - I#GFCTV4                                        *
      * DESCRICAO   - COBRANCA - ROTINA DE FLEXIBILIZACAO             *
      * REGISTRO TABELA DE DESCONTOS - GFCTB041 - ROTINA BATCH        *
      * TAMANHO     - 150                                             *
      * DATA        - 05.09.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTV4-GFCTB041.
           03  GFCTV4-CH-MOVTO.
               05  GFCTV4-DENVIO-MOVTO-TARIF     PIC  X(010).
               05  GFCTV4-CROTNA-ORIGE-MOVTO     PIC  X(004).
               05  GFCTV4-CNRO-ARQ-MOVTO         PIC S9(002)    COMP-3.
               05  GFCTV4-CSEQ-MOVTO             PIC S9(011)    COMP-3.
           03  GFCTV4-CIDTFD-TPO-DESC            PIC S9(001)    COMP-3.
           03  GFCTV4-CIDTFD-MOTVO-DESC          PIC S9(005)    COMP-3.
           03  GFCTV4-CIDTFD-DESC                PIC S9(005)    COMP-3.
RU0125     03  GFCTV4-CPTCAO                     PIC S9(002)    COMP-3.
           03  GFCTV4-CPARM-DESC                 PIC S9(001)    COMP-3.
           03  GFCTV4-QEVNTO-DESC                PIC S9(007)    COMP-3.
           03  GFCTV4-VCONCS-DESC                PIC S9(009)V99 COMP-3.
           03  GFCTV4-CCONTR-FLEXZ               PIC  X(015).
           03  GFCTV4-CBCO-DSTNO-MOVTO           PIC S9(003)    COMP-3.
           03  GFCTV4-CAG-DSTNO-MOVTO            PIC S9(005)    COMP-3.
           03  GFCTV4-CCTA-DSTNO-MOVTO           PIC S9(013)    COMP-3.
           03  GFCTV4-CPAB-DSTNO-MOVTO           PIC S9(005)    COMP-3.
           03  GFCTV4-CSERVC-TARIF               PIC S9(005)    COMP-3.
           03  GFCTV4-DOCOR-EVNTO                PIC  X(10).
           03  GFCTV4-HPREST-MOVTO               PIC  X(008).
           03  GFCTV4-IDTFD-TIMESTAMP            PIC  X(026).
           03  FILLER                            PIC  X(031).
