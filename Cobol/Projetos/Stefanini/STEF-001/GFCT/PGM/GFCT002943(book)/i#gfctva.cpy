      *****************************************************************
      * NOME DA INC - I#GFCTVA                                        *
      * DESCRICAO   - COBRANCA - ROTINA DE FLEXIBILIZACAO             *
      * REGISTRO TABELA DE EVENTOS - GFCTB092 -          ROTINA BATCH *
ST2507* TAMANHO     - 255                                             *
      * DATA        - 13.09.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTVA-GFCTB092.
           03 GFCTVA-DENVIO-MOVTO-TARIF      PIC  X(010).
           03 GFCTVA-CROTNA-ORIGE-MOVTO      PIC  X(004).
           03 GFCTVA-CNRO-ARQ-MOVTO          PIC S9(002)      COMP-3.
           03 GFCTVA-CSEQ-MOVTO              PIC S9(011)      COMP-3.
           03 GFCTVA-CSERVC-TARIF            PIC S9(005)      COMP-3.
           03 GFCTVA-CMIDIA-SERVC-MOVTO      PIC S9(005)      COMP-3.
           03 GFCTVA-CPERIF-SERVC-MOVTO      PIC S9(005)      COMP-3.
           03 GFCTVA-QEVNTO-AGRUP            PIC S9(006)      COMP-3.
           03 GFCTVA-HPREST-SERVC-MOVTO      PIC  X(008).
           03 GFCTVA-CTPO-CTA-MOVTO          PIC S9(001)      COMP-3.
           03 GFCTVA-CBCO-DSTNO-MOVTO        PIC S9(003)      COMP-3.
           03 GFCTVA-CAG-DSTNO-MOVTO         PIC S9(005)      COMP-3.
           03 GFCTVA-CCTA-DSTNO-MOVTO        PIC S9(013)      COMP-3.
           03 GFCTVA-CPAB-DSTNO-MOVTO        PIC S9(005)      COMP-3.
           03 GFCTVA-CAG-CTLZA-DEB           PIC S9(005)      COMP-3.
           03 GFCTVA-CCTA-CTLZA-DEB          PIC S9(013)      COMP-3.
           03 GFCTVA-VTARIF-BRUTO-MOVTO      PIC S9(009)V9(2) COMP-3.
           03 GFCTVA-VTARIF-LIQ-MOVTO        PIC S9(009)V9(2) COMP-3.
           03 GFCTVA-VTARIF-DEB-MOVTO        PIC S9(009)V9(2) COMP-3.
           03 GFCTVA-DAGNDA-DEB-MOVTO        PIC  X(010).
           03 GFCTVA-DEFETV-DEB-MOVTO        PIC  X(010).
           03 GFCTVA-QMAX-TENTV-DEB          PIC S9(003)      COMP-3.
           03 GFCTVA-QTENTV-DEB-MOVTO        PIC S9(003)      COMP-3.
           03 GFCTVA-CSIT-EVNTO-RECBD        PIC S9(002)      COMP-3.
           03 GFCTVA-CIDTFD-PGMC-COBR        PIC S9(002)      COMP-3.
           03 GFCTVA-DOCOR-EVNTO             PIC  X(010).
           03 GFCTVA-VTARIF-CADTR-SIST       PIC S9(009)V9(2) COMP-3.
           03 GFCTVA-CINDCD-TARIF-PCELD      PIC  X(001).
           03 GFCTVA-CTRATO-MOVTO            PIC S9(002)      COMP-3.
           03 GFCTVA-QTD-FQ-COMPON           PIC S9(005)      COMP-3.
           03 GFCTVA-QTD-EXC-DIA             PIC S9(005)      COMP-3.
           03 GFCTVA-QTD-EXC-ACU             PIC S9(005)      COMP-3.
           03 GFCTVA-FLAG-DATA               PIC  X(001).
           03 GFCTVA-AGENCIA-CREC            PIC S9(005)      COMP-3.
           03 GFCTVA-CONTA-CREC              PIC S9(013)      COMP-3.
           03 GFCTVA-PACOTE-EVENTO           PIC S9(005)      COMP-3.
           03 GFCTVA-AGRUPAMENTOS.
              05 GFCTVA-CSGMTO-GSTAO-TARIF   PIC S9(005)      COMP-3.
              05 GFCTVA-CMUN-RURAL           PIC S9(009)      COMP-3.
              05 GFCTVA-CSGL-UF              PIC  X(002).
              05 GFCTVA-BCO-POSTAL           PIC S9(005)      COMP-3.
              05 GFCTVA-CPAB                 PIC S9(005)      COMP-3.
              05 GFCTVA-CPF-CNPJ             PIC S9(009)      COMP-3.
              05 GFCTVA-FILIAL               PIC S9(005)      COMP-3.
              05 GFCTVA-CONTROLE             PIC  X(002).
           03 GFCTVA-PACOTE.
              05 GFCTVA-CSEC-COMP-PCT        PIC  9(003).
              05 GFCTVA-DINI-PER-COMP        PIC  9(008).
              05 GFCTVA-DFIM-PER-COMP        PIC  9(008).
              05 GFCTVA-PERIODO-PCT          PIC  9(006).
              05 GFCTVA-CPER-COBR-COMPO  PIC S9(001)V USAGE COMP-3.
              05 GFCTVA-VEXCED-CP-PCT    PIC S9(009)V9(2) USAGE COMP-3.
              05 GFCTVA-PEXCED-CP-PCT    PIC S9(003)V9(4) USAGE COMP-3.
              05 GFCTVA-QDIA-COBR-COMPO  PIC S9(002)V USAGE COMP-3.
           03 GFCTVA-HIERARQUIA-REG          OCCURS 7 TIMES.
              05 GFCTVA-AGPT-REG             PIC  9(002).
           03 GFCTVA-MAIS-PACOTE.
              05 GFCTVA-DDIA-COBR-TARIF      PIC S9(002)V USAGE COMP-3.
              05 GFCTVA-CIDTFD-COBR-FERI     PIC  X(001).
              05 GFCTVA-CIDTFD-VLR-EXCED     PIC S9(001)V USAGE COMP-3.
              05 GFCTVA-REG-EXCEDENTE        PIC  X(001).
           03 GFCTVA-AGRUP-FUNC-PERSO        PIC  X(001).
           03 GFCTVA-HIERARQUIA-REG-CMPL     OCCURS 2 TIMES.
              05 GFCTVA-AGPT-CMPL            PIC  9(2).
           03 GFCTVA-AGRUPAMENTO-RAZAO.
              05 GFCTVA-GRUPO                PIC  9(03)        COMP-3.
              05 GFCTVA-SUB-GRUPO            PIC  9(03)        COMP-3.
           03 GFCTVA-PROGR-BENEF             PIC  X(001).
      *       'S' - PARTICIPANTE P.B.
      *       ' ' - NAO PARTICIPANTE P.B.
      *
ST2507     03 GFCTVA-CPF-CNPJ-S              PIC  X(009).
ST2507     03 GFCTVA-FILIAL-S                PIC  X(004).
ST2507     03 GFCTVA-CONTROLE-S              PIC  X(002).
