      *****************************************************************
      * NOME DA INC - GFCTWKX5                                        *
      * DESCRICAO   - MOVIMENTO CESTA EXCLUSIVE PARA ENCARTEIRAMENTO  *
      *             - OCORRENCIAS NA ISENCAO DOS EVENTOS              *
ST2507* TAMANHO     - 389                                             *
      * DATA        - NOV/2016                                        *
      * RESPONSAVEL - PAGNOCCA - SONDA IT                             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01 GFCTWKX5-REG.
           02 KX5-PARTE-VA.
              03 KX5-VA-DENVIO-MOVTO-TARIF      PIC  X(010).
              03 KX5-VA-CROTNA-ORIGE-MOVTO      PIC  X(004).
              03 KX5-VA-CNRO-ARQ-MOVTO          PIC S9(002)      COMP-3.
              03 KX5-VA-CSEQ-MOVTO              PIC S9(011)      COMP-3.
              03 KX5-VA-CSERVC-TARIF            PIC S9(005)      COMP-3.
              03 KX5-VA-CMIDIA-SERVC-MOVTO      PIC S9(005)      COMP-3.
              03 KX5-VA-CPERIF-SERVC-MOVTO      PIC S9(005)      COMP-3.
              03 KX5-VA-QEVNTO-AGRUP            PIC S9(006)      COMP-3.
              03 KX5-VA-HPREST-SERVC-MOVTO      PIC  X(008).
              03 KX5-VA-CTPO-CTA-MOVTO          PIC S9(001)      COMP-3.
              03 KX5-VA-CBCO-DSTNO-MOVTO        PIC S9(003)      COMP-3.
              03 KX5-VA-CAG-DSTNO-MOVTO         PIC S9(005)      COMP-3.
              03 KX5-VA-CCTA-DSTNO-MOVTO        PIC S9(013)      COMP-3.
              03 KX5-VA-CPAB-DSTNO-MOVTO        PIC S9(005)      COMP-3.
              03 KX5-VA-CAG-CTLZA-DEB           PIC S9(005)      COMP-3.
              03 KX5-VA-CCTA-CTLZA-DEB          PIC S9(013)      COMP-3.
              03 KX5-VA-VTARIF-BRUTO-MOVTO      PIC S9(009)V9(2) COMP-3.
              03 KX5-VA-VTARIF-LIQ-MOVTO        PIC S9(009)V9(2) COMP-3.
              03 KX5-VA-VTARIF-DEB-MOVTO        PIC S9(009)V9(2) COMP-3.
              03 KX5-VA-DAGNDA-DEB-MOVTO        PIC  X(010).
              03 KX5-VA-DEFETV-DEB-MOVTO        PIC  X(010).
              03 KX5-VA-QMAX-TENTV-DEB          PIC S9(003)      COMP-3.
              03 KX5-VA-QTENTV-DEB-MOVTO        PIC S9(003)      COMP-3.
              03 KX5-VA-CSIT-EVNTO-RECBD        PIC S9(002)      COMP-3.
              03 KX5-VA-CIDTFD-PGMC-COBR        PIC S9(002)      COMP-3.
              03 KX5-VA-DOCOR-EVNTO             PIC  X(010).
              03 KX5-VA-VTARIF-CADTR-SIST       PIC S9(009)V9(2) COMP-3.
              03 KX5-VA-CINDCD-TARIF-PCELD      PIC  X(001).
              03 KX5-VA-CTRATO-MOVTO            PIC S9(002)      COMP-3.
              03 KX5-VA-QTD-FQ-COMPON           PIC S9(005)      COMP-3.
              03 KX5-VA-QTD-EXC-DIA             PIC S9(005)      COMP-3.
              03 KX5-VA-QTD-EXC-ACU             PIC S9(005)      COMP-3.
              03 KX5-VA-FLAG-DATA               PIC  X(001).
              03 KX5-VA-AGENCIA-CREC            PIC S9(005)      COMP-3.
              03 KX5-VA-CONTA-CREC              PIC S9(013)      COMP-3.
              03 KX5-VA-PACOTE-EVENTO           PIC S9(005)      COMP-3.
              03 KX5-VA-AGRUPAMENTOS.
                 05 KX5-VA-CSGMTO-GSTAO-TARIF   PIC S9(005)      COMP-3.
                 05 KX5-VA-CMUN-RURAL           PIC S9(009)      COMP-3.
                 05 KX5-VA-CSGL-UF              PIC  X(002).
                 05 KX5-VA-BCO-POSTAL           PIC S9(005)      COMP-3.
                 05 KX5-VA-CPAB                 PIC S9(005)      COMP-3.
ST2507*          05 KX5-VA-CPF-CNPJ             PIC S9(009)      COMP-3.
ST2507*          05 KX5-VA-FILIAL               PIC S9(005)      COMP-3.
ST2507           05 KX5-VA-CPF-CNPJ             PIC  X(009).
ST2507           05 KX5-VA-FILIAL               PIC  X(004).
                 05 KX5-VA-CONTROLE             PIC  X(002).
              03 KX5-VA-PACOTE.
                 05 KX5-VA-CSEC-COMP-PCT        PIC  9(003).
                 05 KX5-VA-DINI-PER-COMP        PIC  9(008).
                 05 KX5-VA-DFIM-PER-COMP        PIC  9(008).
                 05 KX5-VA-PERIODO-PCT          PIC  9(006).
                 05 KX5-VA-CPER-COBR-COMPO  PIC S9(001)V COMP-3.
                 05 KX5-VA-VEXCED-CP-PCT    PIC S9(009)V9(2) COMP-3.
                 05 KX5-VA-PEXCED-CP-PCT    PIC S9(003)V9(4) COMP-3.
                 05 KX5-VA-QDIA-COBR-COMPO  PIC S9(002)V COMP-3.
              03 KX5-VA-HIERARQUIA-REG          OCCURS 7 TIMES.
                 05 KX5-VA-AGPT-REG             PIC  9(002).
              03 KX5-VA-MAIS-PACOTE.
                 05 KX5-VA-DDIA-COBR-TARIF      PIC S9(002)V  COMP-3.
                 05 KX5-VA-CIDTFD-COBR-FERI     PIC  X(001).
                 05 KX5-VA-CIDTFD-VLR-EXCED     PIC S9(001)V COMP-3.
                 05 KX5-VA-REG-EXCEDENTE        PIC  X(001).
              03 KX5-VA-AGRUP-FUNC-PERSO        PIC  X(001).
              03 KX5-VA-HIERARQUIA-REG-CMPL     OCCURS 2 TIMES.
                 05 KX5-VA-AGPT-CMPL            PIC  9(2).
              03 KX5-VA-AGRUPAMENTO-RAZAO.
                 05 KX5-VA-GRUPO                PIC  9(03)       COMP-3.
                 05 KX5-VA-SUB-GRUPO            PIC  9(03)       COMP-3.
              03 FILLER                         PIC  X(001).
           02 KX5-PARTE-KX1.
ST2507*       10 KX5-X1-PRINCIPAL-CPF        PIC S9(09)V         COMP-3.
ST2507*       10 KX5-X1-FILIAL-CPF           PIC S9(05)V         COMP-3.
ST2507        10 KX5-X1-PRINCIPAL-CPF        PIC  X(09).
ST2507        10 KX5-X1-FILIAL-CPF           PIC  X(04).
              10 KX5-X1-CTR-CPF              PIC S9(02)V         COMP-3.
              10 KX5-X1-CJUNC-DEPDC          PIC S9(05)V         COMP-3.
              10 KX5-X1-CCTA-CLI             PIC S9(07)V         COMP-3.
              10 KX5-X1-CSERVC-TARIF         PIC S9(05)V         COMP-3.
              10 KX5-X1-DINIC-ADSAO-INDVD    PIC S9(09)V         COMP-3.
              10 KX5-X1-DFIM-ADSAO-INDVD     PIC S9(09)V         COMP-3.
              10 KX5-X1-DATA-GFCT            PIC S9(09)V         COMP-3.
              10 KX5-X1-PER-ISENC            PIC S9(03)V         COMP-3.
              10 KX5-X1-DATA-ENCART          PIC S9(09)V         COMP-3.
              10 KX5-X1-DATA-ABERT-CTA       PIC S9(09)V         COMP-3.
              10 KX5-X1-CD-SEGMENTO          PIC S9(03)V         COMP-3.
              10 KX5-X1-AAAAMM-COMPET        PIC S9(06)V         COMP-3.
              10 KX5-X1-TP-DESCONTO          PIC S9(01)V         COMP-3.
              10 KX5-X1-VR-A-COBRAR          PIC S9(09)V9(02)    COMP-3.
              10 KX5-X1-CSERVC-TARIF-ANT     PIC S9(05)V         COMP-3.
              10 KX5-X1-AAAAMM-ISENC-DESC    PIC S9(06)V         COMP-3.
              10 FILLER                      PIC X(40).
           02 KX5-CD-OCORR                   PIC 9(02).
           02 KX5-RESERVA                    PIC X(20).
      *****************************************************************
      *
