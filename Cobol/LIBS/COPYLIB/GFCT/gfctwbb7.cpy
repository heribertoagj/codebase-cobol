      *****************************************************************
      * NOME DA INC - GFCTWBB7                                        *
      * DESCRICAO   - COBRANCA - ROTINA DE FRANQUIA INVERSA           *
      * TAMANHO     - 298                                             *
      * DATA        - AGO/2012                                        *
      * RESPONSAVEL - PAGNOCCA - SONDA IT                             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTBB7-REG.
           02 GFCTBB7-REG-VA.
               03 BB7-DENVIO-MOVTO-TARIF      PIC  X(010).
               03 BB7-CROTNA-ORIGE-MOVTO      PIC  X(004).
               03 BB7-CNRO-ARQ-MOVTO          PIC S9(002)      COMP-3.
               03 BB7-CSEQ-MOVTO              PIC S9(011)      COMP-3.
               03 BB7-CSERVC-TARIF            PIC S9(005)      COMP-3.
               03 BB7-CMIDIA-SERVC-MOVTO      PIC S9(005)      COMP-3.
               03 BB7-CPERIF-SERVC-MOVTO      PIC S9(005)      COMP-3.
               03 BB7-QEVNTO-AGRUP            PIC S9(006)      COMP-3.
               03 BB7-HPREST-SERVC-MOVTO      PIC  X(008).
               03 BB7-CTPO-CTA-MOVTO          PIC S9(001)      COMP-3.
               03 BB7-CBCO-DSTNO-MOVTO        PIC S9(003)      COMP-3.
               03 BB7-CAG-DSTNO-MOVTO         PIC S9(005)      COMP-3.
               03 BB7-CCTA-DSTNO-MOVTO        PIC S9(013)      COMP-3.
               03 BB7-CPAB-DSTNO-MOVTO        PIC S9(005)      COMP-3.
               03 BB7-CAG-CTLZA-DEB           PIC S9(005)      COMP-3.
               03 BB7-CCTA-CTLZA-DEB          PIC S9(013)      COMP-3.
               03 BB7-VTARIF-BRUTO-MOVTO      PIC S9(009)V9(2) COMP-3.
               03 BB7-VTARIF-LIQ-MOVTO        PIC S9(009)V9(2) COMP-3.
               03 BB7-VTARIF-DEB-MOVTO        PIC S9(009)V9(2) COMP-3.
               03 BB7-DAGNDA-DEB-MOVTO        PIC  X(010).
               03 BB7-DEFETV-DEB-MOVTO        PIC  X(010).
               03 BB7-QMAX-TENTV-DEB          PIC S9(003)      COMP-3.
               03 BB7-QTENTV-DEB-MOVTO        PIC S9(003)      COMP-3.
               03 BB7-CSIT-EVNTO-RECBD        PIC S9(002)      COMP-3.
               03 BB7-CIDTFD-PGMC-COBR        PIC S9(002)      COMP-3.
               03 BB7-DOCOR-EVNTO             PIC  X(010).
               03 BB7-VTARIF-CADTR-SIST       PIC S9(009)V9(2) COMP-3.
               03 BB7-CINDCD-TARIF-PCELD      PIC  X(001).
               03 BB7-CTRATO-MOVTO            PIC S9(002)      COMP-3.
               03 BB7-QTD-FQ-COMPON           PIC S9(005)      COMP-3.
               03 BB7-QTD-EXC-DIA             PIC S9(005)      COMP-3.
               03 BB7-QTD-EXC-ACU             PIC S9(005)      COMP-3.
               03 BB7-FLAG-DATA               PIC  X(001).
               03 BB7-AGENCIA-CREC            PIC S9(005)      COMP-3.
               03 BB7-CONTA-CREC              PIC S9(013)      COMP-3.
               03 BB7-PACOTE-EVENTO           PIC S9(005)      COMP-3.
               03 BB7-AGRUPAMENTOS.
                05 BB7-CSGMTO-GSTAO-TARIF   PIC S9(005)      COMP-3.
                05 BB7-CMUN-RURAL           PIC S9(009)      COMP-3.
                05 BB7-CSGL-UF              PIC  X(002).
                05 BB7-BCO-POSTAL           PIC S9(005)      COMP-3.
                05 BB7-CPAB                 PIC S9(005)      COMP-3.
ST2507*         05 BB7-CPF-CNPJ             PIC S9(009)      COMP-3.
ST2507*         05 BB7-FILIAL               PIC S9(005)      COMP-3.
ST2507          05 BB7-CPF-CNPJ             PIC  X(009).
ST2507          05 BB7-FILIAL               PIC  X(004).
                05 BB7-CONTROLE             PIC  X(002).
               03 BB7-PACOTE.
                05 BB7-CSEC-COMP-PCT        PIC  9(003).
                05 BB7-DINI-PER-COMP        PIC  9(008).
                05 BB7-DFIM-PER-COMP        PIC  9(008).
                05 BB7-PERIODO-PCT          PIC  9(006).
                05 BB7-CPER-COBR-COMPO  PIC S9(001)V USAGE COMP-3.
                05 BB7-VEXCED-CP-PCT    PIC S9(009)V9(2) USAGE COMP-3.
                05 BB7-PEXCED-CP-PCT    PIC S9(003)V9(4) USAGE COMP-3.
                05 BB7-QDIA-COBR-COMPO  PIC S9(002)V USAGE COMP-3.
               03 BB7-HIERARQUIA-REG          OCCURS 7 TIMES.
                05 BB7-AGPT-REG             PIC  9(002).
               03 BB7-MAIS-PACOTE.
                05 BB7-DDIA-COBR-TARIF      PIC S9(002)V USAGE COMP-3.
                05 BB7-CIDTFD-COBR-FERI     PIC  X(001).
                05 BB7-CIDTFD-VLR-EXCED     PIC S9(001)V USAGE COMP-3.
                05 BB7-REG-EXCEDENTE        PIC  X(001).
               03 BB7-AGRUP-FUNC-PERSO        PIC  X(001).
               03 BB7-HIERARQUIA-REG-CMPL     OCCURS 2 TIMES.
                05 BB7-AGPT-CMPL            PIC  9(2).
               03 BB7-AGRUPAMENTO-RAZAO.
                05 BB7-GRUPO                PIC  9(03)        COMP-3.
                05 BB7-SUB-GRUPO            PIC  9(03)        COMP-3.
           02 BB7-QTD-FRANQ-INV           PIC  9(03)        COMP-3.
           02 BB7-AAAAMM-EVEN             PIC  9(07)        COMP-3.
           02 BB7-DIA-EVEN                PIC  9(03)        COMP-3.
ST2507*    02 FILLER                      PIC  X(41).
ST2507     02 FILLER                      PIC  X(36).
