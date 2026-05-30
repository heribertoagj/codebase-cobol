      *****************************************************************
      * ABR/2016               S O N D A                       CIBELE *
      *****************************************************************
      * NOME DA INC - GFCTWBZY - CLONE DA I#GFCTZY                    *
      * DESCRICAO   - BONIFICACAO DESCONTO PROGRESSIVO INDIVIDUAL     *
      *               . ZY-REG-GFCTVA = REGISTRO COMPLETO DE ENTRADA  *
      *               . ZY-REG-BONIFI = DADOS P/ TRATAR BONIFICACAO   *
      * TAMANHO     - 320                                             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01 GFCTZY-REG.
          03 ZY-REG-GFCTVA.
             05 ZY-GFCTB092.
                10 ZY-DENVIO-MOVTO-TARIF      PIC  X(010).
                10 ZY-CROTNA-ORIGE-MOVTO      PIC  X(004).
                10 ZY-CNRO-ARQ-MOVTO          PIC S9(002)        COMP-3.
                10 ZY-CSEQ-MOVTO              PIC S9(011)        COMP-3.
                10 ZY-CSERVC-TARIF            PIC S9(005)        COMP-3.
                10 ZY-CMIDIA-SERVC-MOVTO      PIC S9(005)        COMP-3.
                10 ZY-CPERIF-SERVC-MOVTO      PIC S9(005)        COMP-3.
                10 ZY-QEVNTO-AGRUP            PIC S9(006)        COMP-3.
                10 ZY-HPREST-SERVC-MOVTO      PIC  X(008).
                10 ZY-CTPO-CTA-MOVTO          PIC S9(001)        COMP-3.
                10 ZY-CBCO-DSTNO-MOVTO        PIC S9(003)        COMP-3.
                10 ZY-CAG-DSTNO-MOVTO         PIC S9(005)        COMP-3.
                10 ZY-CCTA-DSTNO-MOVTO        PIC S9(013)        COMP-3.
                10 ZY-CPAB-DSTNO-MOVTO        PIC S9(005)        COMP-3.
                10 ZY-CAG-CTLZA-DEB           PIC S9(005)        COMP-3.
                10 ZY-CCTA-CTLZA-DEB          PIC S9(013)        COMP-3.
                10 ZY-VTARIF-BRUTO-MOVTO      PIC S9(009)V9(2)   COMP-3.
                10 ZY-VTARIF-LIQ-MOVTO        PIC S9(009)V9(2)   COMP-3.
                10 ZY-VTARIF-DEB-MOVTO        PIC S9(009)V9(2)   COMP-3.
                10 ZY-DAGNDA-DEB-MOVTO        PIC  X(010).
                10 ZY-DEFETV-DEB-MOVTO        PIC  X(010).
                10 ZY-QMAX-TENTV-DEB          PIC S9(003)        COMP-3.
                10 ZY-QTENTV-DEB-MOVTO        PIC S9(003)        COMP-3.
                10 ZY-CSIT-EVNTO-RECBD        PIC S9(002)        COMP-3.
                10 ZY-CIDTFD-PGMC-COBR        PIC S9(002)        COMP-3.
                10 ZY-DOCOR-EVNTO             PIC  X(010).
                10 ZY-VTARIF-CADTR-SIST       PIC S9(009)V9(2)   COMP-3.
                10 ZY-CINDCD-TARIF-PCELD      PIC  X(001).
                10 ZY-CTRATO-MOVTO            PIC S9(002)        COMP-3.
                10 ZY-QTD-FQ-COMPON           PIC S9(005)        COMP-3.
                10 ZY-QTD-EXC-DIA             PIC S9(005)        COMP-3.
                10 ZY-QTD-EXC-ACU             PIC S9(005)        COMP-3.
                10 ZY-FLAG-DATA               PIC  X(001).
                10 ZY-AGENCIA-CREC            PIC S9(005)        COMP-3.
                10 ZY-CONTA-CREC              PIC S9(013)        COMP-3.
                10 ZY-PACOTE-EVENTO           PIC S9(005)        COMP-3.
                10 ZY-AGRUPAMENTOS.
                   15 ZY-CSGMTO-GSTAO-TARIF   PIC S9(005)        COMP-3.
                   15 ZY-CMUN-RURAL           PIC S9(009)        COMP-3.
                   15 ZY-CSGL-UF              PIC  X(002).
                   15 ZY-BCO-POSTAL           PIC S9(005)        COMP-3.
                   15 ZY-CPAB                 PIC S9(005)        COMP-3.
ST2507*            15 ZY-CPF-CNPJ             PIC S9(009)        COMP-3.
ST2507*            15 ZY-FILIAL               PIC S9(005)        COMP-3.
ST2507             15 ZY-CPF-CNPJ             PIC  X(009).
ST2507             15 ZY-FILIAL               PIC  X(004).
                   15 ZY-CONTROLE             PIC  X(002).
                10 ZY-PACOTE.
                   15 ZY-CSEC-COMP-PCT        PIC  9(003).
                   15 ZY-DINI-PER-COMP        PIC  9(008).
                   15 ZY-DFIM-PER-COMP        PIC  9(008).
                   15 ZY-PERIODO-PCT          PIC  9(006).
                   15 ZY-CPER-COBR-COMPO      PIC S9(001)V USAGE COMP-3.
                   15 ZY-VEXCED-CP-PCT    PIC S9(009)V9(2) USAGE COMP-3.
                   15 ZY-PEXCED-CP-PCT    PIC S9(003)V9(4) USAGE COMP-3.
                   15 ZY-QDIA-COBR-COMPO      PIC S9(002)V USAGE COMP-3.
                10 ZY-HIERARQUIA-REG          OCCURS 7 TIMES.
                   15 ZY-AGPT-REG             PIC  9(002).
ST2507*      05 ZY-FILLER-VA                  PIC  X(015).
ST2507       05 ZY-FILLER-VA                  PIC  X(010).
          03 ZY-REG-BONIFI.
             05 ZY-GFCTB023.
                10 ZY-CHAVE-B023.
                   15 ZY-23-CSERVC-TARIF      PIC S9(5)V   USAGE COMP-3.
                   15 ZY-23-CTPO-BONIF        PIC S9(3)V   USAGE COMP-3.
                   15 ZY-23-CCRITR-BONIF      PIC S9(3)V   USAGE COMP-3.
                   15 ZY-23-DINIC-REGRA-BONIF PIC X(10).
                   15 ZY-23-CSEQ-BONIF-SERVC  PIC S9(3)V   USAGE COMP-3.
                10 ZY-23-VCOBR-BONIF        PIC S9(9)V9(2) USAGE COMP-3.
                10 ZY-23-PDESC-BONIF        PIC S9(3)V9(4) USAGE COMP-3.
                10 ZY-23-QFRANQ-BONIF         PIC S9(3)V   USAGE COMP-3.
                10 ZY-23-CCART-ANLSE-GERC     PIC S9(5)V   USAGE COMP-3.
                10 ZY-23-CGRP-CART-SIST       PIC S9(5)V   USAGE COMP-3.
                10 ZY-23-CIDTFD-TPO-FAIXA     PIC S9(1)V   USAGE COMP-3.
             05 ZY-DADOS-SIGB.
                10 ZY-CONSE-DADO-CONS         PIC S9(015)        COMP-3.
                10 ZY-CONSE-DADO-CONS-R REDEFINES ZY-CONSE-DADO-CONS
                                              PIC  X(008).
             05 ZY-B043-RESTRICAO-S-N         PIC  X(001).
          03 ZY-PROGR-BENEF                   PIC  X(001).
      *       'S' - PARTICIPANTE P.B.
      *       ' ' - NAO PARTICIPANTE P.B.
      *
          03 FILLER                           PIC  X(022).

