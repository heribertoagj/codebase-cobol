      *****************************************************************
      * 03.04.2014             S O N D A   I T                 CIBELE *
      *****************************************************************
      * NOME DA INC - GFCTWBBE (COPIA DA I#GFCTZY)                    *
      * DESCRICAO   - BONIFICACAO DESCONTO PROGRESSIVO INDIVIDUAL     *
      *               . WBBE-REG-GFCTVA =REGISTRO COMPLETO DE ENTRADA *
      *               . WBBE-REG-BONIFI =DADOS P/ TRATAR BONIFICACAO  *
      * TAMANHO     - 320                                             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01 GFCTWBBE-REG.
          03 WBBE-REG-GFCTVA.
             05 WBBE-GFCTB092.
                10 WBBE-DENVIO-MOVTO-TARIF    PIC  X(010).
                10 WBBE-CROTNA-ORIGE-MOVTO    PIC  X(004).
                10 WBBE-CNRO-ARQ-MOVTO        PIC S9(002)        COMP-3.
                10 WBBE-CSEQ-MOVTO            PIC S9(011)        COMP-3.
                10 WBBE-CSERVC-TARIF          PIC S9(005)        COMP-3.
                10 WBBE-CMIDIA-SERVC-MOVTO    PIC S9(005)        COMP-3.
                10 WBBE-CPERIF-SERVC-MOVTO    PIC S9(005)        COMP-3.
                10 WBBE-QEVNTO-AGRUP          PIC S9(006)        COMP-3.
                10 WBBE-HPREST-SERVC-MOVTO    PIC  X(008).
                10 WBBE-CTPO-CTA-MOVTO        PIC S9(001)        COMP-3.
                10 WBBE-CBCO-DSTNO-MOVTO      PIC S9(003)        COMP-3.
                10 WBBE-CAG-DSTNO-MOVTO       PIC S9(005)        COMP-3.
                10 WBBE-CCTA-DSTNO-MOVTO      PIC S9(013)        COMP-3.
                10 WBBE-CPAB-DSTNO-MOVTO      PIC S9(005)        COMP-3.
                10 WBBE-CAG-CTLZA-DEB         PIC S9(005)        COMP-3.
                10 WBBE-CCTA-CTLZA-DEB        PIC S9(013)        COMP-3.
                10 WBBE-VTARIF-BRUTO-MOVTO    PIC S9(009)V9(2)   COMP-3.
                10 WBBE-VTARIF-LIQ-MOVTO      PIC S9(009)V9(2)   COMP-3.
                10 WBBE-VTARIF-DEB-MOVTO      PIC S9(009)V9(2)   COMP-3.
                10 WBBE-DAGNDA-DEB-MOVTO      PIC  X(010).
                10 WBBE-DEFETV-DEB-MOVTO      PIC  X(010).
                10 WBBE-QMAX-TENTV-DEB        PIC S9(003)        COMP-3.
                10 WBBE-QTENTV-DEB-MOVTO      PIC S9(003)        COMP-3.
                10 WBBE-CSIT-EVNTO-RECBD      PIC S9(002)        COMP-3.
                10 WBBE-CIDTFD-PGMC-COBR      PIC S9(002)        COMP-3.
                10 WBBE-DOCOR-EVNTO           PIC  X(010).
                10 WBBE-VTARIF-CADTR-SIST     PIC S9(009)V9(2)   COMP-3.
                10 WBBE-CINDCD-TARIF-PCELD    PIC  X(001).
                10 WBBE-CTRATO-MOVTO          PIC S9(002)        COMP-3.
                10 WBBE-QTD-FQ-COMPON         PIC S9(005)        COMP-3.
                10 WBBE-QTD-EXC-DIA           PIC S9(005)        COMP-3.
                10 WBBE-QTD-EXC-ACU           PIC S9(005)        COMP-3.
                10 WBBE-FLAG-DATA             PIC  X(001).
                10 WBBE-AGENCIA-CREC          PIC S9(005)        COMP-3.
                10 WBBE-CONTA-CREC            PIC S9(013)        COMP-3.
                10 WBBE-PACOTE-EVENTO         PIC S9(005)        COMP-3.
                10 WBBE-AGRUPAMENTOS.
                   15 WBBE-CSGMTO-GSTAO-TARIF PIC S9(005)        COMP-3.
                   15 WBBE-CMUN-RURAL         PIC S9(009)        COMP-3.
                   15 WBBE-CSGL-UF            PIC  X(002).
                   15 WBBE-BCO-POSTAL         PIC S9(005)        COMP-3.
                   15 WBBE-CPAB               PIC S9(005)        COMP-3.
                   15 WBBE-CPF-CNPJ           PIC S9(009)        COMP-3.
                   15 WBBE-FILIAL             PIC S9(005)        COMP-3.
                   15 WBBE-CONTROLE           PIC  9(002).
                10 WBBE-PACOTE.
                   15 WBBE-CSEC-COMP-PCT      PIC  9(003).
                   15 WBBE-DINI-PER-COMP      PIC  9(008).
                   15 WBBE-DFIM-PER-COMP      PIC  9(008).
                   15 WBBE-PERIODO-PCT        PIC  9(006).
                   15 WBBE-CPER-COBR-COMPO    PIC S9(001)V USAGE COMP-3.
                   15 WBBE-VEXCED-CP-PCT  PIC S9(009)V9(2) USAGE COMP-3.
                   15 WBBE-PEXCED-CP-PCT  PIC S9(003)V9(4) USAGE COMP-3.
                   15 WBBE-QDIA-COBR-COMPO    PIC S9(002)V USAGE COMP-3.
                10 WBBE-HIERARQUIA-REG        OCCURS 7 TIMES.
                   15 WBBE-AGPT-REG           PIC  9(002).
             05 WBBE-FILLER-VA                PIC  X(015).
          03 WBBE-REG-BONIFI.
             05 WBBE-GFCTB023.
                10 WBBE-CHAVE-B023.
                   15 WBBE-23-CSERVC-TARIF    PIC S9(5)V   USAGE COMP-3.
                   15 WBBE-23-CTPO-BONIF      PIC S9(3)V   USAGE COMP-3.
                   15 WBBE-23-CCRITR-BONIF    PIC S9(3)V   USAGE COMP-3.
                   15 WBBE-23-DINIC-REGRA-BONIF PIC X(10).
                   15 WBBE-23-CSEQ-BONIF-SERVC PIC S9(3)V  USAGE COMP-3.
                10 WBBE-23-VCOBR-BONIF      PIC S9(9)V9(2) USAGE COMP-3.
                10 WBBE-23-PDESC-BONIF      PIC S9(3)V9(4) USAGE COMP-3.
                10 WBBE-23-QFRANQ-BONIF       PIC S9(3)V   USAGE COMP-3.
                10 WBBE-23-CCART-ANLSE-GERC   PIC S9(5)V   USAGE COMP-3.
                10 WBBE-23-CGRP-CART-SIST     PIC S9(5)V   USAGE COMP-3.
                10 WBBE-23-CIDTFD-TPO-FAIXA   PIC S9(1)V   USAGE COMP-3.
             05 WBBE-DADOS-SIGB.
                10 WBBE-CONSE-DADO-CONS       PIC S9(015)       COMP-3.
                10 WBBE-CONSE-DADO-CONS-R REDEFINES WBBE-CONSE-DADO-CONS
                                              PIC  X(008).
             05 WBBE-B043-RESTRICAO-S-N       PIC  X(001).
          03 WBBE-DATA-ABERTURA               PIC  9(009)        COMP-3.
          03 FILLER                           PIC  X(018).
ST2507    03 WBBE-CPF-CNPJ-S                  PIC  X(009).
ST2507    03 WBBE-FILIAL-S                    PIC  X(004).
ST2507    03 WBBE-CONTROLE-S                  PIC  9(002).
