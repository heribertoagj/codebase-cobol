      ************************** PROCWORK ******************************
      *   I#GFCTVE - DADOS P/ COBRANCA APURACAO DE FRANQUIA E CARENCIA *
      *              - TABELAS: GFCTB092, 33 E A2                      *
      *                                                                *
      *   LERCL: 180                                       SET/2006    *
      ************************** PROCWORK ******************************
      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER                - PROCWORK - GRUPO 50 *
      *    DATA........:   12/02/2008                                  *
      *    OBJETIVO....:   INCLUIR LAYOUT COMPLETO DA I#GFCTVA NO CAMPO*
      *                    VE-GFCTB092                                 *
      *    LRECL.......:   300                                         *
      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *    ANALISTA....:   PAGNNOCA              - SONDA IT - GRUPO 50 *
      *    DATA........:   ABRIL/2013                                  *
      *    OBJETIVO....:   TAMANHO DO CAMPO QDIA-CAREN-TARIF DE 9(02)  *
      *     (JC0413)       PARA 9(03).                                 *
      *                    S9(2) COMP-3  / TAMANHO 02 BYTES            *
      *                    S9(3) COMP-3  / TAMANHO 02 BYTES            *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  REG-IGFCTVE.
        02 VE-GFCTB092.
           03 VE-DENVIO-MOVTO-TARIF      PIC  X(010).
           03 VE-CROTNA-ORIGE-MOVTO      PIC  X(004).
           03 VE-CNRO-ARQ-MOVTO          PIC S9(002)      COMP-3.
           03 VE-CSEQ-MOVTO              PIC S9(011)      COMP-3.
           03 VE-CSERVC-TARIF            PIC S9(005)      COMP-3.
           03 VE-CMIDIA-SERVC-MOVTO      PIC S9(005)      COMP-3.
           03 VE-CPERIF-SERVC-MOVTO      PIC S9(005)      COMP-3.
           03 VE-QEVNTO-AGRUP            PIC S9(006)      COMP-3.
           03 VE-HPREST-SERVC-MOVTO      PIC  X(008).
           03 VE-CTPO-CTA-MOVTO          PIC S9(001)      COMP-3.
           03 VE-CBCO-DSTNO-MOVTO        PIC S9(003)      COMP-3.
           03 VE-CAG-DSTNO-MOVTO         PIC S9(005)      COMP-3.
           03 VE-CCTA-DSTNO-MOVTO        PIC S9(013)      COMP-3.
           03 VE-CPAB-DSTNO-MOVTO        PIC S9(005)      COMP-3.
           03 VE-CAG-CTLZA-DEB           PIC S9(005)      COMP-3.
           03 VE-CCTA-CTLZA-DEB          PIC S9(013)      COMP-3.
           03 VE-VTARIF-BRUTO-MOVTO      PIC S9(009)V9(2) COMP-3.
           03 VE-VTARIF-LIQ-MOVTO        PIC S9(009)V9(2) COMP-3.
           03 VE-VTARIF-DEB-MOVTO        PIC S9(009)V9(2) COMP-3.
           03 VE-DAGNDA-DEB-MOVTO        PIC  X(010).
           03 VE-DEFETV-DEB-MOVTO        PIC  X(010).
           03 VE-QMAX-TENTV-DEB          PIC S9(003)      COMP-3.
           03 VE-QTENTV-DEB-MOVTO        PIC S9(003)      COMP-3.
           03 VE-CSIT-EVNTO-RECBD        PIC S9(002)      COMP-3.
           03 VE-CIDTFD-PGMC-COBR        PIC S9(002)      COMP-3.
           03 VE-DOCOR-EVNTO             PIC  X(010).
           03 VE-VTARIF-CADTR-SIST       PIC S9(009)V9(2) COMP-3.
           03 VE-CINDCD-TARIF-PCELD      PIC  X(001).
           03 VE-CTRATO-MOVTO            PIC S9(002)      COMP-3.
           03 VE-QTD-FQ-COMPON           PIC S9(005)      COMP-3.
           03 VE-QTD-EXC-DIA             PIC S9(005)      COMP-3.
           03 VE-QTD-EXC-ACU             PIC S9(005)      COMP-3.
           03 VE-FLAG-DATA               PIC  X(001).
           03 VE-AGENCIA-CREC            PIC S9(005)      COMP-3.
           03 VE-CONTA-CREC              PIC S9(013)      COMP-3.
           03 VE-PACOTE-EVENTO           PIC S9(005)      COMP-3.
           03 VE-AGRUPAMENTOS.
              05 VE-CSGMTO-GSTAO-TARIF   PIC S9(005)      COMP-3.
              05 VE-CMUN-RURAL           PIC S9(009)      COMP-3.
              05 VE-CSGL-UF              PIC  X(002).
              05 VE-BCO-POSTAL           PIC S9(005)      COMP-3.
              05 VE-CPAB                 PIC S9(005)      COMP-3.
              05 VE-CPF-CNPJ             PIC S9(009)      COMP-3.
              05 VE-FILIAL               PIC S9(005)      COMP-3.
              05 VE-CONTROLE             PIC  X(002).
           03 VE-PACOTE.
              05 VE-CSEC-COMP-PCT        PIC  9(003).
              05 VE-DINI-PER-COMP        PIC  9(008).
              05 VE-DFIM-PER-COMP        PIC  9(008).
              05 VE-PERIODO-PCT          PIC  9(006).
              05 VE-CPER-COBR-COMPO  PIC S9(001)V USAGE COMP-3.
              05 VE-VEXCED-CP-PCT    PIC S9(009)V9(2) USAGE COMP-3.
              05 VE-PEXCED-CP-PCT    PIC S9(003)V9(4) USAGE COMP-3.
              05 VE-QDIA-COBR-COMPO  PIC S9(002)V USAGE COMP-3.
           03 VE-HIERARQUIA-REG          OCCURS 7 TIMES.
              05 VE-AGPT-REG             PIC  9(002).
RIC        03 VE-DANO-REFT-EXTRT         PIC S9(4)V USAGE COMP-3.
           03 FILLER                     PIC  X(012).
        02 VE-GFCTB0A2.
           03 VE-QFRANQ-TARIF            PIC S9(3)V USAGE COMP-3.
           03 VE-CTPO-FRANQ-TARIF        PIC S9(1)V USAGE COMP-3.
           03 VE-CPER-FRANQ-TARIF        PIC S9(1)V USAGE COMP-3.
           03 VE-CPER-CALC-TARIF         PIC S9(1)V USAGE COMP-3.
           03 VE-CINIC-PER-CALC          PIC S9(1)V USAGE COMP-3.
           03 VE-DDIA-COBR-TARIF         PIC S9(2)V USAGE COMP-3.
JC0413*****03 VE-QDIA-CAREN-TARIF        PIC S9(2)V USAGE COMP-3.
JC0413     03 VE-QDIA-CAREN-TARIF        PIC S9(3)V USAGE COMP-3.
           03 VE-PDESC-CAREN-TARIF       PIC S9(3)V9(4) USAGE COMP-3.
           03 VE-CINDCD-INIC-CAREN       PIC S9(1)V USAGE COMP-3.
           03 VE-QDIA-RTCAO-COBR         PIC S9(2)V USAGE COMP-3.
           03 VE-DINIC-CAREN-TARIF       PIC X(10).
           03 VE-CSEQ-MANUT-SERVC        PIC S9(2)V USAGE COMP-3.
           03 VE-CINIC-PER-FRANQ         PIC S9(1)V USAGE COMP-3.
        02 FILLER                        PIC X(20).
ST2507  02 VE-CPF-CNPJ-ST                PIC  X(009).
ST2507  02 VE-FILIAL-ST                  PIC  X(004).
ST2507  02 VE-CONTROLE-ST                PIC  X(002).
