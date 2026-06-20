      ******************************************************************
      * NOME DA INC - I#GFCTWX                                         *
      * DESCRICAO   - DADOS P/ COBRANCA APURACAO DE FRANQUIA E         *
      *               CARENCIA - TABELAS: GFCTB092, 33 E A2 - COPIA    *
      *               DA I#GFCTVE.                                     *
      * TAMANHO     - 180                                              *
      * DATA        - 11/2007                                          *
      * RESPONSAVEL - VINICIUS C. MADUREIRA - PROCWORK - TEL 3068-9902 *
      ******************************************************************
0208  *================================================================*
0208  *                     A L T E R A C A O                          *
0208  *----------------------------------------------------------------*
0208  *    ANALISTA....:   WAGNER                - PROCWORK - GRUPO 50 *
0208  *    DATA........:   12/02/2008                                  *
0208  *    OBJETIVO....:   INCLUIR LAYOUT COMPLETO DA I#GFCTVE NO CAMPO*
0208  *                    WX-GFCTB092                                 *
0208  *    LRECL.......:   320                                         *
0208  *================================================================*
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
ST2507*    LRECL ......:  320                                          *
      *================================================================*
      *
       01  REG-IGFCTWX.
        02 WX-GFCTB092.
           03 WX-CROTNA-ORIGE-MOVTO      PIC  X(004).
           03 WX-CNRO-ARQ-MOVTO          PIC S9(002)      COMP-3.
           03 WX-CSEQ-MOVTO              PIC S9(011)      COMP-3.
           03 WX-CSERVC-TARIF            PIC S9(005)      COMP-3.
           03 WX-CMIDIA-SERVC-MOVTO      PIC S9(005)      COMP-3.
           03 WX-CPERIF-SERVC-MOVTO      PIC S9(005)      COMP-3.
           03 WX-QEVNTO-AGRUP            PIC S9(006)      COMP-3.
           03 WX-HPREST-SERVC-MOVTO      PIC  X(008).
           03 WX-CTPO-CTA-MOVTO          PIC S9(001)      COMP-3.
           03 WX-CBCO-DSTNO-MOVTO        PIC S9(003)      COMP-3.
           03 WX-CAG-DSTNO-MOVTO         PIC S9(005)      COMP-3.
           03 WX-CCTA-DSTNO-MOVTO        PIC S9(013)      COMP-3.
           03 WX-CPAB-DSTNO-MOVTO        PIC S9(005)      COMP-3.
           03 WX-CAG-CTLZA-DEB           PIC S9(005)      COMP-3.
           03 WX-CCTA-CTLZA-DEB          PIC S9(013)      COMP-3.
           03 WX-VTARIF-BRUTO-MOVTO      PIC S9(009)V9(2) COMP-3.
           03 WX-VTARIF-LIQ-MOVTO        PIC S9(009)V9(2) COMP-3.
           03 WX-VTARIF-DEB-MOVTO        PIC S9(009)V9(2) COMP-3.
           03 WX-DAGNDA-DEB-MOVTO        PIC  X(010).
           03 WX-QMAX-TENTV-DEB          PIC S9(003)      COMP-3.
           03 WX-QTENTV-DEB-MOVTO        PIC S9(003)      COMP-3.
           03 WX-CSIT-EVNTO-RECBD        PIC S9(002)      COMP-3.
           03 WX-CIDTFD-PGMC-COBR        PIC S9(002)      COMP-3.
           03 WX-DOCOR-EVNTO             PIC  X(010).
           03 WX-VTARIF-CADTR-SIST       PIC S9(009)V9(2) COMP-3.
           03 WX-CINDCD-TARIF-PCELD      PIC  X(001).
           03 WX-CTRATO-MOVTO            PIC S9(002)      COMP-3.
           03 WX-QTD-FQ-COMPON           PIC S9(005)      COMP-3.
           03 WX-QTD-EXC-DIA             PIC S9(005)      COMP-3.
           03 WX-QTD-EXC-ACU             PIC S9(005)      COMP-3.
           03 WX-FLAG-DATA               PIC  X(001).
           03 WX-AGENCIA-CREC            PIC S9(005)      COMP-3.
           03 WX-CONTA-CREC              PIC S9(013)      COMP-3.
           03 WX-PACOTE-EVENTO           PIC S9(005)      COMP-3.
           03 WX-AGRUPAMENTOS.
              05 WX-CSGMTO-GSTAO-TARIF   PIC S9(005)      COMP-3.
              05 WX-CMUN-RURAL           PIC S9(009)      COMP-3.
              05 WX-CSGL-UF              PIC  X(002).
              05 WX-CPAB                 PIC S9(005)      COMP-3.
              05 WX-CPF-CNPJ             PIC S9(009)      COMP-3.
              05 WX-FILIAL               PIC S9(005)      COMP-3.
              05 WX-CONTROLE             PIC  X(002).
           03 WX-PACOTE.
              05 WX-CSEC-COMP-PCT        PIC  9(003).
              05 WX-DINI-PER-COMP        PIC  9(008).
              05 WX-DFIM-PER-COMP        PIC  9(008).
              05 WX-PERIODO-PCT          PIC  9(006).
              05 WX-CPER-COBR-COMPO  PIC S9(001)V USAGE COMP-3.
              05 WX-VEXCED-CP-PCT    PIC S9(009)V9(2) USAGE COMP-3.
              05 WX-PEXCED-CP-PCT    PIC S9(003)V9(4) USAGE COMP-3.
              05 WX-QDIA-COBR-COMPO  PIC S9(002)V USAGE COMP-3.
           03 WX-HIERARQUIA-REG          OCCURS 7 TIMES.
              05 WX-AGPT-REG             PIC  9(002).
           03 FILLER                     PIC  X(015).
        02 WX-GFCTB0A2.
           03 WX-QFRANQ-TARIF            PIC S9(3)V USAGE COMP-3.
           03 WX-CTPO-FRANQ-TARIF        PIC S9(1)V USAGE COMP-3.
           03 WX-CPER-CALC-TARIF         PIC S9(1)V USAGE COMP-3.
           03 WX-CINIC-PER-CALC          PIC S9(1)V USAGE COMP-3.
           03 WX-DDIA-COBR-TARIF         PIC S9(2)V USAGE COMP-3.
JC0413*****03 WX-QDIA-CAREN-TARIF        PIC S9(2)V USAGE COMP-3.
JC0413     03 WX-QDIA-CAREN-TARIF        PIC S9(3)V USAGE COMP-3.
           03 WX-PDESC-CAREN-TARIF       PIC S9(3)V9(4) USAGE COMP-3.
           03 WX-CINDCD-INIC-CAREN       PIC S9(1)V USAGE COMP-3.
           03 WX-QDIA-RTCAO-COBR         PIC S9(2)V USAGE COMP-3.
           03 WX-DINIC-CAREN-TARIF       PIC X(10).
           03 WX-CSEQ-MANUT-SERVC        PIC S9(2)V USAGE COMP-3.
           03 WX-CINIC-PER-FRANQ         PIC S9(1)V USAGE COMP-3.
        02 FILLER                        PIC X(20).
        02 WX-CARENCIA.
           03 WX-DATA-LGDO               PIC X(10).
           03 WX-IND-PER-CAR             PIC 9(01) USAGE COMP-3.
           03 WX-QTDE-MES-CAR            PIC 9(02) USAGE COMP-3.
        02 FILLER                        PIC X(07).
ST2507  02 WX-CPF-CNPJ-S                 PIC  X(009).
ST2507  02 WX-FILIAL-S                   PIC  X(004).
