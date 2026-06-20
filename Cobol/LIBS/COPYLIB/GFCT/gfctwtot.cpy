      *****************************************************************
      * NOME DA INC - GFCTWTOT                                        *
      * DESCRICAO   - COBRANCA - PARCELAMENTO DE TARIFAS              *
      *               IMAGEM DA BOOK DE COBRANCAS I#GFCTVA            *
      *               ACRESCENTADAS AS INFORMACOES DE PARCELAMENTO    *
      *               DA TARIFA                                       *
      * REGISTRO TABELA DE EVENTOS - GFCTB092 -          ROTINA BATCH *
      * TAMANHO     - 265                                             *
      * DATA        - 29.11.2010                                      *
      * RESPONSAVEL - PAGNOCCA - SONDA PROCWORK                       *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWTOT-REG.
           02  TOT-GFCTVA.
               03 TOT-DENVIO-MOVTO-TARIF       PIC  X(010).
               03 TOT-CROTNA-ORIGE-MOVTO       PIC  X(004).
               03 TOT-CNRO-ARQ-MOVTO           PIC S9(002)      COMP-3.
               03 TOT-CSEQ-MOVTO               PIC S9(011)      COMP-3.
               03 TOT-CSERVC-TARIF             PIC S9(005)      COMP-3.
               03 TOT-CMIDIA-SERVC-MOVTO       PIC S9(005)      COMP-3.
               03 TOT-CPERIF-SERVC-MOVTO       PIC S9(005)      COMP-3.
               03 TOT-QEVNTO-AGRUP             PIC S9(006)      COMP-3.
               03 TOT-HPREST-SERVC-MOVTO       PIC  X(008).
               03 TOT-CTPO-CTA-MOVTO           PIC S9(001)      COMP-3.
               03 TOT-CBCO-DSTNO-MOVTO         PIC S9(003)      COMP-3.
               03 TOT-CAG-DSTNO-MOVTO          PIC S9(005)      COMP-3.
               03 TOT-CCTA-DSTNO-MOVTO         PIC S9(013)      COMP-3.
               03 TOT-CPAB-DSTNO-MOVTO         PIC S9(005)      COMP-3.
               03 TOT-CAG-CTLZA-DEB            PIC S9(005)      COMP-3.
               03 TOT-CCTA-CTLZA-DEB           PIC S9(013)      COMP-3.
               03 TOT-VTARIF-BRUTO-MOVTO       PIC S9(009)V9(2) COMP-3.
               03 TOT-VTARIF-LIQ-MOVTO         PIC S9(009)V9(2) COMP-3.
               03 TOT-VTARIF-DEB-MOVTO         PIC S9(009)V9(2) COMP-3.
               03 TOT-DAGNDA-DEB-MOVTO         PIC  X(010).
               03 TOT-DEFETV-DEB-MOVTO         PIC  X(010).
               03 TOT-QMAX-TENTV-DEB           PIC S9(003)      COMP-3.
               03 TOT-QTENTV-DEB-MOVTO         PIC S9(003)      COMP-3.
               03 TOT-CSIT-EVNTO-RECBD         PIC S9(002)      COMP-3.
               03 TOT-CIDTFD-PGMC-COBR         PIC S9(002)      COMP-3.
               03 TOT-DOCOR-EVNTO              PIC  X(010).
               03 TOT-VTARIF-CADTR-SIST        PIC S9(009)V9(2) COMP-3.
               03 TOT-CINDCD-TARIF-PCELD       PIC  X(001).
               03 TOT-CTRATO-MOVTO             PIC S9(002)      COMP-3.
               03 TOT-QTD-FQ-COMPON            PIC S9(005)      COMP-3.
               03 TOT-QTD-EXC-DIA              PIC S9(005)      COMP-3.
               03 TOT-QTD-EXC-ACU              PIC S9(005)      COMP-3.
               03 TOT-FLAG-DATA                PIC  X(001).
               03 TOT-AGENCIA-CREC             PIC S9(005)      COMP-3.
               03 TOT-CONTA-CREC               PIC S9(013)      COMP-3.
               03 TOT-PACOTE-EVENTO            PIC S9(005)      COMP-3.
               03 TOT-AGRUPAMENTOS.
                  05 TOT-CSGMTO-GSTAO-TARIF
                                               PIC S9(005)      COMP-3.
                  05 TOT-CMUN-RURAL            PIC S9(009)      COMP-3.
                  05 TOT-CSGL-UF               PIC  X(002).
                  05 TOT-BCO-POSTAL            PIC S9(005)      COMP-3.
                  05 TOT-CPAB                  PIC S9(005)      COMP-3.
ST2507*           05 TOT-CPF-CNPJ              PIC S9(009)      COMP-3.
ST2507*           05 TOT-FILIAL                PIC S9(005)      COMP-3.
ST2507            05 TOT-CPF-CNPJ              PIC  X(009).
ST2507            05 TOT-FILIAL                PIC  X(004).
                  05 TOT-CONTROLE              PIC  X(002).
               03 TOT-PACOTE.
                  05 TOT-CSEC-COMP-PCT         PIC  9(003).
                  05 TOT-DINI-PER-COMP         PIC  9(008).
                  05 TOT-DFIM-PER-COMP         PIC  9(008).
                  05 TOT-PERIODO-PCT           PIC  9(006).
                  05 TOT-CPER-COBR-COMPO       PIC S9(001)V     COMP-3.
                  05 TOT-VEXCED-CP-PCT         PIC S9(009)V9(2) COMP-3.
                  05 TOT-PEXCED-CP-PCT         PIC S9(003)V9(4) COMP-3.
                  05 TOT-QDIA-COBR-COMPO       PIC S9(002)V     COMP-3.
               03 TOT-HIERARQUIA-REG               OCCURS 7 TIMES.
                  05 TOT-AGPT-REG              PIC  9(002).
               03 TOT-MAIS-PACOTE.
                  05 TOT-DDIA-COBR-TARIF        PIC S9(002)V    COMP-3.
                  05 TOT-CIDTFD-COBR-FERI       PIC  X(001).
                  05 TOT-CIDTFD-VLR-EXCED       PIC S9(001)V    COMP-3.
                  05 TOT-REG-EXCEDENTE          PIC  X(001).
               03 TOT-AGRUP-FUNC-PERSO          PIC  X(001).
               03 TOT-HIERARQUIA-REG-CMPL          OCCURS 2 TIMES.
                  05 TOT-AGPT-CMPL              PIC  9(2).
               03 TOT-AGRUPAMENTO-RAZAO.
                  05 TOT-GRUPO                  PIC  9(03)      COMP-3.
                  05 TOT-SUB-GRUPO              PIC  9(03)      COMP-3.
               03 FILLER                        PIC  X(01).
            02  TOT-DADOS-PARCELAS.
               03 TOT-QPCELA-COBR-TARIF         PIC S9(03)      COMP-3.
               03 TOT-CIDTFD-COBR-PCELA         PIC  X(01).
               03 TOT-QDIA-COBR-PCELA           PIC S9(03)      COMP-3.
ST2507*     02  FILLER                          PIC  X(10).
ST2507      02  FILLER                          PIC  X(05).
