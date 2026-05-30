      *
      *****************************************************************
      * NOME DA INC - GFCTWBUC                                        *
      * DESCRICAO   - TARIFAS - OBTENCAO DO CLUB VIA CADU             *
      *               BASE NA BOOK PSDCW018 TP-REG = 29               *
      *                                                               *
ST2507* TAMANHO     - 0128                                            *
      * DATA        - MAI/2013                                        *
      * RESPONSAVEL - PAGNOCCA - SONDA IT                             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWBUC-REG.
             05  BUC-CPF-CNPJ.
ST2507*          07  BUC-CPF-CNPJ-NRO     PIC S9(009) COMP-3.
ST2507           07  BUC-CPF-CNPJ-NRO     PIC  X(009).
                 07  BUC-CPF-CNPJ-CTR     PIC S9(002) COMP-3.
ST2507*          07  BUC-CPF-CNPJ-FIL     PIC S9(004) COMP-3.
ST2507           07  BUC-CPF-CNPJ-FIL     PIC  X(004).
             05  BUC-CCLUB                PIC S9(010) COMP-3.
             05  BUC-TP-REG               PIC  X(003).
             05  BUC-CID-OPER.
                 10 BUC-BANCO             PIC  9(003).
                 10 BUC-AGEN              PIC  9(005).
                 10 BUC-CTA               PIC  9(007).
                 10 FILLER                PIC  9(011).
             05  BUC-CEMPR-INC            PIC S9(005) COMP-3.
             05  BUC-CDEPDC               PIC S9(005) COMP-3.
             05  BUC-CCTRO-CUSTO          PIC  X(004).
             05  BUC-CTPO-OBRIG           PIC S9(001) COMP-3.
             05  BUC-CPRODT-CREDT         PIC S9(003) COMP-3.
             05  BUC-CSPROD-CREDT         PIC S9(004) COMP-3.
             05  BUC-CSEQ-TPO-OBRIG       PIC S9(003) COMP-3.
             05  BUC-CPTCAO-ESPAC-TBELA   PIC S9(002) COMP-3.
             05  BUC-CSIT-OPER            PIC S9(001) COMP-3.
             05  BUC-CINDCD-TRAG-OPER     PIC S9(001) COMP-3.
             05  BUC-CCATEG-CTA           PIC S9(001) COMP-3.
             05  BUC-DCELEB-OPER          PIC  X(010).
             05  BUC-DFIM-OPER            PIC  X(010).
             05  BUC-DENCRR-OPER          PIC  X(010).
             05  BUC-CPOSTO-SERVC         PIC S9(005) COMP-3.
             05  BUC-CSGMTO-CTA           PIC S9(003) COMP-3.
             05  BUC-QPCELA-ORIGN         PIC S9(003) COMP-3.
             05  BUC-QPCELA-ABERT         PIC S9(003) COMP-3.
             05  BUC-QPCELA-ATRSO         PIC S9(003) COMP-3.
             05  BUC-QDIA-PCELA-ATRSO     PIC S9(004) COMP-3.
             05  BUC-CSEQ-ENDER-PSSOA     PIC S9(005) COMP-3.
             05  BUC-CDISTR-CORSP         PIC S9(002) COMP-3.
             05  BUC-CEMPR                PIC S9(010) COMP-3.
      *
      *****************************************************************
      * FIM DA INC - GFCTWBUC                                         *
      *****************************************************************
      *
