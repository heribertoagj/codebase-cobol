      *
      *****************************************************************
      * NOME DA INC - GFCTWBUB                                        *
      * DESCRICAO   - TARIFAS - INFORMACOES DE CONTAS SALARIO         *
      *               IMAGEM DA TABELA CSALB002                       *
ST2507* TAMANHO     - 0067                                            *
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
       01  GFCTWBUB-REG.
           10 BUB-CBCO-CTA-SALRL       PIC S9(3)V USAGE COMP-3.
           10 BUB-CAG-CTA-SALRL        PIC S9(5)V USAGE COMP-3.
           10 BUB-CCTA-SALRL           PIC S9(13)V USAGE COMP-3.
           10 BUB-CDIG-CTA-SALRL       PIC X(2).
ST2507*    10 BUB-CCNPJ-EMPR-CONVN     PIC S9(9)V USAGE COMP-3.
ST2507*    10 BUB-CFLIAL-EMPR-CONVN    PIC S9(5)V USAGE COMP-3.
ST2507     10 BUB-CCNPJ-EMPR-CONVN     PIC  X(9).
ST2507     10 BUB-CFLIAL-EMPR-CONVN    PIC  X(4).
           10 BUB-CBCO-EMPR-CONVN      PIC S9(3)V USAGE COMP-3.
           10 BUB-CAG-EMPR-CONVN       PIC S9(5)V USAGE COMP-3.
           10 BUB-CCTA-EMPR-CONVN      PIC S9(13)V USAGE COMP-3.
           10 BUB-CCONVE-CTA-SALRL     PIC S9(9)V USAGE COMP-3.
           10 BUB-CBCO-DSTNO-SALRL     PIC S9(3)V USAGE COMP-3.
           10 BUB-CAG-DSTNO-SALRL      PIC S9(5)V USAGE COMP-3.
           10 BUB-CCTA-DSTNO-SALRL     PIC S9(13)V USAGE COMP-3.
           10 BUB-CDIG-CTA-DSTNO       PIC X(2).
           10 BUB-CSIT-CTA-SALRL       PIC S9(2)V USAGE COMP-3.
           10 BUB-CSIT-CTA-DSTNO       PIC S9(2)V USAGE COMP-3.
           10 BUB-CTPO-CTA-DSTNO       PIC X(2).
           10 BUB-CIDTFD-VNCLO-EMPTM   PIC X(1).
           10 BUB-CIDTFD-CAD-COPLT     PIC X(1).
           10 BUB-CINDCD-TRNSF-CREDT   PIC X(1).
      *
      *****************************************************************
      * FIM DA INC - GFCTWBUB                                         *
      *****************************************************************
      *

