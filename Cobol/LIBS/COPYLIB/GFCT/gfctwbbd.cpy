      *                                                                 00010000
      ***************************************************************** 00020000
      * NOME DA INC - GFCTWBBD                                        * 00030000
      * DESCRICAO   - MOVIMENTO SIGB 18 POSICOES COMPLEMENTO          * 00040000
      *               COM CNPJ CLIENTE E AAAAMM DA APURACAO (GFCT)    * 00050000
ST2507* TAMANHO     - 39                                              * 00060000
      * DATA        - ABR/2014                                        * 00070000
      * RESPONSAVEL - PAGNOCCA - SONDA IT                             * 00080000
      *================================================================*00080100
ST2507*               U L T I M A   A L T E R A C A O                *  00080200
      *----------------------------------------------------------------*00080300
ST2507*    ANALISTA....:  STEFANINI                                    *00080400
ST2507*    DATA........:  07 / 2025                                    *00080500
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *00080600
      *================================================================*00080700
      *                                                                 00100000
       01  GFCTWBBD-REG.                                                00110000
           05  WBBD-AGEN                       PIC S9(005)V COMP-3.     00120000
           05  WBBD-CONTA                      PIC S9(007)V COMP-3.     00130000
           05  WBBD-CARTEIRA-SIGB              PIC S9(005)V COMP-3.     00140000
           05  WBBD-VALOR-TOTAL               PIC S9(013)V9(2) COMP-3.  00140100
           05  WBBD-VR-TOT-QTDE REDEFINES                               00140200
               WBBD-VALOR-TOTAL               PIC S9(015)V COMP-3.      00140300
           05  WBBD-CPF-CNPJ-CLIE.                                      00160000
ST2507*        10  WBBD-CPF-CNPJ-PRINC         PIC S9(009)V COMP-3.     00170000
ST2507*        10  WBBD-CPF-CNPJ-FILIAL        PIC S9(005)V COMP-3.     00180000
ST2507         10  WBBD-CPF-CNPJ-PRINC         PIC  X(009).             00180100
ST2507         10  WBBD-CPF-CNPJ-FILIAL        PIC  X(004).             00180200
               10  WBBD-CPF-CNPJ-CTR           PIC  9(002).             00190000
           05  WBBD-AAAA-MM-APURACAO           PIC  9(006).             00200000
      *                                                                 00210000
