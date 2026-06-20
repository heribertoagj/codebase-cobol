      *                                                                 00010000
      ***************************************************************** 00020000
      * NOME DA INC - GFCTWAOV                                        * 00030000
      * DESCRICAO   - IMAGEM DA TABELA GFCTB0N2-DB2PRD.TCTRL_NGTVC_CLI* 00040000
      *               TABELA DE NEGATIVAÇÕES DO IRES                  * 00050000
ST2507* TAMANHO     - 120                                             * 00060000
      * DATA        - DEZ/2010                                        * 00070000
      * RESPONSAVEL - PAGNOCCA - SONDA PROCWORK                       * 00080000
      *================================================================*00080100
ST2507*               U L T I M A   A L T E R A C A O                *  00080200
      *----------------------------------------------------------------*00080300
ST2507*    ANALISTA....:  STEFANINI                                    *00080400
ST2507*    DATA........:  07 / 2025                                    *00080500
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *00080600
      *================================================================*00080700
      *                                                                 00100000
       01  GFCTWAOV-REG.                                                00110000
           10 WAOV-CBCO-DEB-MOVTO       PIC S9(3)V COMP-3.              00120000
           10 WAOV-CAG-DEB-MOVTO        PIC S9(5)V COMP-3.              00130000
           10 WAOV-CCTA-DEB-MOVTO       PIC S9(13)V COMP-3.             00140000
           10 WAOV-DCONS-NGTVC-CLI      PIC X(10).                      00150000
ST2507*    10 WAOV-CCNPJ-CPF-DEB        PIC S9(9)V COMP-3.              00160000
ST2507*    10 WAOV-CFLIAL-CNPJ-DEB      PIC S9(5)V COMP-3.              00170000
ST2507     10 WAOV-CCNPJ-CPF-DEB        PIC  X(9).                      00170100
ST2507     10 WAOV-CFLIAL-CNPJ-DEB      PIC  X(4).                      00170200
           10 WAOV-CCTRL-CNPJ-CPF-DEB   PIC S9(2)V COMP-3.              00180000
           10 WAOV-CINDCD-RESUL-NGTVC   PIC X(1).                       00190000
           10 WAOV-RRESUL-CONS-NGTVC    PIC X(60).                      00200000
           10 WAOV-CROTNA-ORIGE-MOVTO   PIC X(4).                       00210000
           10 WAOV-DENVIO-MOVTO-TARIF   PIC X(10).                      00220000
           10 WAOV-CNRO-ARQ-MOVTO       PIC S9(2)V COMP-3.              00230000
           10 WAOV-CSEQ-MOVTO           PIC S9(11)V COMP-3.             00240000
      *                                                                 00250000
