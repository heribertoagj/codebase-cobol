      *================================================================*
      * NOME BOOK  : GFCTWF40 - ARQUIVO DE BONIFICACOES PARA O HSBC.   *
      * DATA       : DEZEMBRO/ 2015                                    *
      * AUTOR      : SONDA IT                                          *
      * SISTEMA    : GFCT -                        - TARIFAS           *
      *----------------------------------------------------------------*
      * OBJETIVO   : ARQUIVO CONTENDO AGENCIA, CONTA E TARIFAS A SEREM *
      *              BONIFICADAS PARA APURACAO DO HSBC COM VALORES.    *
      *----------------------------------------------------------------*
      * TAMANHO    : 100   BYTES                                       *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01 GFCTWF40-REGISTRO.
          05 GFCTWF40-AGENCIA         PIC  9(005).
          05 GFCTWF40-CONTA           PIC  9(013).
          05 GFCTWF40-TARIFA          PIC  9(005).
          05 GFCTWF40-DT-REFER        PIC  X(010).
          05 GFCTWF40-CGRP-CART       PIC  9(005).
          05 GFCTWF40-CARTEIRA        PIC  9(005).
          05 GFCTWF40-VLR             PIC  9(015).
          05 GFCTWF40-VLR-R REDEFINES GFCTWF40-VLR.
             10 GFCTWF40-VLR-DEC      PIC  9(13)V9(02).
ST2507*   05 GFCTWF40-CPF-CNPJ        PIC  9(016).
ST2507    05 GFCTWF40-CPF-CNPJ        PIC  X(015).
          05 GFCTWF40-CPF-CNPJ-R REDEFINES GFCTWF40-CPF-CNPJ.
ST2507*      10 GFCTWF40-PRINCIPAL    PIC  9(009).
ST2507*      10 GFCTWF40-FILIAL       PIC  9(005).
ST2507       10 GFCTWF40-PRINCIPAL    PIC  X(009).
ST2507       10 GFCTWF40-FILIAL       PIC  X(004).
             10 GFCTWF40-CONTROLE     PIC  9(002).
ST2507*   05 FILLER                   PIC  X(026).
ST2507    05 FILLER                   PIC  X(027).
      *
