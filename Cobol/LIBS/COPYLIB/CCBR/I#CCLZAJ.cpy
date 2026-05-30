      ******************************************************************
      * NOME BOOK : I#CCLZAJ                                           *
      * OBJETIVO  : GERACAO DE INFORMACOES P/A RECEITA FEDERAL         *
      *             CADASTRO                                           *
      ******************************************************************
      * DATA      : 04/2016                                            *
      * AUTOR     : NEURACI - CAPGEMINI                                *
      * TAMANHO   : 61  BYTES                                          *
      ******************************************************************
       01  REG-CCLZAJ.
           05  CCLZAJ-CHAV.                                             00000140
               10  CCLZAJ-AGENCIA           PIC  9(5)     COMP-3.       00000150
               10  CCLZAJ-RAZAO.                                        00000160
                   15  CCLZAJ-GRUPO         PIC  9(3)     COMP-3.       00000170
                   15  CCLZAJ-SUBGP         PIC  9(3)     COMP-3.       00000180
               10  CCLZAJ-CONTA             PIC  9(7)     COMP-3.       00000190
           05  CCLZAJ-SALDO                 PIC S9(13)V99 COMP-3.       00000220
           05  CCLZAJ-GRUPO-CLIE            PIC  9(3)     COMP-3.       00000170
           05  CCLZAJ-SUBGP-CLIE            PIC  9(3)     COMP-3.       00000180
           05  CCLZAJ-CGC-CPF.                                          00000230
               10 CCLZAJ-PRINCIPAL          PIC  9(09)     COMP-3.      00000240
               10 CCLZAJ-FILIAL             PIC  9(05)     COMP-3.      00000250
               10 CCLZAJ-CTLE               PIC  9(02).                 00000260
           03  CCLZAJ-DT-ENCERRA            PIC  9(09)     COMP-3.      00000270
           03  CCLZAJ-NUMERO-TITULAR        PIC  9(05)     COMP-3.      00000270
           03  CCLZAJ-CPF-CNPJ-EFIR         PIC  9(14).                 00000270
           03  CCLZAJ-TIPO-PESSOA           PIC  9(01).
           03  CCLZAJ-APONT-ASSUM           PIC  X(03).
           03  CCLZAJ-TP-RELAC              PIC  9(01).
           03  FILLER                       PIC  X(01).                 00000270
