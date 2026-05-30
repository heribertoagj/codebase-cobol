      ******************************************************************
      * NOME BOOK : I#CCLZAL                                           *
      * OBJETIVO  : GERACAO DE INFORMACOES P/A RECEITA FEDERAL         *
      *             ARQUIVO DE INCONSISTENCIAS                         *
      ******************************************************************
      * DATA      : 04/2016                                            *
      * AUTOR     : NEURACI - CAPGEMINI                                *
      * TAMANHO   : 30  BYTES                                          *
      ******************************************************************
       01  REG-CCLZAL.
           05  CCLZAL-CHAV.                                             00000140
               10  CCLZAL-AGENCIA           PIC  9(5)     COMP-3.       00000150
               10  CCLZAL-RAZAO.                                        00000160
                   15  CCLZAL-GRUPO         PIC  9(3)     COMP-3.       00000170
                   15  CCLZAL-SUBGP         PIC  9(3)     COMP-3.       00000180
               10  CCLZAL-CONTA             PIC  9(7)     COMP-3.       00000190
           05  CCLZAL-SALDO                 PIC S9(13)V99 COMP-3.       00000190
           05  CCLZAL-CGC-CPF.
               10 CCLZAL-PRINCIPAL          PIC  9(09)     COMP-3.
               10 CCLZAL-FILIAL             PIC  9(05)     COMP-3.
               10 CCLZAL-CTLE               PIC  9(02).
           05  CCLZAL-SITUACAO              PIC  X(1).                  00000220
