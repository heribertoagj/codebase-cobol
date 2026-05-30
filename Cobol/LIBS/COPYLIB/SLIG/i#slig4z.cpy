      ***===========================================================***
      *** NOME INC                                     LENGTH=0034  ***
      *** I#SLIG4Z                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICAO DO MODULO PARA               ***
      ***            VERIFICAR PERFIL DO USUARIO                    ***
      ***===========================================================***
      *
       01 REG-RESTRICAO.
          03 SLIG4Z-DADOS-ENVIADOS.
             05 SLIG4Z-CGRP-ECONM                PIC  9(009).
             05 SLIG4Z-CGC-CPF-CLIENTE           PIC  X(009).
             05 SLIG4Z-FILIAL-CLIENTE            PIC  X(005).
             05 SLIG4Z-CONTROLE-CLIENTE          PIC  9(002).
             05 SLIG4Z-TP-PESS-CLIENTE           PIC  X(001).
          03 SLIG4Z-DADOS-RETORNADOS.
             05 SLIG4Z-MAIOR-GRAU-RESTRICAO      PIC  X(003).
             05 SLIG4Z-MAIOR-CLASSIF-RATING      PIC  X(003).
             05 SLIG4Z-COD-RETORNO               PIC  9(002).
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** CODIGOS RETORNO:                                          ***
      *** SLIG4Z-COD-RETORNO - MENSAGEM                             ***
      *** 00                 - PROCESSAMENTO OK                     ***
      *** 01                 - REGISTRO INEXISTENTE                 ***
      *** 02                 - REGISTRO INCONSISTENTE               ***
      *** 88                 - ERRO DB2                             ***
      ***                                                           ***
      ***-----------------------------------------------------------***

