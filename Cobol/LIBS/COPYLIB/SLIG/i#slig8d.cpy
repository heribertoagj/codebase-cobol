      ***===========================================================***
      *** NOME INC                                     LENGTH=00066 ***
      *** I#SLIG8D                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INDICES CALCULADOS TITULOS             ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG84                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  SLIG8D-REGISTRO.
      *-------- CHAVE COMPLETA TITULOS (SEM CARTEIRA)
         03 SLIG8D-CHAVE-COMPLETA.
      *-------- BANCO CLIENTE
           05 SLIG8D-CEMPR-INC                   PIC S9(005)   COMP-3.
      *-------- AGENCIA CLIENTE
           05 SLIG8D-CDEPDC                      PIC S9(005)   COMP-3.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG8D-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- J FILIAL CNPJ / F CPF CLIENTE
           05 SLIG8D-FILIAL-CCGC-CPF-CLI         PIC S9(05)    COMP-3.
      *-------- J CONTROLE CNPJ / F CPF CLIENTE
           05 SLIG8D-CONTROLE-CCGC-CPF-CLI       PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG8D-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- GRUPO ECONOMICO CLIENTE
           05 SLIG8D-CGRP-ECONM                  PIC S9(09)    COMP-3.
      *-------- J CNPJ / F CPF SACADO
           05 SLIG8D-CCGC-CPF-SACDO              PIC S9(009)   COMP-3.
      *-------- J FILIAL CNPJ / F CPF SACADO
           05 SLIG8D-FILIAL-CCGC-CPF-SACDO       PIC S9(005)   COMP-3.
      *-------- J CONTROLE CNPJ / F CPF SACADO
           05 SLIG8D-CONTROLE-CCGC-CPF-SACDO     PIC S9(002)   COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG8D-CINDCD-PSSOA-SACDO          PIC X(001).
      *
      *
      *-------- INDICES CALCULADOS
         03 SLIG8D-DADOS.
      *--------
           05 SLIG8D-MAIOR-IC                    PIC S9(03)V99 COMP-3.
           05 SLIG8D-MENOR-IL                    PIC S9(03)V99 COMP-3.
      *-------- INDICE LIQUIDEZ
           05 SLIG8D-IL-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG8D-IL-SIMPLES-NULL             PIC  X(01).
           05 SLIG8D-IL-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG8D-IL-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG8D-IL-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG8D-IL-DESCONTO-NULL            PIC  X(01).
      *-------- INDICE CONCENTRACAO PARTICIPACAO
           05 SLIG8D-IC-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG8D-IC-SIMPLES-NULL             PIC  X(01).
           05 SLIG8D-IC-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG8D-IC-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG8D-IC-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG8D-IC-DESCONTO-NULL            PIC  X(01).
      *--------
           05 SLIG8D-FILLER                      PIC  X(03).

      * === CAMPOS DUPLICADOS PARA STRING ===
         03 SLIG8D-DADOS-CCGC-ALPHA.
           05 SLIG8D-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8D-FILIAL-CCGC-CPF-CLI-ST      PIC X(005).
           05 SLIG8D-CTRL-CCGC-CPF-CLI-ST        PIC S9(02)    COMP-3.
           05 SLIG8D-CCGC-CPF-SACDO-ST           PIC X(009).
           05 SLIG8D-FLIAL-CCGC-CPF-SACDO-ST     PIC X(005).
           05 SLIG8D-CTRL-CCGC-CPF-SACDO-ST      PIC S9(002)   COMP-3.
