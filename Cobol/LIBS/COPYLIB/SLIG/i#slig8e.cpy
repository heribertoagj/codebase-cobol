      ***===========================================================***
      *** NOME INC                                     LENGTH=00050 ***
      *** I#SLIG8E                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INDICES CALCULADOS VENDOR              ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG85                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  SLIG8E-REGISTRO.
      *-------- CHAVE COMPLETA VENDOR (SEM CARTEIRA)
         03 SLIG8E-CHAVE-COMPLETA.
      *-------- BANCO CLIENTE
           05 SLIG8E-CEMPR-INC                   PIC S9(005)   COMP-3.
      *-------- AGENCIA CLIENTE
           05 SLIG8E-CDEPDC                      PIC S9(005)   COMP-3.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG8E-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- J FILIAL CNPJ / F CPF CLIENTE
           05 SLIG8E-FILIAL-CCGC-CPF-CLI         PIC S9(05)    COMP-3.
      *-------- J CONTROLE CNPJ / F CPF CLIENTE
           05 SLIG8E-CONTROLE-CCGC-CPF-CLI       PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG8E-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- GRUPO ECONOMICO CLIENTE
           05 SLIG8E-CGRP-ECONM                  PIC S9(09)    COMP-3.
      *-------- J CNPJ / F CPF SACADO
           05 SLIG8E-CCGC-CPF-SACDO              PIC S9(009)   COMP-3.
      *-------- J FILIAL CNPJ / F CPF SACADO
           05 SLIG8E-FILIAL-CCGC-CPF-SACDO       PIC S9(005)   COMP-3.
      *-------- J CONTROLE CNPJ / F CPF SACADO
           05 SLIG8E-CONTROLE-CCGC-CPF-SACDO     PIC S9(002)   COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG8E-CINDCD-PSSOA-SACDO          PIC X(001).
      *
      *
      *-------- INDICES CALCULADOS
         03 SLIG8E-DADOS.
      *-------- INDICE LIQUIDEZ FORNECEDOR
           05 SLIG8E-ILF-SIMPLES                 PIC S9(03)V99 COMP-3.
           05 SLIG8E-ILF-SIMPLES-NULL            PIC  X(01).
      *-------- INDICE LIQUIDEZ COMPRADOR
           05 SLIG8E-ILC-SIMPLES                 PIC S9(03)V99 COMP-3.
           05 SLIG8E-ILC-SIMPLES-NULL            PIC  X(01).
      *-------- INDICE CONCENTRACAO PARTICIPACAO
           05 SLIG8E-IC-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG8E-IC-SIMPLES-NULL             PIC  X(01).
      *--------
           05 SLIG8E-FILLER                      PIC  X(05).

      * === CAMPOS DUPLICADOS PARA STRING ===
         03 SLIG8E-DADOS-CCGC-ALPHA.
           05 SLIG8E-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8E-FILIAL-CCGC-CPF-CLI-ST      PIC X(005).
           05 SLIG8E-CTRL-CCGC-CPF-CLI-ST    PIC S9(02)    COMP-3.
           05 SLIG8E-CCGC-CPF-SACDO-ST           PIC X(009).
           05 SLIG8E-FLIAL-CCGC-CPF-SACDO-ST    PIC X(005).
           05 SLIG8E-CTRL-CCGC-CPF-SACDO-ST  PIC S9(002)   COMP-3.
