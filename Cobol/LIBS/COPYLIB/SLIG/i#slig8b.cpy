      ***===========================================================***
      *** NOME INC                                     LENGTH=00135 ***
      *** I#SLIG8B                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO GENERICO VENDOR          ***
      ***            INFORMACOES PARA CALCULO LIQUIDEZ PARTICIPACAO ***
      ***                                                           ***
BRQ250***           (COPIA DA I#SLIG82)                             ***
BRQ250***            INCLUIDO FILIAL E CONTROLE DO CNPJ             ***
BRQ250***            PARA CLIENTE E SACADO                          ***
      ***===========================================================***
      *
       01  SLIG8B-REGISTRO.
      *-------- CHAVE COMPLETA VENDOR
         03 SLIG8B-CHAVE-COMPLETA.
      *-------- BANCO CLIENTE
           05 SLIG8B-CEMPR-INC                   PIC S9(005)   COMP-3.
      *-------- AGENCIA CLIENTE
           05 SLIG8B-CDEPDC                      PIC S9(005)   COMP-3.
      *-------- CNPJ / CPF CHAVE
           05 SLIG8B-CCGC-CPF-CHV                PIC S9(09)    COMP-3.
      *-------- CNPJ / CPF FILIAL CHAVE
           05 SLIG8B-FILIAL-CHV                  PIC S9(05)    COMP-3.
      *-------- CNPJ / CPF CONTROLE CHAVE
           05 SLIG8B-CONTROLE-CHV                PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA CHAVE J OU F
           05 SLIG8B-CINDCD-PSSOA-CHV            PIC X(001).
      *-------- GRUPO ECONOMICO CLIENTE
           05 SLIG8B-CGRP-ECONM                  PIC S9(09)    COMP-3.
      *
         03 SLIG8B-DETALHE.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG8B-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- CNPJ / CPF FILIAL CLIENTE
           05 SLIG8B-FILIAL-CLI                  PIC S9(05)    COMP-3.
      *-------- CNPJ / CPF CONTROLE CLIENTE
           05 SLIG8B-CONTROLE-CLI                PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG8B-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- J CNPJ / F CPF SACADO
           05 SLIG8B-CCGC-CPF-SACDO              PIC S9(009)   COMP-3.
      *-------- CNPJ / CPF FILIAL SACADO
           05 SLIG8B-FILIAL-SACDO                PIC S9(05)    COMP-3.
      *-------- CNPJ / CPF CONTROLE SACADO
           05 SLIG8B-CONTROLE-SACDO              PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG8B-CINDCD-PSSOA-SACDO          PIC X(001).
      *-------- CARTEIRA 1 SIMPLES, 2 CAUCIONADA, 3 DESCONTO
           05 SLIG8B-IDTFD-CART-LIQDZ            PIC S9(002)   COMP-3.
      *
      *
      *-------- INFORMACOES DE VALORES PARA CALCULO
         03 SLIG8B-DADOS.
      *--------
           05 SLIG8B-VANTES-PER-NAO-PG           PIC S9(13)V99 COMP-3.
           05 SLIG8B-VVCTO-PER-PG                PIC S9(13)V99 COMP-3.
           05 SLIG8B-VVCTO-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG8B-VVCTO-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG8B-VAPOS-PER-PG                PIC S9(13)V99 COMP-3.
           05 SLIG8B-VAPOS-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG8B-VAPOS-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG8B-VPGTO-PER-CLI               PIC S9(13)V99 COMP-3.
           05 SLIG8B-VPGTO-PER-SACDO             PIC S9(13)V99 COMP-3.
           05 SLIG8B-VPGTO-ANTCP-CLI             PIC S9(13)V99 COMP-3.
           05 SLIG8B-VPGTO-ANTCP-SACDO           PIC S9(13)V99 COMP-3.
      *--------
           05 SLIG8B-FILLER                      PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
         03 SLIG8B-DADOS-CCGC-ALPHA.
           05 SLIG8B-CCGC-CPF-CHV-ST             PIC X(009).
           05 SLIG8B-FILIAL-CHV-ST               PIC X(005).
           05 SLIG8B-CONTROLE-CHV-ST             PIC S9(02)    COMP-3.
           05 SLIG8B-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8B-FILIAL-CLI-ST               PIC X(005).
           05 SLIG8B-CONTROLE-CLI-ST             PIC S9(02)    COMP-3.
           05 SLIG8B-CCGC-CPF-SACDO-ST           PIC X(009).
           05 SLIG8B-FILIAL-SACDO-ST             PIC X(005).
           05 SLIG8B-CONTROLE-SACDO-ST           PIC S9(02)    COMP-3.
