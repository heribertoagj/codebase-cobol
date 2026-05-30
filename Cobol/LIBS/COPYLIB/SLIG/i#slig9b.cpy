      ***===========================================================***
      *** NOME INC                                     LENGTH=00036 ***
      *** I#SLIG9B                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGEM DAS TABELAS                             ***
      ***            PARTICIPACAO-GRUPO-EMITENTE-CHEQUE             ***
      ***            PARTICIPACAO-GRUPO-SACADO-TITULO               ***
      ***            PARTICIPACAO-GRUPO-COMPRADOR-VENDOR *          ***
      ***                         * RETIRAR CAUCIONADA NO SORT      ***
      ***                         * RETIRAR DESCONTO NO SORT TAMBEM ***
      ***           (COPIA DA I#SLIG92                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  SLIG9B-REGISTRO.
      *-------- GRUPO ECONOMICO CLIENTE
           05 SLIG9B-CGRP-ECONM                  PIC S9(09)    COMP-3.
      *-------- SEQUENCIA DA CLASSIFICACAO
           05 SLIG9B-CSEQ-CLASF-PRTCP            PIC S9(15)    COMP-3.
      *-------- J CNPJ / F CPF SACADO
           05 SLIG9B-CCGC-CPF-SACDO              PIC S9(009)   COMP-3.
      *-------- J FILIAL CNPJ / F CPF SACADO
           05 SLIG9B-FILIAL-CCGC-CPF-SACDO       PIC S9(005)   COMP-3.
      *-------- J CONTROLE CNPJ / F CPF SACADO
           05 SLIG9B-CONTROLE-CCGC-CPF-SACDO     PIC S9(002)   COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG9B-CINDCD-PSSOA-SACDO          PIC X(001).
      *-------- INDICE CONCENTRACAO PARTICIPACAO
           05 SLIG9B-IC-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG9B-IC-SIMPLES-NULL             PIC  X(01).
           05 SLIG9B-IC-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG9B-IC-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG9B-IC-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG9B-IC-DESCONTO-NULL            PIC  X(01).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG9B-CCGC-CPF-SACDO-ST           PIC X(009).
           05 SLIG9B-FLIAL-CCGC-CPF-SACDO-ST     PIC X(005).
           05 SLIG9B-CTRL-CCGC-CPF-SACDO-ST      PIC S9(002)   COMP-3.
