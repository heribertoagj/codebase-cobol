      ***===========================================================***
      *** NOME INC                                     LENGTH=00034 ***
      *** I#SLIG9C                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGEM DAS TABELAS                             ***
      ***            PARTICIPACAO-BANCO-EMITENTE-CHEQUE             ***
      ***            PARTICIPACAO-BANCO-SACADO-TITULO               ***
      ***           (COPIA DA I#SLIG93                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  SLIG9C-REGISTRO.
      *-------- BANCO CLIENTE
           05 SLIG9C-CEMPR-INC                   PIC S9(005)   COMP-3.
      *-------- SEQUENCIA DA CLASSIFICACAO
           05 SLIG9C-CSEQ-CLASF-PRTCP            PIC S9(015)   COMP-3.
      *-------- J CNPJ / F CPF SACADO
           05 SLIG9C-CCGC-CPF-SACDO              PIC S9(009)   COMP-3.
      *-------- J FILIAL CNPJ / F CPF SACADO
           05 SLIG9C-FILIAL-CCGC-CPF-SACDO       PIC S9(005)   COMP-3.
      *-------- J CONTROLE CNPJ / F CPF SACADO
           05 SLIG9C-CONTROLE-CCGC-CPF-SACDO     PIC S9(002)   COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG9C-CINDCD-PSSOA-SACDO          PIC X(001).
      *-------- INDICE CONCENTRACAO PARTICIPACAO
           05 SLIG9C-IC-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG9C-IC-SIMPLES-NULL             PIC  X(01).
           05 SLIG9C-IC-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG9C-IC-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG9C-IC-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG9C-IC-DESCONTO-NULL            PIC  X(01).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG9C-CCGC-CPF-SACDO-ST           PIC X(009).
           05 SLIG9C-FLIAL-CCGC-CPF-SACDO-ST     PIC X(005).
           05 SLIG9C-CTRL-CCGC-CPF-SACDO-ST      PIC S9(002)   COMP-3.
