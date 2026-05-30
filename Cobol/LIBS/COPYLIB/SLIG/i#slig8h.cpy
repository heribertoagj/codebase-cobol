      ***===========================================================***
      *** NOME INC                                     LENGTH=00043 ***
      *** I#SLIG8H                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGEM DA TABELA                               ***
      ***            LIQUIDEZ-CLIENTE-BANDA-CHEQUE                  ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG88                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  SLIG8H-REGISTRO.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG8H-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- FILIAL CNPJ CLIENTE
           05 SLIG8H-CFLIAL-CLI                  PIC S9(05)    COMP-3.
      *-------- SEQUENCIA DA CLASSIFICACAO
           05 SLIG8H-CSEQ-CLASF-BANDA            PIC S9(15)    COMP-3.
      *-------- CONTROLE CNPJ/CPF CLIENTE
           05 SLIG8H-CCTRL-CLI                   PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG8H-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- BANCO SACADO
           05 SLIG8H-CBCO-SACDO                  PIC S9(003)   COMP-3.
      *-------- AGENCIA SACADO
           05 SLIG8H-CAGEN-SACDO                 PIC S9(005)   COMP-3.
      *-------- CONTA SACADO
           05 SLIG8H-CCONTA-SACDO                PIC S9(013)   COMP-3.
      *-------- INDICE LIQUIDEZ FINAL
           05 SLIG8H-ILF-SIMPLES                 PIC S9(03)V99 COMP-3.
           05 SLIG8H-ILF-SIMPLES-NULL            PIC  X(01).
           05 SLIG8H-ILF-CAUCIONADA              PIC S9(03)V99 COMP-3.
           05 SLIG8H-ILF-CAUCIONADA-NULL         PIC  X(01).
           05 SLIG8H-ILF-DESCONTO                PIC S9(03)V99 COMP-3.
           05 SLIG8H-ILF-DESCONTO-NULL           PIC  X(01).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG8H-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8H-CFLIAL-CLI-ST               PIC X(005).
           05 SLIG8H-CCTRL-CLI-ST                PIC S9(02)    COMP-3.
