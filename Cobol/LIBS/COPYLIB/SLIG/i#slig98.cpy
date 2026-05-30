
      ***===========================================================***
      *** NOME INC                                     LENGTH=00100 ***
      *** I#SLIG98                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - ID-SACDO-LIQDZ                ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-ID-SACADO.
           05 SLIG98-CGC-CPF-SACDO          PIC 9(009) COMP-3.
           05 SLIG98-FILIAL-SACDO           PIC 9(005) COMP-3.
           05 SLIG98-CONTRL-SACDO           PIC 9(002).
           05 SLIG98-COPLT-SACDO            PIC X(040).
           05 SLIG98-TIPO-PESSOA-EMITT      PIC X(001).
      *
      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG98-CGC-CPF-SACDO-ST       PIC X(009).
           05 SLIG98-FILIAL-SACDO-ST        PIC X(005).
           05 SLIG98-CONTRL-SACDO-ST        PIC 9(002).
           05 SLIG98-FILLER                 PIC X(033).
