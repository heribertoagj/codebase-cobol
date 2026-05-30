
      ***===========================================================***
      *** NOME INC                                     LENGTH=00046 ***
      *** I#SLIG33                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - ID-SACDO-LIQDZ                ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-ID-SACDO-LIQDZ.
           05 SLIG33-CGC-CPF-SACDO          PIC S9(09) COMP-3.
           05 SLIG33-INDCD-PSSOA-SACDO      PIC X(001).
           05 SLIG33-COPLT-SACDO            PIC X(040).
           05 SLIG33-COPLT-SACDO-N          PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG33-CGC-CPF-SACDO-ST       PIC X(009).
