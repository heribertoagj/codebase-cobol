      ***===========================================================***
      *** NOME INC                                     LENGTH=0042  ***
      *** I#SLIGH3                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CONCENTRACAO SACADO                    ***
      ***            VALORES E PERCENTUAIS DE CONCENTRACAO EM       ***
      ***            RELACAO AO BANCO                               ***
      ***===========================================================***
      *
       01  REG-CONSSACS.
           03 SLIGH3-CGC-CPF-SAC           PIC 9(009)    COMP-3.
           03 SLIGH3-TIPO-PS-SAC           PIC X(001).
           03 SLIGH3-PCOCEN-MAIOR          PIC 9(003)V99 COMP-3.
           03 SLIGH3-PCOCEN-SIMPLE         PIC 9(003)V99 COMP-3.
           03 SLIGH3-PCOCEN-CAUCN          PIC 9(003)V99 COMP-3.
           03 SLIGH3-PCOCEN-DESC           PIC 9(003)V99 COMP-3.
           03 SLIGH3-VABERTO-SIMP          PIC 9(013)V99 COMP-3.
           03 SLIGH3-VABERTO-CAUC          PIC 9(013)V99 COMP-3.
           03 SLIGH3-VABERTO-DESC          PIC 9(013)V99 COMP-3.
           03 SLIGH3-CGC-CPF-SAC-ST        PIC X(009).
      *
      ***===========================================================***
      *
