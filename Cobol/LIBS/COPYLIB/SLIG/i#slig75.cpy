      ***===========================================================***
      *** NOME INC                                     LENGTH=00100 ***
      *** I#SLIG75                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO CONCENTRACAO DIARIA      ***
      ***                                                           ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-INTERMEDIARIO-COCEN.
           05 SLIG75-CGC-CPF-CLI            PIC 9(009) COMP-3.
           05 SLIG75-FILIAL-CLI             PIC 9(004) COMP-3.
           05 SLIG75-CONTROLE-CLI           PIC 9(002) COMP-3.
           05 SLIG75-TIPO-PESSOA-CLI        PIC X(001).
           05 SLIG75-NOME-CLI               PIC X(040).
           05 SLIG75-CGC-CPF-SAC            PIC 9(009) COMP-3.
           05 SLIG75-FILIAL-SAC             PIC 9(004) COMP-3.
           05 SLIG75-CONTROLE-SAC           PIC 9(002) COMP-3.
           05 SLIG75-TIPO-PESSOA-SAC        PIC X(001).
           05 SLIG75-CD-CART-CLI            PIC 9(002)    COMP-3.
           05 SLIG75-VL-ABERTO-CLI          PIC 9(013)V99 COMP-3.
           05 SLIG75-VL-ABERTO-SAC-SIMP     PIC 9(013)V99 COMP-3.
           05 SLIG75-VL-ABERTO-SAC-CAUC     PIC 9(013)V99 COMP-3.
           05 SLIG75-VL-ABERTO-SAC-DESC     PIC 9(013)V99 COMP-3.
           05 SLIG75-CGC-CPF-CLI-ST         PIC X(009).
           05 SLIG75-FILIAL-CLI-ST          PIC X(004).
           05 SLIG75-CONTROLE-CLI-ST        PIC 9(002) COMP-3.
           05 SLIG75-CGC-CPF-SAC-ST         PIC X(009).
           05 SLIG75-FILIAL-SAC-ST          PIC X(004).
           05 SLIG75-CONTROLE-SAC-ST        PIC 9(002) COMP-3.
           05 SLIG75-FILLER                 PIC X(4).
